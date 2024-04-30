(in-package #:tls-server/async/tls)

;;;; temporary
#+sbcl
(eval-when (:compile-toplevel :load-toplevel)
  (require 'sb-cover))

#+sbcl
(eval-when (:compile-toplevel :load-toplevel)
  (declaim (optimize sb-cover:store-coverage-data debug safety)))

;;;;

(define-foreign-library openssl (:unix "libssl.so"))
(use-foreign-library openssl)

(defcfun "BIO_new" :pointer (bio-method :pointer))
(defcfun ("BIO_read" bio-read%) :int (bio-method :pointer) (data :pointer) (dlen :int))
(defcfun "BIO_s_mem" :pointer)
(defcfun "BIO_test_flags" :int (bio :pointer) (what :int))
(defcfun "BIO_write" :int (bio-method :pointer) (data :pointer) (dlen :int))

(defcfun "ERR_reason_error_string" :pointer (e :int))
(defcfun "ERR_get_error" :int)


(defcfun "SSL_CTX_check_private_key" :int (ctx :pointer))
(defcfun "SSL_CTX_ctrl" :int (ctx :pointer) (cmd :int) (value :long) (args :pointer))
(defcfun "SSL_CTX_free" :int (ctx :pointer))
(defcfun "SSL_CTX_new" :pointer (method :pointer))
(defcfun "SSL_CTX_set_options" :int (ctx :pointer) (options :uint))
(defcfun "SSL_CTX_use_PrivateKey_file" :int (ctx :pointer) (path :string) (type :int))
(defcfun "SSL_CTX_use_certificate_file" :int (ctx :pointer) (path :string) (type :int))
(defcfun "SSL_accept" :int (ssl :pointer))
(defcfun "SSL_get_error" :int (ssl :pointer) (ret :int))
(defcfun "SSL_free" :int (ssl :pointer))
(defcfun "SSL_is_init_finished" :int (ssl :pointer))
(defcfun "SSL_new" :pointer (bio-method :pointer))
(defcfun ("SSL_read" ssl-read%) :int (ssl :pointer) (buffer :pointer) (bufsize :int))
(defcfun "SSL_set_accept_state" :pointer (ssl :pointer))
(defcfun "SSL_set_bio" :void (ssl :pointer) (rbio :pointer) (wbio :pointer))
(defcfun "SSL_write" :int (ssl :pointer) (buffer :pointer) (bufsize :int))
(defcfun "TLS_method" :pointer)

(defcfun ("__errno_location" errno%) :pointer)
(defcfun ("strerror_r" strerror-r%) :pointer (errnum :int) (buffer :pointer) (buflen :int))

(defcfun "poll" :int (fdset :pointer) (rb :int) (timeout :int))
(defcfun ("close" close-fd) :int (fd :int))
(defcfun ("read" read-2) :int (fd :int) (buf :pointer) (size :int))
(defcfun ("write" write-2) :int (fd :int) (buf :pointer) (size :int))
(defcfun "fcntl" :int (fd :int) (cmd :int) (value :int))
(defcfun "accept" :int (fd :int) (addr :pointer) (addrlen :int))
(defcfun "setsockopt" :int (fd :int) (level :int) (optname :int) (optval :pointer) (optlen :int))

(defun strerror (errnum)
  "Lisp string for particular error. See man strerror(3)."
  (let ((str (make-array 256 :element-type 'character)))
    (with-pointer-to-vector-data (buffer str)
      (foreign-string-to-lisp (strerror-r% errnum buffer 256)))))

(defun errno ()
  "See man errno(3). "
  (mem-ref (errno%) :int))



(mgl-pax:defsection  @async-server
    (:title "Asynchronous TLS server")
  (client type)
  (@communication-setup mgl-pax:section)
  (@request-handling mgl-pax:section))

(mgl-pax:defsection @communication-setup
    (:title "Communication setup")
  (make-ssl-context function))

(mgl-pax:defsection @request-handling
    (:title "Communication setup")
  "When POLL returns, each client is tested by PROCESS-CLIENT-FD whether read (or
write is possible). If so, DO-SOCK-READ is called to read the data,
DECRYPT-SOCKET-OCTETS decrypts them, and calls a callback (slot IO-ON-READ of
the client) to process them.

The application defined callback reads the data using SSL-READ, processes them,
and sends response with SEND-UNENCRYPTED-BYTES.

SEND-UNENCRYPTED-BYTES feeds data to a client buffer (slot ENCRYPT-BUF), and
later this data are processed by DO-ENCRYPT and after encryption are buffered in
WRITE-BUF of the client. These data are sent to the client socket eventually by
DO-SOCK-WRITE called from PROCESS-CLIENT-FD on the callback."
  (process-client-fd function)
  (process-data-on-socket function)
  (decrypt-socket-octets function)
  (ssl-read function)
  (send-unencrypted-bytes function)
  (encrypt-data function)
  (write-data-to-socket function))

(defvar *encrypt-buf-size* 256
  "Initial size of the vector holding data to encrypt.")

(defvar *default-buffer-size* 1500) ; close to socket size

(defstruct (client  (:constructor make-client%)
            (:print-object
                    (lambda (object out)
                      (format out "#<client fd ~d, ~d octets to ~a>" (client-fd object)
                              (client-octets-needed object) (client-io-on-read object)))))
  "Data of one client connection. This includes:

- File desriptor (FD),
- SSL data opaque pointer (SSL),
- Input and output BIO for exchanging data with OPENSSL (RBIO, WBIO),
- Unencrypted octets to encrypt and send (WRITE-BUF),
- Encrypted octets to send to the file descriptor (ENCRYPT-BUF),
- Callback function when read data are available (IO-ON-READ).
- OCTETS-NEEDED number of octets required by IO-ON-READ, if FRAGMENT-OK this is an upper limit."
  (fd -1 :type fixnum :read-only t)
  (ssl (null-pointer) :type cffi:foreign-pointer :read-only nil) ; mostly RO, but invalidated afterwards
  (rbio (null-pointer) :type cffi:foreign-pointer :read-only t)
  (wbio (null-pointer) :type cffi:foreign-pointer :read-only t)
  (write-buf nil :type (or null cons))
  (encrypt-buf (make-array *encrypt-buf-size* :element-type '(unsigned-byte 8))
   :type (simple-array (unsigned-byte 8)))
  (io-on-read #'process-client-hello :type compiled-function)
  (fdset-idx 0 :type fixnum :read-only nil) ; could be RO, but...
  (octets-needed +client-preface-length+ :type fixnum)
  (encrypt-buf-size 0 :type fixnum)
  (start-time (get-internal-real-time) :type fixnum)
  (state (list 'CAN-WRITE 'CAN-WRITE-SSL))
  ;; set of CAN-READ-PORT, CAN-READ-SSL, HAS-DATA-TO-ENCRYPT, CAN-WRITE-SSL,
  ;; CAN-READ-BIO, HAS-DATA-TO-WRITE, CAN-WRITE
  ;; BIO-NEEDS-READ
  )

(defmacro define-reader (name source args &body body &aux declaration)
  (when (eq (caar body) 'declare)
    (setq declaration (pop body)))
  (destructuring-bind (client vector size) args
    (declare (ignore client))
    `(progn
       (defun ,name ,args
         ,(format nil "Move up to ~a octets from ~a to the ~a.~2%Return 0 when no data are
available. Raise an error on error." size source vector)
         ,declaration
         ,@body)
       (setf (get ',name 'source) ',source))))

(defmacro define-writer (name destination args &body body &aux declaration)
  #+nil  (unless (equal (symbol-name name)
                        (format nil "WRITE-TO-~a" (symbol-name destination)))
           (warn "Writer name mismatch: ~a should be write-to-~a" name destination))
  (destructuring-bind (client vector from to) args
    (declare (ignore client from to))
    (when (eq (caar body) 'declare)
      (setq declaration (pop body)))
    `(progn
       (defun ,name ,args
         ,(format nil "Move octets from ~a to the ~a.~2%Return 0 when no data are
available. Raise an error on error." vector destination)
         ,declaration
         (progn ,@body))
       (setf (get ',name 'destination) ',destination))))


;;;; Client state
(defun if-state (client state)
  (member state (client-state client)))

(defun add-state (client state)
  (pushnew state (client-state client)))

(defun remove-state (client state)
  (setf (client-state client)
        (remove state (client-state client))))

;;;; Input port

(defun setup-port (socket nagle)
  "Set the TCP socket: nonblock and possibly nagle."
  (unless (zerop (fcntl socket f-setfl
                        (logior o-nonblock (fcntl socket f-getfl 0))))
    (error "Could not set O_NONBLOCK on the client"))
  (unless nagle
    (unless (zerop
             (with-foreign-object (flag :int)
               (setf (mem-ref flag :int) 1)
               (setsockopt socket ipproto-tcp tcp-nodelay
                           flag (foreign-type-size :int))))
      (error "Could not set O_NODELAY on the client"))))

(define-reader read-from-peer peer-in (client vec vec-size)
  (with-pointer-to-vector-data (buffer vec)
    (let ((read (read-2 (client-fd client) buffer vec-size)))
      (cond ((plusp read) read)
            ((zerop read) (signal 'done))
            ((> -1 read) (error "This cannot happen #2"))
            ((/= (errno) EAGAIN)
             (warn "Read error: ~a" (strerror (errno)))
             (signal 'done))
            (t 0)))))

;;;; Read BIO (rbio)

;;; This name is somewhat confusing - it is BIO for SSL reads, so it actually
;;; gets written to.

(define-writer write-octets-to-decrypt openssl-to-decrypt (client vector from to)
  (with-pointer-to-vector-data (buffer vector)
    (let ((written (bio-write (client-rbio client)
                              (inc-pointer buffer from)
                              (- to from))))
      (when (minusp written) (error "Bio-write failed"))
      (maybe-init-ssl client)
      written)))

(defun decrypt-socket-octets (client vector from to)
  "Process SIZE bytes received from the peer that are in the BUFFER.

Fed them into the SSL object to be unencrypted, and let the client callback
handle the data."
  (push-bytes client
              'use-decrypted-octets
              #'write-octets-to-decrypt vector from to))

(defun process-data-on-socket (client)
  "Read data from client socket. If something is read (and it should, as this is
called when there should be a data), it is passed to the tls buffer and
decrypted."
  (when
      (pull-push-bytes client #'read-from-peer
                       #'decrypt-socket-octets)
    (add-state client 'CAN-READ-SSL)
#+nil    (when (if-state client 'bio-needs-read))
    (add-state client 'CAN-READ-BIO))
  (remove-state client 'CAN-READ-PORT))

;;;; Read SSL
(define-reader ssl-read input-ssl (client vec size)
  (let ((res
          (with-pointer-to-vector-data (buffer vec)
            (ssl-read% (client-ssl client) buffer size))))
    (unless (plusp res)
        (remove-state client 'can-read-ssl))
    (do-io-if-wanted client res)
    (max 0 res)))

;;;; Run application
(defun use-decrypted-octets (client written)
  "Consume decrypted octets as long as they go. Call application code in the
callback; this should send data to the encrypt queue."
  (declare (ignore written))
  (loop while
        (if (plusp (client-octets-needed client))
            (on-complete-ssl-data client)
            (funcall (client-io-on-read client) client))))

;;;; Encrypt queue
(defun send-unencrypted-bytes (client new-data comment)
  "Collect new data to be encrypted and sent to client.

Data are just concatenated to the ENCRYPT-BUF. Later, they would be encrypted
and passed."
  (declare (ignore comment))
  (let ((old-fp (client-encrypt-buf-size client)))
    (setf (client-encrypt-buf-size client) (+ old-fp (length new-data)))
    (loop
      while (> (client-encrypt-buf-size client) (length (client-encrypt-buf client)))
      do (setf (client-encrypt-buf client)
               (double-buffer-size (client-encrypt-buf client))))
    (replace (client-encrypt-buf client) new-data :start1 old-fp))
  (add-state client 'has-data-to-encrypt))

;;;; Write to SSL
(define-writer encrypt-some output-ssl (client vector from to)
  (with-pointer-to-vector-data (buffer vector)
      (let* ((ssl (client-ssl client))
             (res (ssl-write ssl (inc-pointer buffer from) (- to from))))
        (cond
          ((plusp res)
           (add-state client 'can-read-bio)
           res)
          (t (let ((status (ssl-get-error ssl res)))
               (warn "ssl-write not full: ~d" res)
               (when (not (member status
                                  (list ssl-error-none ssl-error-want-write ssl-error-want-read)))
                 (error "SSL write failed, status ~d" status))
               (remove-state client 'can-write-ssl)
               0))))))

(defun encrypt-data-internal (client data)
  "Move octets from DATA to the ssl and then to write buffer (w/o flush)."
  (remove-state client 'has-data-to-encrypt)
  (push-bytes client
              (constantly nil)
              (lambda (client vector from to) (encrypt-some client vector from to))
              data 0 (client-encrypt-buf-size client)))

(defun encrypt-data (client)
  "Encrypt data in client's ENCRYPT-BUF.

Do nothing if no data to encrypt or SSL not yet initialized (and return zero).

Otherwise, use a temporary vector to write data "
  (unless (or (zerop (client-encrypt-buf-size client))
              (zerop (ssl-is-init-finished (client-ssl client))))
    (encrypt-data-internal client
                           (client-encrypt-buf client))))

;;;; Write BIO
(define-reader read-encrypted-from-openssl bio-out (client vec size)
  (declare ((simple-array (unsigned-byte 8)) vec)
           (fixnum size))
  (with-pointer-to-vector-data (buffer vec)
    (let ((res (bio-read% (client-wbio client) buffer size)))
      (cond ((plusp res)
             (add-state client 'has-data-to-write)
             res)
            ((zerop (bio-test-flags (client-wbio client) bio-flags-should-retry))
             (error "Failed to read from bio, and cant retry"))
            (t
             (remove-state client 'can-read-bio)
             0)))))

(defun move-encrypted-bytes (client)
  "Move data encrypted by OpenSSL to the output socket queue, and then to the
socket.

This should be called in a way friendly to Nagle algorithm. My understaning is
this is either when we pipeline a lot of data, or when we send out somethinf
that expects a response."

  (pull-push-bytes client #'read-encrypted-from-openssl #'queue-encrypted-bytes)
  (write-data-to-socket client))

;;;; TCP write port
(define-writer send-to-peer peer-out (client vector from to)
  (with-pointer-to-vector-data (buffer vector)
    (let ((res (write-2 (client-fd client)
                        (inc-pointer buffer from)
                        (- to from)))
          (err (errno)))
      (cond ((and (= res -1)
                  (= err eagain))
             (remove-state client 'can-write)
             0)
            ((= res -1)
             ;; e.g., broken pipe
             (invoke-restart 'kill-http-connection)
             (error "Error during write: ~d (~a)" err (strerror err)))
            ((plusp res) res)
            (t (error "This cant happen (#1)"))))))

(defun write-data-to-socket (client)
  "Write buffered encrypted data to the client socket. Update the write buffer to
keep what did not fit."
  (let ((concated (concatenate* (client-write-buf client)))) ;;DEBUG
    (let ((written
            (push-bytes client (constantly nil)
                         #'send-to-peer
                         concated 0 (length concated))))
      (setf (client-write-buf client)
            (cond ((= written (length concated))
                   (remove-state client 'has-data-to-write)
                   nil)
                  ((plusp written)
                   (remove-state client 'can-write)
                   (list (subseq concated written)))
                  (t (error "Write failed")))))))

;;;; Action selector
(defun do-available-actions (client)
  (loop
    (cond
      ((if-state client 'can-read-port)
       (process-data-on-socket client))
      ((if-state client 'can-read-ssl)
       (use-decrypted-octets client nil))
      ((and (if-state client 'has-data-to-encrypt)
            (if-state client 'can-write-ssl))
       (encrypt-data client))
      ((if-state client 'can-read-bio)
       (move-encrypted-bytes client))
      ((and (if-state client 'has-data-to-write)
            (if-state client 'can-write))
       (write-data-to-socket client))
      (t (return)))))


(defun use-pem-for (context fn path error)
  "Apply FN on file on CONTEXT, PATH and SSL-FILETYPE-PEM.

FN is expected to be one of SSL-CTX-use-XXX-file."
  (setf path (merge-pathnames path))
  (let ((full-path (probe-file (merge-pathnames path))))
    (unless full-path
      (error "~a (No PEM file ~a)" error path))
    (with-foreign-string (file (namestring full-path))
      (unless (= 1 (funcall fn context file ssl-filetype-pem))
        (error "~a (Failed processing ~a)" error path)))))

(defun make-ssl-context ()
  "Make a SSL context for http2 server..

This includes public and private key pair (from files in this directory),

Functionally, it is same as TLS-SERVER/MINI-HTTP2:MAKE-HTTP2-TLS-CONTEXT;
however, it used directly cffi and sets some parameters in a different way."
  (let ((context (ssl-ctx-new (tls-method)))
        (*default-pathname-defaults*
          (asdf:component-pathname (asdf:find-system "tls-server"))))
    (when (null-pointer-p context)
      (error "Could not create context"))
    (use-pem-for context #'ssl-ctx-use-certificate-file #P"certs/server.pem"
                 "failed to load server certificate")
    (use-pem-for context #'ssl-ctx-use-privatekey-file #P"certs/server.key"
                 "failed to load server private key")
    (unless (= 1 (ssl-ctx-check-private-key context))
      (error "server private/public key mismatch"))
    (ssl-ctx-set-options context ssl-op-all)
    (ssl-ctx-ctrl context ssl-ctrl-set-min-proto-version tls-1.2-version (null-pointer))
    (tls-server/mini-http2::ssl-ctx-set-alpn-select-cb  context (get-callback 'tls-server/mini-http2::select-h2-callback))
    context))

(defmacro with-ssl-context ((ctx) &body body)
  "Run body with SSL context created by MAKE-SSL-CONTEXT in CTX."
  (check-type ctx symbol)
  `(let ((,ctx (make-ssl-context)))
     (unwind-protect
          (progn ,@body)
       (ssl-ctx-free ,ctx))))

;; Moving data around
(deftype reader () '(function (t t fixnum) fixnum))
(deftype writer () '(function (t t t fixnum) fixnum))

(defun pull-push-bytes (client in-fn out-fn)
  "Read data using IN-FN and write them out using OUT-FN.

Pass CLIENT as the first argument to both of them. the other are vector to read
or write and the size to read or write. Read function should return number of
bytes read.

Finish when the last read reads nothing.

Assumes writes cannot fail."
  (declare (optimize speed (safety 1) (debug 0))
           (reader in-fn)
           (writer out-fn))
  (let ((vec (make-array *default-buffer-size* :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent vec))
    (loop
      for read = nil then t
      for n fixnum = (funcall in-fn client vec *default-buffer-size*)

          while (plusp n)
          ;; assumption: never fail
          do
             (assert (= n (funcall out-fn client vec 0 n)))
          finally (return read))))

(defun pull-once-push-bytes (client in-fn out-fn)
  "Read data using IN-FN and write them out using OUT-FN.

Pass CLIENT as the first argument to both of them. the other are vector to read
or write and the size to read or write. Read function should return number of
bytes read.

Finish when the last read reads nothing.

Assumes writes cannot fail."
  (declare (optimize speed (safety 1) (debug 0))
           (reader in-fn)
           (writer out-fn))
  (let* ((vec (make-array *default-buffer-size* :element-type '(unsigned-byte 8)))
         (n (funcall in-fn client vec *default-buffer-size*)))
    (assert (= n (funcall out-fn client vec 0 n)))))

(defun push-bytes (client next-fn out-fn vector from to)
  "Process octets in the VECTOR in the interval <FROM TO).

Call OUT-FN on VECTOR, FROM TO. It returns number of processed octets (or 0 if
none, or raises an error). If something is written, NEXT-FN is also called to do \"next stage\".

Repeat on partial write."
  (if (= from to)
      from
      (loop
        for written = (funcall out-fn client vector from to)
        do
           (incf from written)
           (cond
             ((= from to)
              (funcall next-fn client written)
              (return from))
             ((plusp written)
              (funcall next-fn client written))
             ((zerop written)
              (return from))))))

(defun do-io-if-wanted (client ret)
  "Check real error after a call to SSL_connect, SSL_accept,
SSL_do_handshake, SSL_read_ex, SSL_read, SSL_peek_ex, SSL_peek, SSL_shutdown,
SSL_write_ex or SSL_write.

If the operation was successfully completed, do nothing.

If it is a harmless one (want read or want write), try to process the data.

Raise error otherwise."
  (let ((ssl (client-ssl client))
        (wbio (client-rbio client)))
    (let ((err-code (ssl-get-error ssl ret)))
      (cond
        ;; after ssl read
        ((= err-code ssl-error-want-write)
         (when (zerop (bio-test-flags wbio bio-flags-should-retry))
           (error "Retry flag should be set.")))
        ((= err-code ssl-error-want-read)
         ;; This is relevant for accept call and handled in loop
         ;; may be needed for pull phase
         (when (zerop (bio-test-flags wbio bio-flags-should-retry))
           (error "Retry flag should be set."))
         #+nil(move-encrypted-bytes client))
        ((= err-code ssl-error-none) nil)
        ((= err-code ssl-error-zero-return)
         ;; Peer closed TLS connection
         (invoke-restart 'kill-http-connection))
        ((= err-code ssl-error-ssl)
         (process-ssl-errors))
        ((= err-code ssl-error-syscall)
         (warn "SSL syscall error ~d (~a) " (errno) (strerror (errno)))
         (invoke-restart 'kill-http-connection))
        (t (warn "SSL error ~d" err-code)
           (invoke-restart 'kill-http-connection))))))

(defun ssl-err-reason-error-string (code)
  (foreign-string-to-lisp (err-reason-error-string code)))

(defun process-ssl-errors ()
  (loop for err = (err-get-error)
        until (zerop err)
        unless (position err #(#xa000412 )) ;bad certificate
          do
             (cerror "Ignore" 'ssl-error-condition :code err)))

(defun maybe-init-ssl (client)
  "If SSL is not initialized yet, initialize it.

SSL_accept may want to read or write something. Writing is handled by DO-IO-IF-WANTED, reading is handled by returning T so that caller knows to retry."
  (when (zerop (ssl-is-init-finished (client-ssl client)))
    (do-io-if-wanted client (ssl-accept (client-ssl client)))
    t))

(defun doubled-buffer (buffer)
  "Return a larger buffer with same initial data as the provided one."
  (let ((new (make-array (* 2 (length buffer))
                         :element-type '(unsigned-byte 8))))
    (replace new buffer)
    new))

(defun double-buffer-size (old)
  "Analog of realloc."
  (let ((new (make-array (* 2 (length old))
                         :element-type '(unsigned-byte 8))))
    (replace new old)
    new))

(define-writer queue-encrypted-bytes write-buffer (client new-data from to)
  (setf (client-write-buf client)
        (append (client-write-buf client)
                (list (subseq new-data from to))))
  (- to from))

(define-condition done (error)
  ()
  (:documentation "The socket on the other side is closed."))

(define-condition ssl-error-condition (error)
  ((code :accessor get-code :initarg :code))
  (:documentation "The socket on the other side is closed."))

(defmethod print-object ((object ssl-error-condition) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~x: ~a" (get-code object)
            (ssl-err-reason-error-string (get-code object)))))

(defun concatenate* (vectors)
  (let* ((len (reduce #'+ vectors :key #'length))
         (res (make-array len :element-type '(unsigned-byte 8))))
    (loop for v of-type (simple-array (unsigned-byte 8)) in vectors
          with i = 0
          do
             (setf (subseq res i (+ i (length v))) v)
             (incf i (length v))
          finally (return res))))

(defun process-client-fd (fd-ptr client)
  "Process events available on FD-PTR with CLIENT.

Read if you can, write if you can, announce DONE when done."
  (with-foreign-slots ((fd events revents) fd-ptr (:struct pollfd))
    (when (plusp (logand c-pollin revents))
      (add-state client 'can-read-port))
    (when (plusp (logand c-pollout revents))
      (add-state client 'can-write))
    (when (plusp (logand revents  (logior c-POLLERR  c-POLLHUP  c-POLLNVAL)))
      (signal 'done))
    (do-available-actions client)))

(defvar *fdset-size* 10
  "Size of the fdset - that, is, maximum number of concurrent clients.")

(defvar *empty-fdset-items* nil "List of empty slots in the fdset table.")

(defun make-client-object (socket ctx s-mem)
  "Create new CLIENT object suitable for TLS server."
  (let* ((client (make-client% :fd socket
                               :rbio (bio-new s-mem)
                               :wbio (bio-new s-mem)
                               :ssl (ssl-new ctx)
                               :io-on-read #'process-client-hello
                               :octets-needed +client-preface-length+)))
    (ssl-set-accept-state (client-ssl client)) ; no return value
    (ssl-set-bio (client-ssl client) (client-rbio client) (client-wbio client))
    client))

(defun set-fd-slot (fdset socket new-events idx)
  "Set FD and EVENTS of slot IDX in fdset."
  (with-foreign-slots ((fd events)
                       (inc-pointer fdset (* idx size-of-pollfd))
                       (:struct pollfd))
    (when socket (setf fd socket events new-events))))

(defun add-socket-to-fdset (fdset socket client)
  "Find free slot in the FDSET and put client there."
  (let ((fd-idx (pop *empty-fdset-items*)))
    (set-fd-slot fdset socket  (logior c-pollerr c-pollhup c-pollnval c-pollin) fd-idx )
    (setf (client-fdset-idx client) fd-idx)))

(defun init-fdset (fdset size)
  (loop for i from 0 to (1- size)
        do
           (set-fd-slot fdset -1 0 i)))

(defun handle-client-io (client fdset)
  (let ((fd-ptr (inc-pointer fdset (* (client-fdset-idx client) size-of-pollfd))))
    (process-client-fd fd-ptr client)
    (when (plusp (client-encrypt-buf-size client))
      (let ((written-octets (encrypt-data client)))
        (replace (client-encrypt-buf client) (client-encrypt-buf client)
                 :start2 written-octets :end2 (client-encrypt-buf-size client))
        (decf (client-encrypt-buf-size client) written-octets)))
    (with-foreign-slots ((events)
                         fd-ptr
                         (:struct pollfd))
      (setf events
            (if (client-write-buf client)
                (logior events c-pollout)
                (logand events (logxor -1 c-pollout)))))))

(defun setup-new-connect-pollfd (fdset listening-socket)
  (set-fd-slot fdset
               (sb-bsd-sockets:socket-file-descriptor (usocket:socket listening-socket))
                (logior c-pollerr c-pollhup c-pollnval c-pollin)
                0))

(defvar *nagle* t
  "If nil, disable Nagle algorithm (= enable nodelay)")

(defvar *clients* nil
  "List of clients processed.")

(defun process-new-client (fdset listening-socket ctx s-mem)
  "Add new client: accept connection, create client and add it to pollfd and to *clients*."
  (with-foreign-slots ((revents) fdset (:struct pollfd))
    (if (plusp (logand c-pollin revents))
        (let* ((socket (accept
                        (sb-bsd-sockets::socket-file-descriptor (usocket:socket listening-socket)) (null-pointer) 0))
               (client (make-client-object socket ctx s-mem)))
          (setup-port socket *nagle*)
          (add-socket-to-fdset fdset socket client)
          ;; maybe TODO: if no fdset slot available, stop reading listening socket
          (push client *clients*)))
    (if (plusp (logand revents  (logior c-POLLERR  c-POLLHUP  c-POLLNVAL)))
        (error "Error on listening socket"))))

(defun close-client-connection (fdset client)
  (setf *clients* (remove client *clients*))
  (unless (null-pointer-p (client-ssl client))
    (ssl-free (client-ssl client)))     ; BIOs are closed automatically
  (setf (client-ssl client) (null-pointer))
  (push (client-fdset-idx client) *empty-fdset-items*)
  (set-fd-slot fdset -1 0 (client-fdset-idx client))
  (close-fd (client-fd client)))

(defun process-client-sockets (fdset nread)
  (unless (zerop nread)
    (dolist (client *clients*)
      (restart-case
          (handler-case
              (handle-client-io client fdset)
            (done () (invoke-restart 'kill-http-connection)))
        (kill-http-connection ()
          (close-client-connection fdset client))))))

(defun serve-tls (listening-socket)
  (with-foreign-object (fdset '(:struct pollfd) *fdset-size*)
    (init-fdset fdset *fdset-size*)
    (with-ssl-context (ctx)
      (let* ((s-mem (bio-s-mem))
             ;; FIXME: if *clients* was bound, it cannot be easily observed from
             ;; monitoring;
             #+(or) (*clients* nil)
             (*empty-fdset-items* (alexandria:iota (1- *fdset-size*) :start 1)))
        (setup-new-connect-pollfd fdset listening-socket)
        (unwind-protect
             (loop
               (let ((nread (poll fdset *fdset-size* -1)))
                 (process-new-client fdset listening-socket ctx s-mem)
                 (process-client-sockets fdset nread)))
          (dolist (client *clients*)
            (close-client-connection fdset client)))))))

(defmethod do-new-connection (socket (tls (eql :tls)) (dispatch-method (eql :async-custom))
                              &key ((:nagle *nagle*) *nagle*))
  "Handle new connections by adding pollfd to and then polling.

When poll indicates available data, process them with openssl using BIO. Data to
the client are sent to SSL to BIO to socket buffer (and again poll to write
them).

This in the end does not use usocket, async nor cl+ssl - it is a direct rewrite
from C code."
  (serve-tls socket)
  ;; there is an outer loop in create-server that we want to skip
  (invoke-restart 'kill-server))



;;;; HTTP2 TLS async client
(defun on-complete-ssl-data (client)
  "Read exactly OCTETS octets from SSL to VEC.

Raise error if only part of data is available. FIXME: process that anyway"
  (let ((vec (make-shareable-byte-vector +client-preface-length+))
        (fn (client-io-on-read client))
        (octets (client-octets-needed client)))
      ;; TODO: check first with SSL_pending?
    (let ((read (ssl-read client vec octets)))
      (cond
        ((not (plusp read)) nil)          ; just return and try again
        ((/= read octets)
         (error "Read ~d octets. This is not enough octets, why?~%~s~%" read (subseq vec 0 read)))
        (t (funcall fn client vec)
           t ; read more data
           )))))

(defun process-client-hello (client vec)
  (cond ((equalp vec +client-preface-start+)
         (send-unencrypted-bytes client tls-server/mini-http2::*settings-frame* 'settings)
         (send-unencrypted-bytes client tls-server/mini-http2::*ack-frame* 'ack-settings)
         (setf (client-io-on-read client) #'process-header
               (client-octets-needed client) 9)
         (move-encrypted-bytes client))
        (t
         (error 'client-preface-mismatch :received vec))))

(defun process-header (client header)
  (let* ((frame-size (get-frame-size header))
         (type (get-frame-type header)))
    (declare ((unsigned-byte 8) type)
             (frame-size frame-size))
    #+nil      (format t "Header read: type ~d, expecting frame with ~d octets~%" type frame-size)

    (when (= type +goaway-frame-type+)
      ;; TODO: handle go-away frame
      )
    (unless (>= 16384 frame-size)
      (error  "Too big header! go-away"))
    (let ((id-to-process (get-stream-id-if-ends header)))
      (when id-to-process
        (send-unencrypted-bytes client
                                (buffer-with-changed-stream *header-frame* id-to-process)
                                'response-headers)
        (send-unencrypted-bytes client
                                (buffer-with-changed-stream *data-frame* id-to-process)
                                'response-payload)
#+nil        (write-data-to-socket client)))
    (ignore-bytes client frame-size)))

(defun really-ignore-bytes (client vec count)
  (let ((size (- (client-octets-needed client))))
    (if (plusp size)
      (ssl-read client vec (min size count))
      0)))

(defun really-ignore (client vec from to)
  "Ignore COUNT bytes.

It means, account for COUNT bytes to ignore in OCTETS-NEEDED slot and if done,
process to read the next header."
  (declare (ignorable client vec))

  (when (zerop (incf (client-octets-needed client) (- to from)))
    (setf (client-io-on-read client) #'process-header
          (client-octets-needed client) 9)
    (on-complete-ssl-data client))
  (- to from))

(defun pull-from-ignore (client) ; name me
  (pull-push-bytes client
                   #'really-ignore-bytes
                   #'really-ignore)
  (on-complete-ssl-data client))

(defun ignore-bytes (client count)
  (if (zerop count)
      (setf (client-octets-needed client) 9
            (client-io-on-read client) #'process-header)
      (setf (client-octets-needed client) (- count)
            (client-io-on-read client) #'pull-from-ignore))
  t)
