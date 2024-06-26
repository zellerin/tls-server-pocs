(in-package #:tls-server/async/tls)

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
(defcfun "SSL_pending" :int (ssl :pointer))
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
  (@app-interface mgl-pax:section)
  (@request-handling mgl-pax:section)
  (@communication-setup mgl-pax:section))

(mgl-pax:defsection @app-interface
    (:title "Interface to the application")
  (client-application-data function)
  (set-next-action function)
  (ssl-read function)
  (send-unencrypted-bytes function)
  (poll-timeout condition)
  (*no-client-poll-timeout* variable)
  (*poll-timeout* variable))

(mgl-pax:defsection @communication-setup
    (:title "HTTP2 handling")
  (make-ssl-context function)
  (make-client-object function)
  (process-client-hello function)
  (process-header function)
  (ignore-bytes function)
  (print-goaway-frame function))

(mgl-pax:defsection @request-handling
    (:title "Client actions loop")
  "Each client has a STATE that encapsulates what actions are effectively possible.
SELECT-NEXT-ACTION selects appropriate action. When no new action is available,
next client is handled and eventually POLL called when all clients were served.

When POLL returns, new action is available for some client (read from a socket or write to it).

The actions are in general indicated by arrows in the diagram:

![](flow.svg)"

  (process-client-fd function)
  (do-available-actions function)
  (select-next-action function)

  (process-data-on-socket function)
  (encrypt-data function)
  (move-encrypted-bytes function)
  (write-data-to-socket function))

(defvar *encrypt-buf-size* 256
  "Initial size of the vector holding data to encrypt.")

(defvar *default-buffer-size* 1500) ; close to socket size

(defvar *client-hello-callback* nil)

(defstruct (client  (:constructor make-client%)
            (:print-object
                    (lambda (object out)
                      (format out "#<client fd ~d, ~d octets to ~a>" (client-fd object)
                              (client-octets-needed object) (client-io-on-read object)))))
  "Data of one client connection. This includes:

- File descriptor of underlying socket (FD),
- Opaque pointer to the openssl handle (SSL),
- Input and output BIO for exchanging data with OPENSSL (RBIO, WBIO),
- Plain text octets to encrypt and send (ENCRYPT-BUF),
- Encrypted octets to send to the file descriptor (WRITE-BUF),
- Callback function when read data are available (IO-ON-READ).
- Number of octets required by IO-ON-READ. Negative values have special handling.
- Client state from the low-level data flow point of view (STATE)
- Application data (slot to be used by the application)"
  (fd -1 :type fixnum :read-only t)
  (ssl (null-pointer) :type cffi:foreign-pointer :read-only nil) ; mostly RO, but invalidated afterwards
  (rbio (null-pointer) :type cffi:foreign-pointer :read-only t)
  (wbio (null-pointer) :type cffi:foreign-pointer :read-only t)
  (write-buf nil :type (or null cons))
  (encrypt-buf (make-array *encrypt-buf-size* :element-type '(unsigned-byte 8))
   :type (simple-array (unsigned-byte 8)))
  (io-on-read *client-hello-callback* :type compiled-function)
  (fdset-idx 0 :type fixnum :read-only nil) ; could be RO, but...
  (octets-needed +client-preface-length+ :type fixnum)
  (encrypt-buf-size 0 :type fixnum)
  (start-time (get-internal-real-time) :type fixnum)
  (state (list 'CAN-WRITE 'CAN-WRITE-SSL 'bio-needs-read 'ssl-init-needed))
  ;; set of CAN-READ-PORT, CAN-READ-SSL, HAS-DATA-TO-ENCRYPT, CAN-WRITE-SSL,
  ;; CAN-READ-BIO, HAS-DATA-TO-WRITE, CAN-WRITE
  ;; BIO-NEEDS-READ SSL-INIT-NEEDED
  application-data)

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

(defun states-to-string (state)
  "Short string describing the state using codes on the diagram."
  (with-output-to-string (*standard-output*)
    (when (member 'can-read-port state)
      (princ #\①))
    (when (member 'can-read-ssl state)
      (princ #\③))
    (when (member 'has-data-to-write state)
      (princ #\ⓤ))
    (when (member 'can-write-ssl state)
      (princ #\④))
    (when (member 'can-read-bio state)
      (princ #\⑤))
    (when (member 'has-data-to-write state)
      (princ #\Ⓔ))
    (when (member 'can-write state)
      (princ #\⑥))))



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
      (unless (= read vec-size)
        (remove-state client 'CAN-READ-PORT))
      (cond ((plusp read) read)
            ((zerop read)  (signal 'done))
            ((> -1 read) (error "This cannot happen #2"))
            ((/= (errno) EAGAIN)
             (warn "Read error (~d): ~d ~a" read (errno) (strerror (errno)))
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
      (unless (plusp written) (error "Bio-write failed"))
      written)))

(defun decrypt-socket-octets (client vector from to)
  "Send data in the VECTOR between FROM and TO to the ② openssl for decryption ."
  (push-bytes client
              (constantly nil)
              #'write-octets-to-decrypt vector from to))

(defun process-data-on-socket (client)
  "Read data from client socket ① and pass them to the tls buffer ② to decrypt."
  (unless
      (pull-once-push-bytes client #'read-from-peer
                              #'decrypt-socket-octets)
    (remove-state client 'can-read-port))
  (add-state client 'CAN-READ-SSL)
  (add-state client 'can-write-ssl)
  (when (if-state client 'bio-needs-read)
    (add-state client 'CAN-READ-BIO)
    (remove-state client
                  'BIO-NEEDS-READ)))

;;;; Read SSL
(defun ssl-read (client vec size)
   "Move up to SIZE octets from the decrypted SSL ③ to the VEC.

Return 0 when no data are available. Possibly remove CAN-READ-SSL flag."
   nil
   (let ((res
          (with-pointer-to-vector-data (buffer vec)
            (ssl-read% (client-ssl client) buffer size))))
     (unless (plusp res) (remove-state client 'can-read-ssl))
     (handle-ssl-errors client res)
     (max 0 res)))

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
               (cond ((member status
                                  (list ssl-error-none ssl-error-want-write ssl-error-want-read))
                      (remove-state client 'can-write-ssl)
                      (add-state client 'has-data-to-encrypt)
                      0)
                     ((= status ssl-error-zero-return)
                      (warn "Peer send close notify alert. One possible cause - it does not like our certificate")
                      (signal 'done))
                     (t (error "SSL write failed, status ~d" status)))))))))

(defun encrypt-data (client)
  "Encrypt data in client's ENCRYPT-BUF.

Do nothing if there is no data to encrypt or SSL not yet initialized (and return zero).

Otherwise, use a temporary vector to write data "
  (push-bytes client
              (lambda (client written)
                (declare (ignore written))
                (setf (client-encrypt-buf-size client) 0)
                (remove-state client 'has-data-to-encrypt))
              (lambda (client vector from to) (encrypt-some client vector from to))
              (client-encrypt-buf client) 0 (client-encrypt-buf-size client)))

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
  "Move data encrypted by OpenSSL to the socket write queue Ⓔ.

This should be called in a way friendly to Nagle algorithm. My understaning is
this is either when we pipeline a lot of data, or when we send out somethinf
that expects a response."
  (pull-push-bytes client #'read-encrypted-from-openssl #'queue-encrypted-bytes)
  (add-state client 'has-data-to-write))

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
  "Write buffered encrypted data Ⓔ to the client socket ⑥. Update the write buffer to
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

(defun encrypt-and-send (client)
  (encrypt-data client)
  (move-encrypted-bytes client)
  (write-data-to-socket client))

;;;; Action selector
(defun select-next-action (client)
  "One of possible next actions consistent with then the state of the client, or
nil if no action is available.

This is factored out so that it can be traced. There is a
TLS-SERVER/MEASURE::ACTIONS clip on this function."
  (cond
    ((if-state client 'can-read-port) #'process-data-on-socket)
    ((if-state client 'can-read-ssl)
     (if (plusp (client-octets-needed client))
         #'on-complete-ssl-data
         (client-io-on-read client)))
    ((and (if-state client 'has-data-to-encrypt)
          (if-state client 'can-write-ssl))
     #'encrypt-data)
    ((if-state client 'can-read-bio) #'move-encrypted-bytes)
    ((and (if-state client 'has-data-to-write)
          (if-state client 'can-write))
     #'write-data-to-socket)
    ((and (if-state client 'ssl-init-needed)
          (not (if-state client 'bio-needs-read)))
     #'maybe-init-ssl)
    (t nil)))

(defun do-available-actions (client)
  "Run available actions as selected by SELECT-NEXT-ACTION till there is none."
  (loop
    for action = (select-next-action client)
    while action do (funcall action client)))


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
    (assert (= n (funcall out-fn client vec 0 n)))
    (= (the fixnum *default-buffer-size*) n)))

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

(defun handle-ssl-errors (client ret)
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
           (error "Retry flag should be set."))
         (warn "This path wath not tested."))
        ((= err-code ssl-error-want-read)
         ;; This is relevant for accept call and handled in loop
         ;; may be needed for pull phase
         (add-state client 'bio-needs-read)
         ;; is this needed?
         (when (zerop (bio-test-flags wbio bio-flags-should-retry))
           (error "Retry flag should be set.")))
        ((= err-code ssl-error-none) nil)
        ((= err-code ssl-error-zero-return)
         ;; Peer closed TLS connection
         (add-state client 'peer-closed-connection))
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
  "If SSL is not initialized yet, initialize it."
  (if (zerop (ssl-is-init-finished (client-ssl client)))
    (handle-ssl-errors client (ssl-accept (client-ssl client)))
    (remove-state client 'ssl-init-needed)))

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
  "Process events available on FD-PTR (a pointer to struct pollfd) with CLIENT.

The new possible action corresponding to ① or ⑥ on the diagram above is added to the client state and DO-AVAILABLE-ACTIONS is called to react to that."
  (with-foreign-slots ((fd events revents) fd-ptr (:struct pollfd))
    (when (plusp (logand c-pollin revents)) (add-state client 'can-read-port))
    (when (plusp (logand c-pollout revents)) (add-state client 'can-write))
    (do-available-actions client)
    (when (plusp (logand revents (logior c-POLLERR  c-POLLHUP  c-POLLNVAL)))
      ;; this happens, e.g., with h2load -D (time based)
      (warn "Poll error for ~a: ~d" client (logand revents (logior c-POLLERR  c-POLLHUP  c-POLLNVAL)))
      (signal 'done))))

(defvar *fdset-size* 10
  "Size of the fdset - that, is, maximum number of concurrent clients.")

(defvar *empty-fdset-items* nil "List of empty slots in the fdset table.")

(defun set-next-action (client action size)
  "Set action for next chunk of received data."
  (setf (client-io-on-read client) action
        (client-octets-needed client) size))

(defun make-client-object (socket ctx s-mem)
  "Create new CLIENT object suitable for TLS server.

Initially, the ENCRYPT-BUFFER contains the settings to send, and next action is
reading of client hello."
  (let* ((client (make-client% :fd socket
                               :rbio (bio-new s-mem)
                               :wbio (bio-new s-mem)
                               :ssl (ssl-new ctx)
                               :octets-needed +client-preface-length+)))
    (ssl-set-accept-state (client-ssl client)) ; no return value
    (ssl-set-bio (client-ssl client) (client-rbio client) (client-wbio client))
    (send-unencrypted-bytes client tls-server/mini-http2::*settings-frame* 'settings)
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
    (with-foreign-slots ((events)
                         fd-ptr
                         (:struct pollfd))
      (setf events
            (if (and (if-state client 'has-data-to-write)
                     (not (if-state client 'can-write)))
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
               (client (when *empty-fdset-items* (make-client-object socket ctx s-mem))))
          (cond
            (client
             (setup-port socket *nagle*)
             (add-socket-to-fdset fdset socket client)
             (push client *clients*))
            (t (close-fd socket)
               (warn "Too many clients, refused to connect a new one.")))))
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

(define-condition poll-timeout (error)
  ()
  (:documentation
   "Poll was called with a timeout and returned before any file descriptor became
ready."))

(defvar *no-client-poll-timeout* -1
        "Timeout in ms to use when there is no client (to limit time to a client to
connect).

This is supposed to be used primarily in the automated tests, where you do not
want indefinite waits in case of a problem.

On timeout signals POLL-TIMEOUT error.

Default -1 means an indefinite wait.")

(defvar *poll-timeout* -1
  "Timeout to use for poll in ms when there connected clients, but no client communication.

On timeout signals POLL-TIMEOUT error.

Default -1 means an indefinite wait.")

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
               (let ((nread (poll fdset *fdset-size*
                                  (if *clients* *poll-timeout* *no-client-poll-timeout*))))
                 (when (zerop nread) (cerror "Ok" 'poll-timeout))
                 (process-new-client fdset listening-socket ctx s-mem)
                 (process-client-sockets fdset nread)))
          (dolist (client *clients*)
            (close-client-connection fdset client)))))))

(defmethod do-new-connection (socket (tls (eql :tls)) (dispatch-method (eql :async-custom))
                              &key
                                ((:nagle *nagle*) *nagle*)
                                ((:client-hello-callback *client-hello-callback*)
                                 (or *client-hello-callback* #'process-client-hello)))
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
  "Read number of octets indicated in CLIENT into a vector and then apply client fn on it.

 FIXME: process that anyway"
  (let* ((octets (client-octets-needed client))
         (vec (make-shareable-byte-vector octets))
         (fn (client-io-on-read client)))
      ;; TODO: check first with SSL_pending?
    (let ((read (ssl-read client vec octets)))
      (cond
        ((not (plusp read))
         (handle-ssl-errors client read))
        ((/= read octets)
         (error "Read ~d octets. This is not enough octets, why?~%~s~%" read (subseq vec 0 read)))
        (t (funcall fn client vec))))))

(defun process-client-hello (client vec)
  "Check first received octets to verify they are the client hello string.

Next, read and process a header."
  (cond ((equalp vec +client-preface-start+)
         (set-next-action client #'process-header 9)
         (add-state client 'reply-wanted))
        (t
         (error 'client-preface-mismatch :received vec))))

(defun print-goaway-frame (client frame)
  (unless (every #'zerop  (subseq frame 4 8))
    (write-line "Got goaway:")
    (format t "Error code: ~s, last strea ~s" (subseq frame 4 8)
            (subseq frame 0 4))
    (write-line (map 'string 'code-char frame) *standard-output* :start 8)
    (set-next-action client #'process-header 9)))

(defun process-header (client header)
  "Process 9 octets as a HTTP2 frame header."
  (let* ((frame-size (get-frame-size header))
         (type (get-frame-type header)))
    (declare ((unsigned-byte 8) type))
    (when (and (= type 4)                                   ; settings
               (zerop (logand 1 (get-frame-flags header)))) ; not ack
      (send-unencrypted-bytes client tls-server/mini-http2::*ack-frame* 'ack-settings))
    (when (= type +goaway-frame-type+)
      ;; TODO: handle go-away frame
      (set-next-action client #'print-goaway-frame frame-size)
      (return-from process-header))
    (unless (>= 16384 frame-size)
      (error  "Too big header! go-away"))
    (let ((id-to-process (get-stream-id-if-ends header)))
      (when id-to-process
        (send-unencrypted-bytes client
                                (buffer-with-changed-stream *header-frame* id-to-process)
                                'response-headers)
        (send-unencrypted-bytes client
                                (buffer-with-changed-stream *data-frame* id-to-process)
                                'response-payload)))
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
    (set-next-action client #'process-header 9)
    (on-complete-ssl-data client))
  (- to from))

(defun pull-from-ignore (client) ; name me
  (pull-push-bytes client
                   #'really-ignore-bytes
                   #'really-ignore))

(defun ignore-bytes (client count)
  (if (zerop count)
      (set-next-action client #'process-header 9)
      (set-next-action client #'pull-from-ignore (- count)))
  t)
