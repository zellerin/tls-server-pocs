(in-package #:tls-server/async/tls)

(define-foreign-library openssl (:unix "libssl.so"))
(use-foreign-library openssl)

(defcfun "BIO_new" :pointer (bio-method :pointer))
(defcfun "BIO_read" :int (bio-method :pointer) (data :pointer) (dlen :int))
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
(defcfun "SSL_read" :int (ssl :pointer) (buffer :pointer) (bufsize :int))
(defcfun "SSL_set_accept_state" :pointer (ssl :pointer))
(defcfun "SSL_set_bio" :void (ssl :pointer) (rbio :pointer) (wbio :pointer))
(defcfun "SSL_write" :int (ssl :pointer) (buffer :pointer) (bufsize :int))
(defcfun "TLS_method" :pointer)

(defcfun "poll" :int (fdset :pointer) (rb :int) (timeout :int))
(defcfun ("close" close-fd) :int (fd :int))
(defcfun ("read" read-2) :int (fd :int) (buf :pointer) (size :int))
(defcfun ("write" write-2) :int (fd :int) (buf :pointer) (size :int))
(defcfun "fcntl" :int (fd :int) (cmd :int) (value :int))
(defcfun "accept" :int (fd :int) (addr :pointer) (addrlen :int))

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
(defvar *max-read-chunks* 10
  "Read up to this number of chunks before encrypting the output.")

(defstruct (client (:constructor make-client%))
  "Data of one client connection. This includes:

- File desriptor (FD),
- SSL data opaque pointer (SSL),
- Input and output BIO for exchanging data with OPENSSL (RBIO, WBIO),
- Unencrypted octets to encrypt and send (WRITE-BUF),
- Encrypted octets to send to the file descriptor (ENCRYPT-BUF),
- Callback function when read data are available (IO-ON-READ).
- OCTETS-NEEDED number of octets required by IO-ON-READ, if FRAGMENT-OK this is an upper limit."
  (fd -1 :type fixnum :read-only t)
  (ssl (null-pointer) :type cffi:foreign-pointer :read-only t)
  (rbio (null-pointer) :type cffi:foreign-pointer :read-only t)
  (wbio (null-pointer) :type cffi:foreign-pointer :read-only t)
  (write-buf nil :type (or null cons))
  (encrypt-buf (make-array *encrypt-buf-size* :element-type '(unsigned-byte 8))
   :type (simple-array (unsigned-byte 8)))
  (io-on-read #'process-client-hello :type compiled-function)
  (fdset-idx 0 :type fixnum :read-only t)
  (octets-needed +client-preface-length+ :type fixnum)
  (encrypt-buf-size 0 :type fixnum))

(defun use-pem-for (context fn path error)
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
    (use-pem-for context #'ssl-ctx-use-certificate-file #P"certs/server.crt"
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
  (check-type ctx symbol)
  `(let ((,ctx (make-ssl-context)))
     (unwind-protect
          (progn ,@body)
       (ssl-ctx-free ,ctx))))

(defun move-encrypted-bytes (wbio client buffer vec)
  "Move data encrypted by OpenSSL to the output socket queue."
  (declare (optimize speed (safety 1) (debug 0))
           ((simple-array (unsigned-byte 8)) vec))
  (loop for n fixnum = (bio-read wbio buffer *default-buffer-size*)
        while (plusp n)
        do (queue-encrypted-bytes client (subseq vec 0 n))))

(defun do-io-if-wanted (client ret)
  "Check real error after a call to SSL_connect, SSL_accept,
SSL_do_handshake, SSL_read_ex, SSL_read, SSL_peek_ex, SSL_peek, SSL_shutdown,
SSL_write_ex or SSL_write.

If the operation was successfully completed, do nothing.

If it is a harmless one (want read or want write), send data to be sent.

Raise error otherwise."
  (let ((ssl (client-ssl client))
        (wbio (client-wbio client)))
    (let ((err-code (ssl-get-error ssl ret)))
      (case err-code
        ((#.ssl-error-want-read #.ssl-error-want-write)
         (let* ((vec (make-shareable-byte-vector *default-buffer-size*)))
           (with-pointer-to-vector-data (buffer vec)
             (declare (dynamic-extent buffer))
             (move-encrypted-bytes wbio client buffer vec)
             (when (zerop (bio-test-flags wbio bio-flags-should-retry))
               (error "Should retry should be set.")))))
        (#.ssl-error-none nil)
        (#.ssl-error-ssl (process-ssl-errors))
        (t (cerror "Ignore" "SSL error ~d during write attempt" err-code))))))

(defun ssl-err-reason-error-string (code)
  (foreign-string-to-lisp (err-reason-error-string code)))

(defun process-ssl-errors ()
  (loop for err =  (err-get-error)
        until (zerop err)
        unless (position err #(#xa000412 )) ;bad certificate
          do
             (cerror "Ignore" 'ssl-error-condition :code err)))

(defun maybe-init-ssl (client)
  "If SSL is not initialized yet, initialize it"
  (when (zerop (ssl-is-init-finished (client-ssl client)))
    (do-io-if-wanted client (ssl-accept (client-ssl client)))))

(defun doubled-buffer (buffer)
  "Return a larger buffer with same initial data as the provided one."
  (let ((new (make-array (* 2 (length buffer))
                         :element-type '(unsigned-byte 8))))
    (replace new buffer)
    new))

(defun send-unencrypted-bytes (client new-data)
  "Collect new data to be encrypted and sent to client.

Data are just concatenated to the ENCRYPT-BUF."
  (let ((old-fp (client-encrypt-buf-size client))
        (buffer (client-encrypt-buf client)))
    (incf (client-encrypt-buf-size client) (length new-data))
    (when
        (> (client-encrypt-buf-size client) (length (client-encrypt-buf client)))
      (setf (client-encrypt-buf client) (doubled-buffer buffer)))
    (replace (client-encrypt-buf client) new-data :start1 old-fp)))

(defun encrypt-data (client)
  "Encrypt data in client's ENCRYPT-BUF.

Do nothing if no data to encrypt or SSL not yet initialized (and return zero).

Otherwise, use a temporary vector to write data, and update the buffer."
  (unless (or (zerop (client-encrypt-buf-size client))
              (zerop (ssl-is-init-finished (client-ssl client))))
    (let ((data (client-encrypt-buf client))
          (read-vector (make-shareable-byte-vector *default-buffer-size*)))
      (with-pointer-to-vector-data (read-buffer read-vector)
        (declare (dynamic-extent read-buffer))
        (with-pointer-to-vector-data (buffer data)
          (declare (dynamic-extent buffer))
          (loop
            with len = (client-encrypt-buf-size client)
              and all-written = 0
              and ssl = (client-ssl client)
            for written = (ssl-write ssl buffer len)
            for status = (ssl-get-error ssl written)
            do
               (cond
                 ((= len written)
                  (move-encrypted-bytes (client-wbio client)  client read-buffer read-vector)
                  (setf (client-encrypt-buf-size client) 0)
                  (return))
                 ((plusp written)
                  (incf all-written written)
                  (setf buffer (inc-pointer buffer written))
                  (decf len written)
                  (move-encrypted-bytes (client-wbio client) read-buffer client read-vector))
                 ((not (member status (list ssl-error-none ssl-error-want-write ssl-error-want-read)))
                  (error "SSL write failed, status ~d" status))
                 ((zerop written)
                  (update-encrypt-buf-ptr client all-written)
                  (return)))))))))

(defun queue-encrypted-bytes (client new-data)
  (setf (client-write-buf client)
        (append (client-write-buf client)
                (list new-data))))

(defun decrypt-socket-octets (client buffer size)
  "Process SIZE bytes received from the peer that are in the BUFFER.

Fed them into the SSL object to be unencrypted, and let the client callback
handle "
  (loop for written = (bio-write (client-rbio client) buffer size)
        when (minusp written) do (error "Bio-write failed")
          do
             (decf size written)
             (setf buffer (inc-pointer buffer written))
             (maybe-init-ssl client)
             (loop while
               (if (plusp (client-octets-needed client))
                   (on-complete-ssl-data client)
                   (funcall (client-io-on-read client) client)))
             (do-io-if-wanted client written)
        when (zerop size)
          do (return)))

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

(defvar *default-buffer-size* 64)

(defun process-data-on-socket (client)
  "Read data from client socket. If something is read (and it should, as this is
called when there should be a data), it is passed to the tls buffer and
decrypted."
  (with-foreign-object (buffer :char *default-buffer-size*)
    (loop
      for i from 0 to *max-read-chunks*
      for read = (read-2 (client-fd client) buffer *default-buffer-size*)
      while (plusp read)
      do (decrypt-socket-octets client buffer read)
         ;; todo: check also errno, do not rely on it being EAGAIN or EWOULDBLOCK.
      finally (when (zerop read) (signal 'done)))))

(defun concatenate* (vectors)
  (let* ((len (reduce #'+ vectors :key #'length))
         (res (make-array len :element-type '(unsigned-byte 8))))
    (loop for v of-type (simple-array (unsigned-byte 8)) in vectors
          with i = 0
          do
             (setf (subseq res i (+ i (length v))) v)
             (incf i (length v))
          finally (return res))))

(defun write-data-to-socket (client)
  "Write buffered encrypted data to the client socket. Update the write buffer to
keep what did not fit."
  (let ((concated (concatenate* (client-write-buf client))))
    (with-pointer-to-vector-data (buffer concated)
      (let ((n (write-2 (client-fd client) buffer
                        (length concated))))
        (setf (client-write-buf client)
              (cond ((= n (length concated))
                     nil)
                    ((plusp n)
                     (list (subseq concated n)))
                    (t (error "Write failed"))))))))

(defun process-client-fd (fd-ptr client)
  "Process events available on FD-PTR with CLIENT.

Read if you can, write if you can, announce DONE when done."
  (with-foreign-slots ((fd events revents) fd-ptr (:struct pollfd))
    (if (plusp (logand c-pollin revents))
        (process-data-on-socket client))
    (if (plusp (logand c-pollout revents))
        (write-data-to-socket client))
    (if (plusp (logand revents  (logior c-POLLERR  c-POLLHUP  c-POLLNVAL)))
        (signal 'done))))

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
    (ssl-set-accept-state (client-ssl client))
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

(defun update-encrypt-buf-ptr (client octets)
  "Remove OCTETS "
  (replace (client-encrypt-buf client)
           (client-encrypt-buf client)
           :start2 octets :end2 (client-encrypt-buf-size client))
  (decf (client-encrypt-buf-size client) octets))

(defun handle-client-io (client fdset)
  (let ((fd-ptr (inc-pointer fdset (* (client-fdset-idx client) size-of-pollfd))))
    (process-client-fd fd-ptr client)
    (encrypt-data client)
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

(defvar *clients* nil
  "List of clients processed.")

(defun process-new-client (fdset listening-socket ctx s-mem)
  "Add new client: accept connection, create client and add it to pollfd and to *clients*."
  (with-foreign-slots ((revents) fdset (:struct pollfd))
    (if (plusp (logand c-pollin revents))
        (let* ((socket (accept
                        (sb-bsd-sockets::socket-file-descriptor (usocket:socket listening-socket)) (null-pointer) 0))
               (client (make-client-object socket ctx s-mem)))
          (unless (zerop (fcntl socket f-setfl
                                (logior o-nonblock (fcntl socket f-getfl 0))))
            (error "Could not set O_NONBLOCK on client"))
          (add-socket-to-fdset fdset socket client)
          ;; maybe TODO: if no fdset slot available, stop reading listening socket
          (push client *clients*)))
    (if (plusp (logand revents  (logior c-POLLERR  c-POLLHUP  c-POLLNVAL)))
        (error "Error on listening socket"))))

(defun close-client-connection (fdset client)
  (setf *clients* (remove client *clients*))
  (when (client-ssl client)
    (ssl-free (client-ssl client)))     ; BIOs are closed automatically
  (setf (client-ssl client) nil)
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

(defmethod do-new-connection (socket (tls (eql :tls)) (dispatch-method (eql :async-custom)))
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
        (ssl (client-ssl client))
        (fn (client-io-on-read client))
        (octets (client-octets-needed client)))
    (with-pointer-to-vector-data (buffer vec)
      ;; TODO: check first with SSL_pending?
      (let ((read (ssl-read ssl buffer octets)))
        (cond
          ((not (plusp read)) nil)          ; just return and try again
          ((/= read octets)
           (error "Read ~d octets. This is not enough octets, why?~%~s~%" read (subseq vec 0 read)))
          (t (funcall fn client vec) t))))))

(defun process-client-hello (client vec)
  (cond ((equalp vec +client-preface-start+)
         (send-unencrypted-bytes client tls-server/mini-http2::*settings-frame*)
         (send-unencrypted-bytes client tls-server/mini-http2::*ack-frame*)
         (setf (client-io-on-read client) #'process-header
               (client-octets-needed client) 9))
        (t
         (error 'client-preface-mismatch :received vec))))

(defun process-header (client header)
  (let* ((frame-size (get-frame-size header))
         (type (get-frame-type header)))
    (declare ((unsigned-byte 8) type)
             (frame-size frame-size))
    (when (= type +goaway-frame-type+)
      ;; TODO: handle go-away frame
      )
    (unless (>= 16384 frame-size)
      (error  "Too big header! go-away"))
    (let ((id-to-process (get-stream-id-if-ends header)))
      (when id-to-process
        (send-unencrypted-bytes client
                                (buffer-with-changed-stream *header-frame* id-to-process))
        (send-unencrypted-bytes client
                                (buffer-with-changed-stream *data-frame* id-to-process))))
    (ignore-bytes client frame-size)))

(defun ignore-bytes (client count)
  (if (zerop count)
      (setf (client-octets-needed client) 9
            (client-io-on-read client) #'process-header)
      (setf (client-octets-needed client) 0
            (client-io-on-read client)
            (lambda (client)
              (let ((vec (make-shareable-byte-vector count)))
                (with-pointer-to-vector-data (buffer vec)
                  (let ((read (ssl-read (client-ssl client) buffer count)))
                    (decf count read)
                    (when (zerop count)
                      (setf (client-io-on-read client) #'process-header
                            (client-octets-needed client) 9)
                      (on-complete-ssl-data client))))))))
  t)

;;;; version with async
(defmethod do-new-connection (socket (tls (eql :v3)) (dispatch-method (eql :async)))
  "Handle new connections using cl-async event loop.

Pros: This version can be run in one thread and process many clients.

Cons: requires a specific C library, and the implementation as-is depends on
SBCL internal function - we re-use the file descriptor of socket created by
usocket package, as otherwise access to the port of server is complicated."
  (let ((ctx (make-ssl-context))
        (s-mem (bio-s-mem)))
    (cl-async:start-event-loop
     (lambda ()
       (cl-async:tcp-server nil nil
                            (lambda (socket bytes)
                              (with-pointer-to-vector-data (p bytes)
                                (decrypt-socket-octets (cl-async:socket-data socket)
                                                       p
                                                       (length bytes))
                                (let* ((client (cl-async:socket-data socket))
                                       (concated (concatenate* (client-write-buf client))))
                                  (cl-async:write-socket-data  socket concated))))
                            :connect-cb (lambda (socket)
                                          (setf (cl-async:socket-data socket)
                                                (make-client-object socket ctx s-mem)))
                            :event-cb (lambda (err) (format t "--> ~s~%" err))
                            :fd (sb-bsd-sockets:socket-file-descriptor
                                 (usocket:socket socket))))))
  (invoke-restart 'kill-server))
