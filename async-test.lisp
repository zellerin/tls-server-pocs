(require 'cffi)
(require 'usocket)
(require 'cffi-grovel)

(in-package #:tls-server/async/tls)

(define-foreign-library openssl (:unix "libssl.so"))
(use-foreign-library openssl)

(defcfun "BIO_new" :pointer (bio-method :pointer))
(defcfun "BIO_free" :pointer (bio :pointer))
(defcfun "BIO_read" :int (bio-method :pointer) (data :pointer) (dlen :int))
(defcfun "BIO_s_mem" :pointer)
(defcfun "BIO_test_flags" :int (bio :pointer) (what :int))
(defcfun "BIO_write" :int (bio-method :pointer) (data :pointer) (dlen :int))

(defcfun "SSL_CTX_check_private_key" :int (ctx :pointer))
(defcfun "SSL_CTX_ctrl" :int (ctx :pointer) (cmd :int) (value :long) (args :pointer))
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

(mgl-pax:defsection  @async-server
    (:title "Asynchronous TLS server")
  (client type)
  (make-ssl-context function))

(defstruct client
  "Data of one client connection. This includes:

- File desriptor (FD),
- SSL data opaque pointer (SSL),
- Input and output BIO for exchanging data with OPENSSL (RBIO, WBIO),
- Unencrypted octets to encrypt and send (WRITE-BUF),
- Encrypted octets to send to the file descriptor (ENCRYPT-BUF),
- Callback function when read data are available (IO-ON-READ)."
  fd ssl rbio wbio write-buf encrypt-buf io-on-read fdset-idx)

(defun make-ssl-context ()
  "Make a SSL context.

This includes public and private key pair (from files in this directory),

FIXME: We should not need both this and MINI-HTTP2:MAKE-HTTP2-TLS-CONTEXT"
  (let ((context (ssl-ctx-new (tls-method)))
        (*default-pathname-defaults*
          (asdf:component-pathname (asdf:find-system "tls-server"))))
    (when (null-pointer-p context)
      (error "Could not create context"))
    (with-foreign-string (file (namestring (merge-pathnames #P"certs/server.crt")))
      (unless (= 1 (ssl-ctx-use-certificate-file context file ssl-filetype-pem))
        (error "failed to load server certificate")))
    (with-foreign-string (file (namestring (merge-pathnames "certs/server.key")))
      (unless (= 1 (ssl-ctx-use-privatekey-file
                    context file ssl-filetype-pem))
        (error "failed to load server private key")))
    (unless (= 1 (ssl-ctx-check-private-key context))
      (error "server private/public key mismatch"))
    (ssl-ctx-set-options context ssl-op-all)
    (ssl-ctx-ctrl context ssl-ctrl-set-min-proto-version tls-1.2-version (null-pointer))
    (mini-http2::ssl-ctx-set-alpn-select-cb  context (get-callback 'mini-http2::select-h2-callback))
    context))

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
    (case (ssl-get-error ssl ret)
      ((#.ssl-error-want-read #.ssl-error-want-write)
       (let* ((vec (make-shareable-byte-vector *default-buffer-size*)))
         (with-pointer-to-vector-data (buffer vec)
           (declare (dynamic-extent buffer))
           (move-encrypted-bytes wbio client buffer vec)
           (when (zerop (bio-test-flags wbio bio-flags-should-retry))
             (error "Should retry should be set.")))))
      (#.ssl-error-none nil)
      (t (error "Fail write: SSL error code is unknown")))))

(defun maybe-init-ssl (client)
  "If SSL is not initialized yet, initialize it"
  (when (zerop (ssl-is-init-finished (client-ssl client)))
    (do-io-if-wanted client (ssl-accept (client-ssl client)))))

(defun send-unencrypted-bytes-v1 (client new-data)
  "Process new data to be encrypted and sent to client.

Simpler variant - data are just concatenated to the ENCRYPT-BUF. Someone later,
they would be encrypted and passed."
  (setf (client-encrypt-buf client)
        (concatenate '(vector (unsigned-byte 8))
                     (client-encrypt-buf client)
                     new-data)))

(defun send-unencrypted-bytes-v2 (client new-data)
  "Process new data to be encrypted and sent to client.

Pass for encoding everything in the queue and new data. Set the buffer to the
data that were not accepted for encoding yet."
  (let ((data-to-send (client-encrypt-buf client)))
    (declare ((simple-array (unsigned-byte 8)) new-data)
             ((or null (simple-array (unsigned-byte 8))) new-data data-to-send)
             (optimize (safety 0) speed))
    (let ((old-remaining
            (when (client-encrypt-buf client)
              (do-encrypt client data-to-send))))
      (setf (client-encrypt-buf client)
            (if old-remaining
                (concatenate '(vector (unsigned-byte 8))
                             old-remaining new-data)
                (do-encrypt client new-data))))))

(defvar *unencrypted-sender* 'send-unencrypted-bytes-v1
  "Default function to send unencrypted data.")

(defun send-unencrypted-bytes (client new-data)
  "Process new data to be encrypted and sent to client."
  (funcall *unencrypted-sender* client new-data))

(defun do-encrypt (client data)
  "Encrypt data in client's ENCRYPT-BUF.

Do nothing if no data to encrypt or SSL not yet initialized.

Otherwise, use a temporary vector to write data "
  (unless (or (null data)
              (zerop (ssl-is-init-finished (client-ssl client))))
    (let ((read-vector (make-shareable-byte-vector *default-buffer-size*)))
      (with-pointer-to-vector-data (read-buffer read-vector)
        (declare (dynamic-extent read-buffer))
        (with-pointer-to-vector-data (buffer data)
          (declare (dynamic-extent buffer))
          (loop
            with len = (length data)
            and all-written = 0
            and ssl = (client-ssl client)
            for written = (ssl-write ssl buffer len)
            for status = (ssl-get-error ssl written)
            do
               (cond
                 ((= len written)
                  (move-encrypted-bytes (client-wbio client)  client read-buffer read-vector)
                  (return nil))
                 ((plusp written)
                  (incf all-written written)
                  (setf buffer (inc-pointer buffer written))
                  (decf len written)
                  (move-encrypted-bytes (client-wbio client) read-buffer client read-vector))
                 ((not (member status (list ssl-error-none ssl-error-want-write ssl-error-want-read)))
                  (error "SSL write failed, status ~d" status))
                 ((zerop written)
                  (return (subseq data all-written))))))))))

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
             (funcall (client-io-on-read client) client)
             (do-io-if-wanted client written)
        when (zerop size)
          do (return)))

(define-condition done (error)
  ()
  (:documentation "The socket on the other side is closed."))

(defvar *default-buffer-size* 64)

(defun do-sock-read (client)
  "Read data from client socket. If something is read (and it should, as this is called when there should be a data), "
  (with-foreign-object (buffer :char *default-buffer-size*)
    (let ((read (read-2 (client-fd client) buffer *default-buffer-size*)))
      (if (plusp read)
          (decrypt-socket-octets client buffer read)
          (signal 'done)))))

(defun concatenate* (vectors)
  (let* ((len (reduce #'+ vectors :key #'length))
         (res (make-array len :element-type '(unsigned-byte 8))))
    (loop for v of-type (simple-array (unsigned-byte 8)) in vectors
          with i = 0
          do
             (setf (subseq res i (+ i (length v))) v)
             (incf i (length v))
          finally (return res))))

(defun do-sock-write (client)
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
        (do-sock-read client))
    (if (plusp (logand c-pollout revents))
        (do-sock-write client))
    (if (plusp (logand revents  (logior c-POLLERR  c-POLLHUP  c-POLLNVAL)))
        (signal 'done))))

(defvar *fdset-size* 10)
(defvar *empty-fdset-items* (loop for i from 0 to (1- *fdset-size*) collect i))

(defun make-client-object (socket ctx s-mem)
  "Make a new CLIENT object"
  (let* ((client (make-client :fd socket
                              :rbio (bio-new s-mem)
                              :wbio (bio-new s-mem)
                              :ssl (ssl-new ctx)
                              :io-on-read #'process-client-hello)))
    (ssl-set-accept-state (client-ssl client))
    (ssl-set-bio (client-ssl client) (client-rbio client) (client-wbio client))
    client))

(defun add-socket-to-fdset (fdset socket client)
  "Find free slot in the FDSET and put client there."
  (let ((fd-idx (pop *empty-fdset-items*)))
    (with-foreign-slots ((fd events)
                         (inc-pointer fdset (* fd-idx size-of-pollfd))
                         (:struct pollfd))
      (setf fd socket events (logior c-pollerr c-pollhup c-pollnval c-pollin)))
    (setf (client-fdset-idx client) fd-idx)))

(defun init-fdset (fdset size)
  (loop for i from 0 to (1- size)
        do
           (with-foreign-slots ((fd events) fdset (:struct pollfd))
             (setf fd -1))
           (setf fdset (inc-pointer fdset size-of-pollfd))))

(defun handle-client-io (client fdset)
  (let ((fd-ptr (inc-pointer fdset (* (client-fdset-idx client) size-of-pollfd))))
    (process-client-fd fd-ptr client)
    (when (client-encrypt-buf client)
      (setf (client-encrypt-buf client)
            (do-encrypt client (client-encrypt-buf client))))
    (with-foreign-slots ((events)
                         fd-ptr
                         (:struct pollfd))
      (setf events
            (if (client-write-buf client)
                (logior events c-pollout)
                (logand events (logxor -1 c-pollout)))))))

(defun serve-tls (&optional (host "0.0.0.0") (port 8443))
  (usocket:with-socket-listener (listening-socket host port
                                                  :reuse-address t
                                                  :element-type '(unsigned-byte 8))
    #+nil    (funcall announce-url-callback (url-from-socket listening-socket host tls))
    (with-foreign-object (fdset '(:struct pollfd) *fdset-size*)
      (init-fdset fdset *fdset-size*)
      (let* ((s-mem (bio-s-mem))
             (ctx (make-ssl-context))
             (clients nil)
             (*empty-fdset-items* (loop for i from 1 to (1- *fdset-size*) collect i)))
        (with-foreign-slots ((fd events) fdset (:struct pollfd))
          (setf fd (sb-bsd-sockets:socket-file-descriptor (usocket:socket listening-socket))
                events (logior c-pollerr c-pollhup c-pollnval c-pollin)))

        (loop
          (let ((nread (poll fdset *fdset-size* -1)))
            (with-foreign-slots ((fd events revents) fdset (:struct pollfd))
              (if (plusp (logand c-pollin revents))
                  (let* ((socket (sb-bsd-sockets:socket-file-descriptor
                                  (usocket:socket
                                   (usocket:socket-accept listening-socket :element-type
                                                          '(unsigned-byte 8)))))
                         (client (make-client-object socket ctx s-mem)))
                    (add-socket-to-fdset fdset socket client)
                    (push client clients)))
              (if (plusp (logand revents  (logior c-POLLERR  c-POLLHUP  c-POLLNVAL)))
                  (error "Error on listening socket")))
            (unless (zerop nread)
              (dolist (client clients)
                (handler-case
                    (handle-client-io client fdset)
                  (done ()
                    (ssl-free (client-ssl client))
                    (push (client-fdset-idx client) *empty-fdset-items*)
                    (with-foreign-slots ((fd events)
                                         (inc-pointer fdset
                                                      (* (client-fdset-idx client) size-of-pollfd))
                                         (:struct pollfd))
                      (setf fd -1))
                    (setf clients (remove client clients))
                    (close-fd (client-fd client))))))))))))

;;;; HTTP2 TLS async client
(defun process-client-hello (client)
  (let ((vec (make-shareable-byte-vector +client-preface-length+)))
    (with-pointer-to-vector-data (buffer vec)
      (let ((read (ssl-read (client-ssl client) buffer +client-preface-length+)))
        (cond
          ((not (plusp read)))          ; just return and try again
          ((/= read +client-preface-length+)
           (error "Read ~d octets. This is not enough octets for the client preface (or preface split, why?~%~s~%" read (subseq vec 0 read)))
          ((equalp vec +client-preface-start+)
           (send-unencrypted-bytes client mini-http2::*settings-frame*)
           (send-unencrypted-bytes client mini-http2::*ack-frame*)
           (setf (client-io-on-read client) #'process-header))
          (t
           (error "Client preface incorrect. Does client send http2?~%~s~%" vec)))))))

(defun process-header (client)
  (let ((header (make-shareable-byte-vector 9))) ; header size
    (with-pointer-to-vector-data (buffer header)
      (let ((read (ssl-read (client-ssl client) buffer 9)))
        (cond
          ((not (plusp read))) ; just return and try again
          ((/= read 9)
           (error "Read ~d octets. This is not enough for header." read))
          (t
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
             (setf (client-io-on-read client) (ignore-bytes frame-size))
             (funcall (client-io-on-read client) client))))))))

(defun ignore-bytes (count)
  (if (zerop count)
      #'process-header
      (lambda (client)
        (let ((vec (make-shareable-byte-vector count)))
          (with-pointer-to-vector-data (buffer vec)
            (let ((read (ssl-read (client-ssl client) buffer count)))
              (decf count read)
              (when (zerop count)
                (setf (client-io-on-read client) #'process-header)
                (process-header client))))))))
