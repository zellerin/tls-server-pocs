(require 'cffi)
(require 'usocket)
(require 'cffi-grovel)

(in-package #:tls-test/async/tls)

(define-foreign-library openssl
  (:unix "libssl.so"))

(use-foreign-library openssl)

(defcfun "BIO_s_mem" :pointer)
(defcfun "BIO_new" :pointer (bio-method :pointer))

(defcfun "BIO_write" :int (bio-method :pointer) (data :pointer) (dlen :int))
(defcfun "BIO_read" :int (bio-method :pointer) (data :pointer) (dlen :int))

(defcfun "BIO_should_retry" :int (bio-method :pointer))


(defcfun "SSL_new" :pointer (bio-method :pointer))
(defcfun "SSL_read" :int (ssl :pointer) (buffer :pointer) (bufsize :int))
(defcfun "SSL_write" :int (ssl :pointer) (buffer :pointer) (bufsize :int))
(defcfun "SSL_get_error" :int (ssl :pointer) (ret :int))
(defcfun "SSL_is_init_finished" :int (ssl :pointer))
(defcfun "OpenSSL_add_all_algorithms" :void) ; FIXME: obsolete
(defcfun "SSL_CTX_new" :pointer (method :pointer))
(defcfun "TLS_method" :pointer)
(defcfun "SSL_CTX_use_certificate_file" :int (ctx :pointer) (path :string) (type :int))
(defcfun "SSL_CTX_use_PrivateKey_file" :int (ctx :pointer) (path :string) (type :int))
(defcfun "SSL_CTX_check_private_key" :int (ctx :pointer))

(defcfun "SSL_set_accept_state" :pointer (ssl :pointer))
;(defcfun "SSL_library_init" :int) no longer needed
(defcfun "SSL_accept" :int (ssl :pointer))
(defcfun "SSL_set_bio" :pointer (ssl :pointer) (rbio :pointer) (wbio :pointer))
(defcfun "SSL_CTX_set_options" :int (ctx :pointer) (options :uint))
(defcfun "SSL_CTX_ctrl" :int (ctx :pointer) (cmd :int) (value :long) (args :pointer))

(defcfun "poll" :int (fdset :pointer) (rb :int) (timeout :int))
(defcfun ("read" read-2) :int (fd :int) (buf :pointer) (size :int))
(defcfun ("write" write-2) :int (fd :int) (buf :pointer) (size :int))


(defstruct client fd ssl rbio wbio write-buf encrypt-buf io-on-read)

(defun make-ssl-context ()
  (let ((context (ssl-ctx-new (tls-method))))
    (when (null-pointer-p context)
      (error "Could not create context"))
    (with-foreign-string (file "/home/zellerin/projects/tls-server/certs/server.crt")
      (unless (= 1 (ssl-ctx-use-certificate-file context file ssl-filetype-pem))
        (error "failed to load server certificate")))
    (with-foreign-string (file "/home/zellerin/projects/tls-server/certs/server.key")
      (unless (= 1 (ssl-ctx-use-privatekey-file
                    context file ssl-filetype-pem))
        (error "failed to load server private key")))
    (unless (= 1 (ssl-ctx-check-private-key context))
      (error "server private/public key mismatch"))
    (ssl-ctx-set-options context ssl-op-all)
    (ssl-ctx-ctrl context ssl-ctrl-set-min-proto-version tls-1.2-version (null-pointer))
    (mini-http2::ssl-ctx-set-alpn-select-cb  context (cffi:get-callback 'mini-http2::select-h2-callback))
    context))

(defun maybe-init-ssl (client)
  (when (zerop (ssl-is-init-finished (client-ssl client)))
    (do-io-if-wanted client (ssl-accept (client-ssl client)))))

(defun send-unencrypted-bytes (client new-data)
  (setf (client-encrypt-buf client)
        (concatenate '(vector (unsigned-byte 8))
                     (client-encrypt-buf client)
                     new-data)))

(defun do-encrypt (client)
  (when (and (not (zerop (ssl-is-init-finished (client-ssl client))))
             (client-encrypt-buf client))
    (let ((len (length (client-encrypt-buf client)))
          (all-written 0)
          (read-vector (make-shareable-byte-vector default-buf-size)))
      (with-pointer-to-vector-data (read-buffer read-vector)
        (with-pointer-to-vector-data (buffer (client-encrypt-buf client))
          (loop
            (let* ((written (ssl-write (client-ssl client) buffer len))
                   (status (ssl-get-error (client-ssl client) written)))
              (cond
                ((not (member status (list ssl-error-none ssl-error-want-write ssl-error-want-read)))
                 (error "SSL write failed"))
                ((zerop written)
                 (setf (client-encrypt-buf client)
                       (subseq (client-encrypt-buf client) all-written))
                 (return))
                ((plusp written)
                 (incf all-written written)
                 (inc-pointer buffer written)
                 (decf len written)
                 (loop
                   for n = (bio-read (client-wbio client) read-buffer default-buf-size)
                   while (plusp n)
                   do (queue-encrypted-bytes client (subseq read-vector 0 n)))
                 (when (zerop len)
                   (setf (client-encrypt-buf client) nil)
                   (return)))))))))))

(defun queue-encrypted-bytes (client new-data)
  (setf (client-write-buf client)
        (concatenate '(vector (unsigned-byte 8))
                     (client-write-buf client)
                     new-data)))

#+unused(defun process-decrypted-bytes (client)
  (let ((vec (cffi:make-shareable-byte-vector default-buf-size)))
    (cffi:with-pointer-to-vector-data (buffer vec)
      (let ((read (ssl-read (client-ssl client) buffer default-buf-size)))
        (when (plusp read)
          (funcall (client-io-on-read client) vec read)
          (process-decrypted-bytes client))))))

(defun do-io-if-wanted (client ret)
  (case (ssl-get-error (client-ssl client) ret)
    ((#.ssl-error-want-read #.ssl-error-want-write)
     (let* ((vec (make-shareable-byte-vector default-buf-size)))
       (with-pointer-to-vector-data (buffer vec)
         (loop for n = (bio-read (client-wbio client) buffer default-buf-size)
               while (plusp n)
               do (queue-encrypted-bytes client (subseq vec 0 n))
               finally
                  (when (and nil
                             (zerop (bio-should-retry (client-wbio client))))
                    (error "Should retry should be set."))))))
    (#.ssl-error-none nil)
    (t (error "Fail write: SSL error code is unknown"))))

(defun on-read-cb (client buffer size)
  "Process SSL bytes received from the peer. The data needs to be fed into the
SSL object to be unencrypted."

  ;; TODO: rewrite from C-in-Lisp to Lisp
  (loop for n = (bio-write (client-rbio client) buffer size)
        when (minusp n) do (error "Bio-write failed")
          do
             (decf size n)
             (setf buffer (cffi:inc-pointer buffer n))
             (maybe-init-ssl client)
             (funcall (client-io-on-read client) client)
             ;; The encrypted data is now in the input bio so now we can perform actual
             ;; read of unencrypted data.

             (do-io-if-wanted client n)
        when (zerop size)
          do (return)))

(define-condition done (error)
  ())

(defconstant default-buf-size 64)

(defun do-sock-read (client)
  (cffi:with-foreign-object (buffer :char default-buf-size)
    (let ((read (read-2 (client-fd client) buffer default-buf-size)))
      (if (plusp read)
          (on-read-cb client buffer read)
          (error 'done)))))

(defun do-sock-write (client)
  (with-pointer-to-vector-data (buffer (client-write-buf client))
    (let ((n (write-2 (client-fd client) buffer
                      (length (client-write-buf client)))))
      (cond ((= n (length (client-write-buf client)))
             (setf (client-write-buf client) nil))
            ((plusp n) (setf (client-write-buf client)
                             (subseq (client-write-buf client) n)))
            (t (error "Write failed"))))))

(defun process-fd-1 (fd-ptr client)
  "Process events available on FD 1 (client).

The FD-PTR points to the field of the client; we use only revents of it."
  (cffi:with-foreign-slots ((fd events revents) fd-ptr (:struct pollfd))
    (if (plusp (logand c-pollin revents))
        (do-sock-read client))
    (if (plusp (logand c-pollout revents))
        (do-sock-write client))
    (if (plusp (logand revents  (logior c-POLLERR  c-POLLHUP  c-POLLNVAL)))
        (error 'done))))

(defun do-terminal-read (fd)
  (break "Terminal read: ~a" fd))

(defun process-fd-0 (fd-ptr)
  (cffi:with-foreign-slots ((fd events revents) fd-ptr (:struct pollfd))
    (if (plusp (logand c-pollin revents))
        (do-terminal-read fd-ptr))
    (if (plusp (logand revents  (logior c-POLLERR  c-POLLHUP  c-POLLNVAL)))
        (error 'done))))

(defun serve-tls (&optional (host "0.0.0.0") (port 8443))
  (usocket:with-socket-listener (listening-socket host port
                                                  :reuse-address t
                                                  :element-type '(unsigned-byte 8))
    #+nil    (funcall announce-url-callback (url-from-socket listening-socket host tls))
    (cffi:with-foreign-object (fdset '(:struct pollfd) 2)
      (cffi:with-foreign-slots ((fd events) fdset (:struct pollfd))
        (setf fd 0
              events c-pollin))
      (let* ((s-mem (bio-s-mem))
             (ctx (make-ssl-context)))
        (loop
          (usocket:with-connected-socket (plain (usocket:socket-accept listening-socket
                                                                       :element-type '(unsigned-byte 8)))
            (let* ((socket  (sb-bsd-sockets:socket-file-descriptor
                             (usocket:socket plain)))
                   (client (make-client :fd socket
                                        :rbio (bio-new s-mem)
                                        :wbio (bio-new s-mem)
                                        :ssl (ssl-new ctx)
                                        :io-on-read #'process-client-hello)))
              (ssl-set-accept-state (client-ssl client))
              (ssl-set-bio (client-ssl client) (client-rbio client) (client-wbio client))

              (cffi:with-foreign-slots ((fd events)
                                        (cffi:inc-pointer fdset size-of-pollfd)
                                        (:struct pollfd))
                (setf fd socket events (logior c-pollerr c-pollhup c-pollnval c-pollin)))

              (loop
                (cffi:with-foreign-slots ((fd events revents)
                                          (cffi:inc-pointer fdset size-of-pollfd)
                                          (:struct pollfd))
                  (setf events (logand events (logxor -1 c-pollout)))
                  (when (client-write-buf client)
                    (setf events (logior events c-pollout))))
                (let ((nread (poll fdset 2 -1)))
                  (unless (zerop nread)
                    (process-fd-1 (cffi:inc-pointer fdset size-of-pollfd) client)
                    (process-fd-0 fdset)
                    (do-encrypt client)))))))))))

;;;; HTTP2 TLS async client

(defun process-client-hello (client)
  (let ((vec (cffi:make-shareable-byte-vector +client-preface-length+)))
    (cffi:with-pointer-to-vector-data (buffer vec)
      (let ((read (ssl-read (client-ssl client) buffer +client-preface-length+)))
        (cond
          ((not (plusp read))) ; just return and try again
          ((/= read +client-preface-length+)
           (error "Read ~d octets. This is not enough octets for the client preface (or preface split, why?~%~s~%" read (subseq vec 0 read)))
          ((equalp vec +client-preface-start+)
           (send-unencrypted-bytes client mini-http2::*settings-frame*)
           (send-unencrypted-bytes client mini-http2::*ack-frame*)
           (setf (client-io-on-read client) #'process-header))
          (t
           (error "Client preface incorrect. Does client send http2?~%~s~%" vec)))))))

(defun process-header (client)
  (let ((header (cffi:make-shareable-byte-vector 9))) ; header size
    (cffi:with-pointer-to-vector-data (buffer header)
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
               (cerror "OK" "got goaway frame, leaving~%~s~%" header))
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
          (cffi:with-pointer-to-vector-data (buffer vec)
            (let ((read (ssl-read (client-ssl client) buffer count)))
              (decf count read)
              (when (zerop count)
                (setf (client-io-on-read client) #'process-header)
                (process-header client))))))))
