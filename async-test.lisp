(require 'cffi)
(require 'usocket)
(require 'cffi-grovel)

(in-package #:tls-test/async/tls)

(defcfun "BIO_s_mem" :pointer)
(defcfun "BIO_new" :pointer (bio-method :pointer))

(defcfun "BIO_write" :int (bio-method :pointer) (data :pointer) (dlen :int))
(defcfun "BIO_read" :int (bio-method :pointer) (data :pointer) (dlen :int))

(defcfun "BIO_should_retry" :int (bio-method :pointer))


(defcfun "SSL_new" :pointer (bio-method :pointer))
(defcfun "SSL_read" :int (ssl :pointer) (buffer :pointer) (bufsize :int))
(defcfun "SSL_get_error" :int (ssl :pointer) (ret :int))
(defcfun "SSL_is_init_finished" :int (ssl :pointer))
(defcfun "OpenSSL_add_all_algorithms" :void) ; FIXME: obsolete
(defcfun "SSL_CTX_new" :pointer (method :pointer))
(defcfun "TLS_method" :pointer)
(defcfun "SSL_CTX_use_certificate_file" :int (ctx :pointer) (path :pointer) (type :int))
(defcfun "SSL_CTX_use_PrivateKey_file" :int (ctx :pointer) (path :pointer) (type :int))
(defcfun "SSL_CTX_check_private_key" :int (ctx :pointer))

(defcfun "SSL_set_accept_state" :pointer (ssl :pointer))
(defcfun "SSL_library_init" :int)
(defcfun "SSL_accept" :int (ssl :pointer))
(defcfun "SSL_set_bio" :pointer (ssl :pointer) (rbio :pointer) (wbio :pointer))
(defcfun "SSL_CTX_set_options" :int (ctx :pointer) (options :int))
(defcfun "SSL_CTX_set_min_proto_version" :int (ctx :pointer) (options :int))

(defcfun "poll" :int (fdset :pointer) (rb :int) (timeout :int))
(defcfun ("read" read-2) :int (fd :int) (buf :pointer) (size :int))
(defcfun ("write" write-2) :int (fd :int) (buf :pointer) (size :int))


(defstruct client fd ssl rbio wbio write-buf encrypt-buf io-on-read)

(defun make-ssl-context ()
  (ssl-library-init)
  (openssl-add-all-algorithms)
  (let ((context (ssl-ctx-new (tls-method))))
    (when (null-pointer-p context )
      (error "Could not create context"))
    (unless (= 1 (ssl-ctx-use-certificate-file
                  context
                  "/home/zellerin/projects/tls-server/certs/server.key"
                  ssl-filetype-pem))
      (error "failed to load server private key"))
    (unless (= 1 (ssl-ctx-use-privatekey-file
                  context
                  "/home/zellerin/projects/tls-server/certs/server.cert"
                  ssl-filetype-pem))
      (error "failed to load server certificate"))

    (unless (= 1 (ssl-ctx-check-private-key context))
      (error "server private/public key mismatch"))
    (ssl-ctx-set-options context ssl-op-all)
    (ssl-ctx-set-min-proto-version context tls-1.2-version)
    context))

(defun print-unencrypted-data (data)
  (break "~a" data))

#+unused-yet(defun do-one-fd-item (fd-pointer client)
  "Return pointer to next fd item."
  (cffi:with-foreign-slots ((fd events revents)
                            fdset
                            (:struct pollfd))
    (when (plusp (logand revents c-pollin))
      (data-available-callback client))
    (cffi:inc-pointer fdset size-of-pollfd)
    ))

#+nil(cffi:defcfun ("on_read_cb" on-read-cb) :int (buf :pointer) (size :int))

(defun maybe-init-ssl (client)
  (when (zerop (ssl-is-init-finished (client-ssl client)))
    (do-io-if-wanted client (ssl-accept (client-ssl client)))))

(defun queue-encrypted-bytes (client new-data)
  (setf (client-write-buf client)
        (concatenate '(vector (unsigned-byte 8))
                      (client-write-buf client)
                      new-data)))

(defun process-decrypted-bytes (client)
  (let ((vec (cffi:make-shareable-byte-vector default-buf-size)))
    (cffi:with-pointer-to-vector-data (buffer vec)
      (let ((read (ssl-read (client-ssl client) buffer default-buf-size)))
        (when (plusp read)
                                        ;          (funcall (client-io-on-read client) vec read)
          (format t "Got ~s" (subseq vec 0 read))
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
                  (when (and nil (zerop (bio-should-retry (client-wbio client))))
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

             (process-decrypted-bytes client)
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
    (format t "revents: ~x~%" revents)
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
                                        :io-on-read #'print-unencrypted-data)))
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
                  (format t "nread: ~a~%" nread)
                  (unless (zerop nread)
                    (process-fd-1 (cffi:inc-pointer fdset size-of-pollfd) client)
                    (process-fd-0 fdset)))))))))))
