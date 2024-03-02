(require 'cffi)
(require 'usocket)
(require 'cffi-grovel)

(cffi:define-foreign-library myssl
  (:unix "/home/zellerin/projects/tls-server/ssl_server.so"))

(cffi:use-foreign-library myssl)

(load (cffi-grovel:process-grovel-file "/home/zellerin/projects/tls-server/async-grovel.lisp" "/tmp/foo.lisp"))

(cffi:defcfun "serve" :int (fdset (:pointer (:struct pollfd))) (port :int))

(cffi:defcfun "BIO_s_mem" :pointer)
(cffi:defcfun "BIO_new" :pointer (bio-method :pointer))

(cffi:defcfun "SSL_new" :pointer (bio-method :pointer))

(cffi:defcfun "make_ssl_context" :pointer)

(cffi:defcfun "SSL_set_accept_state" :pointer (ssl :pointer))
(cffi:defcfun "SSL_set_bio" :pointer (ssl :pointer) (rbio :pointer) (wbio :pointer))

(cffi:defcfun "poll" :int (fdset :pointer) (rb :int) (timeout :int))
(cffi:defcfun ("read" read-2) :int (fd :int) (buf :pointer) (size :int))
(cffi:defcfun ("write" write-2) :int (fd :int) (buf :pointer) (size :int))


(defstruct client fd ssl rbio wbio write-buf encrypt-buf io-on-read)

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

(define-condition done (error)
  ())

(defconstant default-buf-size 64)

(defun on-read-cb )

(defun do_sock_read (client)
  (cffi:with-foreign-object (buffer :char default-buf-size)
    (let ((read (read-2 (client-fd client) buffer default-buf-size)))
      (if (plusp read)
          (on-read-cb buffer read)
          (error 'done)))))


(defun process-fd-1 (fd-ptr client)
  (cffi:with-foreign-slots ((fd events revents) fd-ptr (:struct pollfd))
    (if (plusp (logand c-pollin revents))
        (do-sock-read client))
    (if (plusp (logand c-pollout revents))
        (if (= -1 (do-sock-write)) (error 'done)))
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
                (let ((nready (poll fdset 2 -1)))
                  (unless (zerop nread)
                    (process-fd-1 (cffi:inc-pointer fdset size-of-pollfd) client)
                    (process-fd-0 fdset)))
                )

              (serve fdset socket))))))))
