(require 'cffi)
(require 'usocket)
(require 'cffi-grovel)

(cffi:define-foreign-library myssl
  (:unix "/home/zellerin/projects/tls-server/ssl_server.so"))

(cffi:use-foreign-library myssl)

(load (cffi-grovel:process-grovel-file
            (merge-pathnames  "async-grovel" *load-pathname*)))

(cffi:defcfun "serve" :int (fdset (:pointer (:struct pollfd))) (port :int))
(cffi:defcfun "ssl_init" :void)


(ssl-init)
(let ((host "0.0.0.0") (port 8443))
  (usocket:with-socket-listener (listening-socket host port
                                                  :reuse-address t
                                                  :element-type '(unsigned-byte 8))
    #+nil    (funcall announce-url-callback (url-from-socket listening-socket host tls))
    (cffi:with-foreign-object (fdset '(:struct pollfd) 2)
      (cffi:with-foreign-slots ((fd events) fdset (:struct pollfd))
        (setf fd 0
              events c-pollin))
      (loop
        (usocket:with-connected-socket (plain (usocket:socket-accept listening-socket
                                                                     :element-type '(unsigned-byte 8)))
          (let ((socket  (sb-bsd-sockets:socket-file-descriptor
                          (usocket:socket plain))))

            (cffi:with-foreign-slots ((fd events)
                                      (cffi:inc-pointer fdset size-of-pollfd)
                                      (:struct pollfd))
              (setf fd socket events (logior c-pollerr c-pollhup c-pollnval c-pollin)))

            (serve fdset socket)))))))
