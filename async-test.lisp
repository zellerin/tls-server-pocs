(require 'cffi)
(require 'usocket)
(cffi:define-foreign-library myssl
  (:unix "/home/zellerin/projects/tls-server/ssl_server.so"))

(cffi:use-foreign-library myssl)
(cffi:defcfun "serve" :int (port :int))

(let ((host "0.0.0.0") (port 8443))
  (usocket:with-socket-listener (listening-socket host port
                                                  :reuse-address t
                                                  :element-type '(unsigned-byte 8))
#+nil    (funcall announce-url-callback (url-from-socket listening-socket host tls))
    (serve (sb-bsd-sockets:socket-file-descriptor
            (usocket:socket listening-socket)))))
