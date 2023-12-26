(require 'cffi)
(require 'usocket)
(cffi:define-foreign-library myssl
  (:unix "/home/zellerin/projects/tls-server/ssl_server.so"))

(cffi:use-foreign-library myssl)
(cffi:defcfun "serve" :int (port :int))
(cffi:defcfun "ssl_init" :void)

(ssl-init)
(let ((host "0.0.0.0") (port 8443))
  (usocket:with-socket-listener (listening-socket host port
                                                  :reuse-address t
                                                  :element-type '(unsigned-byte 8))
    #+nil    (funcall announce-url-callback (url-from-socket listening-socket host tls))
    (loop
      (usocket:with-connected-socket (plain (usocket:socket-accept listening-socket
                                                                   :element-type '(unsigned-byte 8)))
        (serve (sb-bsd-sockets:socket-file-descriptor
                (usocket:socket plain)))))))
