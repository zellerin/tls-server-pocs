;;;; tls-server.asd
;;
;;;; Copyright (c) 2023 Tomáš Zellerin <tomas@zellerin.cz>


(asdf:defsystem #:tls-server
  :description "Various implementations of toy HTTP2 server."
  :author "Tomáš Zellerin <tomas@zellerin.cz>"
  :license  "MIT"
  :version "0.9"
  :serial t
  :in-order-to ((asdf::test-op (asdf:test-op "tls-server/test")))
  :depends-on ("cl+ssl" "usocket" "bordeaux-threads" "mgl-pax" "puri" "http2"
                        "cl-async-ssl" "cffi")
  :defsystem-depends-on ("cffi-grovel")
  :pathname "src"
  :components ((:file "package")
               (:file "utils")
               (:file "server")
               (:file "http2")
               (:file "tls")
               (:file "synchronous")
               (:file "with-http2-lib")
               (:file "uv")
               (:cffi-grovel-file "async-openssl-grovel")
               (:file "async-openssl")))

(defsystem #:tls-server/test
  :depends-on ("tls-server" "fiasco" "cl-ppcre" "puri" "http2/client")
  :perform (asdf:test-op (o s)
                         (progn
                           (symbol-call :fiasco '#:run-package-tests :package '#:tls-server/mini-http2/tests
                                        :verbose t)
                           (symbol-call :fiasco '#:run-package-tests :package '#:tls-server/tests
                                        :verbose t))
  :serial t)
  :license  "MIT"
  :pathname "tests"
  :components ((:file "package")
               (:file "utils")
               (:file "client")
               (:file "http2")))

(defsystem #:tls-server/clip
  :depends-on ("tls-server" "clip-1994" "clip-1994/doc")
  :license  "MIT"
  :pathname "clip"
  :components ((:file "setup")
))
