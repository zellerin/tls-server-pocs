;;;; tls-server.asd
;;
;;;; Copyright (c) 2023 Tomáš Zellerin <tomas@zellerin.cz>


(asdf:defsystem #:tls-server
  :description "Describe tls-server here"
  :author "Tomáš Zellerin <tomas@zellerin.cz>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :in-order-to ((asdf::test-op (asdf:test-op "tls-server/test")))
  :depends-on ("cl+ssl" "usocket" "bordeaux-threads" "mgl-pax" "puri" "http2"
                        "cl-async-ssl")
  :components ((:file "package")
               (:file "utils")
               (:file "http2")
               (:file "tls")
               (:file "synchronous")
               (:file "with-http2-lib")
               (:file "uv")))

(defsystem #:tls-server/test
  :depends-on ("tls-server" "fiasco" "cl-ppcre" "puri" "http2/client")
  :perform (asdf:test-op (o s)
                         (progn
                           (symbol-call :fiasco '#:run-package-tests :package '#:mini-http2/tests
                                        :verbose t)
                           (symbol-call :fiasco '#:run-package-tests :package '#:tls-server/tests
                                        :verbose t))
  :serial t)
  :license  "MIT"
  :pathname "tests"
  :components ((:file "package")
               (:file "utils")
               (:file "timing")
               (:file "client")
               (:file "http2")))
