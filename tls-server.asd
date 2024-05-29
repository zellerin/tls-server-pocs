;;;; tls-server.asd
;;
;;;; Copyright (c) 2023 Tomáš Zellerin <tomas@zellerin.cz>


(asdf:defsystem #:tls-server/core
  :description "Various implementations of toy HTTP2 server."
  :author "Tomáš Zellerin <tomas@zellerin.cz>"
  :license  "MIT"
  :version "0.9"
  :serial t
  :in-order-to ((asdf::test-op (asdf:test-op "tls-server/test")))
  :depends-on ("mgl-pax" "usocket" "puri" "bordeaux-threads")
  :pathname "src"
  :components ((:file "package")
               (:file "utils")
               (:file "server")
               (:file "http2")))

(asdf:defsystem #:tls-server/synchronous
  :description "Synchronous implementations of toy HTTP2 server."
  :author "Tomáš Zellerin <tomas@zellerin.cz>"
  :license  "MIT"
  :version "0.9"
  :serial t
  :depends-on ("tls-server/core" "cl+ssl" "http2")
  :pathname "src"
  :components ((:file "tls")
               (:file "synchronous")))

(asdf:defsystem #:tls-server/async
  :description "Synchronous implementations of toy HTTP2 server."
  :author "Tomáš Zellerin <tomas@zellerin.cz>"
  :license  "MIT"
  :version "0.9"
  :serial t
  :in-order-to ((asdf::test-op (asdf:test-op "tls-server/test")))
  :depends-on ("tls-server/core" "bordeaux-threads" #:cl-async #:cl-async-ssl)
  :pathname "src"
  :components ((:file "uv")))

(asdf:defsystem #:tls-server/async-openssl
  :description "Synchronous implementations of toy HTTP2 server."
  :author "Tomáš Zellerin <tomas@zellerin.cz>"
  :license  "MIT"
  :version "0.9"
  :serial t
  :depends-on (#:tls-server/core  "cffi")
  :defsystem-depends-on ("cffi-grovel")
  :pathname "src"
  :components ((:file "package-async")
               (:cffi-grovel-file "async-openssl-grovel")
               (:file "async-openssl")))

(asdf:defsystem #:tls-server
  :description "Synchronous implementations of toy HTTP2 server."
  :author "Tomáš Zellerin <tomas@zellerin.cz>"
  :license  "MIT"
  :version "0.9"
  :serial t
  :in-order-to ((asdf::test-op (asdf:test-op "tls-server/test")))
  :depends-on (#:tls-server/synchronous #:tls-server/async #:tls-server/async-openssl)
  :components ((:file "src/doc")))

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
  :depends-on ("tls-server" "clip-1994" "clip-1994/loader" "clip-1994/doc")
  :license  "MIT"
  :pathname "clip"
  :components ((:file "setup")))
