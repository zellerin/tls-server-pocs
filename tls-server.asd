;;;; tls-server.asd
;;
;;;; Copyright (c) 2023 Tomáš Zellerin <tomas@zellerin.cz>


(asdf:defsystem #:tls-server
  :description "Describe tls-server here"
  :author "Tomáš Zellerin <tomas@zellerin.cz>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :in-order-to ((test-op (test-op "tls-server/test")))
  :depends-on ("cl+ssl" "usocket" "bordeaux-threads" #+sbcl "sb-sprof")
  :components ((:file "package")
               (:file "single")))


(defsystem #:tls-server/test
  :depends-on ("tls-server" "fiasco")
  :perform (test-op (o s)
                    (symbol-call :fiasco '#:run-package-tests :package '#:tls-server/tests)
  :serial t)
  :license  "MIT"
  :pathname "tests"
  :components ((:file "package")))
