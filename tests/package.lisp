(fiasco:define-test-package :tls-server/tests
  (:use #:cl #:fiasco #:usocket))

(in-package tls-server/tests)

(deftest dummy-test ()
  "Test that testing works"
  (is t))
