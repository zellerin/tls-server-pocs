(fiasco:define-test-package :tls-server/tests
  (:use #:cl #:fiasco #:usocket #:mini-http2))


(fiasco:define-test-package #:mini-http2/tests
  (:use #:cl #:fiasco #:mini-http2))

(in-package tls-server/tests)
