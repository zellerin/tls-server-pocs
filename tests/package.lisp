(fiasco:define-test-package :tls-server/tests
  (:use #:cl #:fiasco #:usocket #:tls-server/mini-http2))


(fiasco:define-test-package #:tls-server/mini-http2/tests
  (:use #:cl #:fiasco #:tls-server/mini-http2))

(in-package tls-server/tests)
