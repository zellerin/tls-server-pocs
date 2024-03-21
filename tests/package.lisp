(fiasco:define-test-package :tls-server/tests
  (:use #:cl #:fiasco #:usocket #:tls-server/mini-http2 #:tls-server))


(fiasco:define-test-package #:tls-server/mini-http2/tests
  (:use #:cl #:fiasco #:tls-server/mini-http2 #:tls-server))

(fiasco:define-test-package #:tls-server/utils/tests
  (:use #:cl #:fiasco #:tls-server/utils))

(in-package #:tls-server/tests)
