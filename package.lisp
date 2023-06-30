;;;; package.lisp
;;
;;;; Copyright (c) 2023 Tomáš Zellerin <tomas@zellerin.cz>


(defpackage #:tls-server
  (:use #:cl))

(defpackage #:tls-server/thread
  (:use #:cl #:tls-server))
