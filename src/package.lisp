;;;; Copyright (c) 2023, 2024 Tomáš Zellerin <tomas@zellerin.cz>

(mgl-pax:define-package #:tls-server/utils
  (:use #:cl #:mgl-pax)
  (:documentation "Octet processing utilities. See TLS-SERVER/UTILS::@OCTETS"))

(mgl-pax:define-package #:tls-server
    (:use #:cl #:mgl-pax)
  (:export #:@index #:@overview)
  (:documentation
   "Exports CREATE-SERVER (needed to invoke a server),
DO-NEW-CONNECTION (specialized to implement particular servers), and various
restarts. See @SERVER-ACTIONS.

Also holds the top-level documentation sections, @INDEX and @OVERVIEW."))

(in-package #:tls-server)

(define-package #:tls-server/mini-http2
  (:use #:cl #:tls-server/utils
        ;; TODO: just buffer needed
        #:tls-server)
  (:documentation
   "Basic functionality for low-level HTTP2 implementation."))
