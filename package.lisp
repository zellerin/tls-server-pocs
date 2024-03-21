;;;; package.lisp
;;
;;;; Copyright (c) 2023 Tomáš Zellerin <tomas@zellerin.cz>
(mgl-pax:define-package #:tls-server
  (:use #:cl #:mgl-pax)
  (:export #:@index #:@overview))

(in-package #:tls-server)

(define-package #:tls-server/utils
  (:use #:cl #:mgl-pax
        ;; TODO: just go-away restart...
        #:tls-server))

(define-package #:tls-server/mini-http2
  (:use #:cl #:tls-server/utils
        ;; TODO: just buffer needed
        #:tls-server)
#+nil  (:export #:go-away
           #:*settings-frame* #:*ack-frame* #:*header-frame* #:*data-frame*
           #:buffer-with-changed-stream #:get-frame-size #:get-stream-id
           #:get-stream-id-if-ends
           #:+client-preface-length+ #:+goaway-frame-type+
           #:stream-id #:frame-size #:octet-vector
           #:fully-read-array
           #:do-log #:*buffer*
           #:read-client-preface)
  (:documentation
   "Basic functionality for low-level HTTP2 implementation."))

(define-package #:tls-server/synchronous
  (:use #:cl #:tls-server/mini-http2 #:tls-server #:tls-server/utils))

(define-package #:tls-server/async
  (:use #:cl #:tls-server/mini-http2 #:tls-server/mini-http2)
  (:import-from #:cl-async #:socket-data #:write-socket-data #:close-socket
                #:start-event-loop #:tcp-server))

(define-package #:tls-server/async/tls
  (:use #:cl #:tls-server/mini-http2 #:cffi #:tls-server))

(defsection @index
    (:title "Experiments with HTTP/2 server")

  "I wanted to play with different options for HTTP/2 server implementations. While
I have a more correct implementation of HTTP/2, I wanted something simple to
test different client handling options, as well as speed limits and impact of
different choices.

So this repository implements:

- very simplified (and indeed incorrect in more than few ways) server side of HTTP/2
- protocol, and based of that several versions of TCP server that accept and
- handle the request."
  (tls-server/utils::@octets section)
  (tls-server/mini-http2::@http2-protocol section)
  (tls-server/mini-http2::@server-actions section)
  (tls-server/mini-http2::@tls section)
  (tls-server/synchronous::@synchronous section)
  (tls-server/mini-http2::@use-http2-lib section)
  (tls-server/async::@async  section)
  (tls-server/async/tls::@async-server section))

(defsection @overview
    (:title "Overview")

  "I wanted to play with different options for HTTP/2 server implementations. While
I have a more correct implementation of HTTP/2, I wanted something simple to
test different client handling options, as well as speed limits and impact of
different choices.

So this repository implements:

- very simplified (and indeed incorrect in more than few ways) server side of HTTP/2
- protocol, and based of that several versions of TCP server that accept and
- handle the request."
)
