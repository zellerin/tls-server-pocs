;;;; package.lisp
;;
;;;; Copyright (c) 2023 Tomáš Zellerin <tomas@zellerin.cz>
(in-package #:cl)
(mgl-pax:define-package  #:mini-http2
  (:use #:cl)
  (:nicknames  #:tls-server)
  (:export #:go-away
           #:*settings-frame* #:*ack-frame* #:*header-frame* #:*data-frame*
           #:buffer-with-changed-stream #:get-frame-size #:get-stream-id
           #:get-stream-id-if-ends
           #:+client-preface-length+ #:+goaway-frame-type+
           #:stream-id #:frame-size #:octet-vector
           #:fully-read-array
           #:do-log #:*buffer*
           #:read-client-preface
           #:create-server))


(mgl-pax:define-package #:tls-server/synchronous
  (:use #:cl #:mini-http2))

(mgl-pax:define-package #:tls-server/async
    (:use #:cl #:mini-http2)
  (:import-from #:cl-async #:socket-data #:write-socket-data #:close-socket
                #:start-event-loop #:tcp-server))

(mgl-pax:define-package #:tls-server/async/tls
  (:use #:cl #:mini-http2 #:cffi))

(in-package #:mini-http2)
(mgl-pax:defsection mini-http2::@index
    (:title "Experiments with HTTP/2 server")

  "I wanted to play with different options for HTTP/2 server implementations. While
I have a more correct implementation of HTTP/2, I wanted something simple to
test different client handling options, as well as speed limits and impact of
different choices.

So this repository implements:

- very simplified (and indeed incorrect in more than few ways) server side of HTTP/2
- protocol, and based of that several versions of TCP server that accept and
- handle the request."
  (mini-http2::@server-actions mgl-pax:section)
  (mini-http2::@tls mgl-pax:section)
  (tls-server/synchronous::@synchronous mgl-pax:section)
  (mini-http2::@use-http2-lib mgl-pax:section)
  (tls-server/async::@async  mgl-pax:section)
  (mini-http2::@http2-protocol mgl-pax:section)
  (tls-server/async/tls::@async-server mgl-pax:section))

(mgl-pax:defsection mini-http2::@overview
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
