;;;; package.lisp
;;
;;;; Copyright (c) 2023 Tomáš Zellerin <tomas@zellerin.cz>


(mgl-pax:define-package #:mini-http2
  (:use #:cl)
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

(mgl-pax:define-package #:tls-server/poll-dispatcher
  (:use #:cl #:cffi)
  (:export call-with-pollfds add-fd  remove-fd client-data-cleanup-sockets
           unix-read
           fd-info fd-info-read-action fd-info-write-action fd-info-stream make-fd-info
           wait-for-fd))

(mgl-pax:define-package #:tls-server/nonblock
  (:use #:cl #:mini-http2
        #:tls-server/poll-dispatcher))

(in-package #:mini-http2)
(mgl-pax:defsection mini-http2::@http2-server-pocs
    (:title "Experiments with HTTP/2 server")
  "I wanted to play with different options for HTTP/2 server implementations. While
I have a more correct implementation of HTTP/2, I wanted something
simple to test different client handling options, as well as speed limits and impact of different choices.

So this repository implements:

- very simplified (and indeed incorrect in few ways) but server side of HTTP/2 protocol, and based of that
- several versions of TCP server that accept and handle the request."
  (mini-http2::@http2-protocol mgl-pax:section)
  (mini-http2::@server-actions mgl-pax:section)
  (tls-server/nonblock::@nonblock-server mgl-pax:section)
  (tls-server/poll-dispatcher::@poll-dispatcher mgl-pax:section))
