(in-package #:tls-server)

(defsection @packages
    (:title "Packages")
  (#:tls-server package)
  (#:tls-server/utils package)
  (#:tls-server/mini-http2 package))

(defsection @index
    (:title "Experiments with HTTP/2 server")

  "I wanted to play with different options for HTTP/2 server implementations. While
I have a more correct implementation of HTTP/2, I wanted something simple to
test different client handling options, as well as speed limits and impact of
different choices.

So this repository implements:

- very simplified (and indeed incorrect in more than few ways) server side of
  HTTP/2 protocol, and based of that
- several versions of TCP server that accept and handle the request."

  "Presently, the most advanced is the async version that uses CFFI
![](images/five-clients.png)
![](images/single-client.png)
"
  (tls-server::@server-actions section)
  (@implementations section)
  (tls-server/utils::@octets section)
  (tls-server/mini-http2::@http2-protocol section)
  (tls-server/mini-http2::@tls section)
  (tls-server/synchronous::@synchronous section)
#+not-ready-yet  (tls-server/mini-http2::@use-http2-lib section)
  (tls-server/async::@async  section)
  (tls-server/async/tls::@async-server section)
  (tls-server/utils::@mgl-extensions section)
  (@packages section)
  (@experiments section))

(defsection @overview
    (:title "Overview")

  "I wanted to play with different options for HTTP/2 server implementations. While
I have a more correct implementation of HTTP/2, I wanted something simple to
test different client handling options, as well as speed limits and impact of
different choices.

So this repository implements:

- very simplified (and indeed incorrect in more than few ways) server side of HTTP/2 protocol, and based of that
- several versions of TCP server that accept and handle the request.

All the server implementations can be started with CREATE-SERVER. New server
types with same interface are defined by specializing DO-NEW-CONNECTION"
  "See also [additional documentation](https://docs.zellerin.cz/tls-server-poc) for
details of the implementations."
  (create-server function)
  (do-new-connection generic-function)
  (@implementations section)
  (@measurements section)
  (@todos section))

(defsection @implementations
    (:title "Implementations")
  "Following implementations are defined:"
  (do-new-connection (method nil (t t (eql :none))))
  (do-new-connection (method nil (t t (eql :thread))))
;; FIXME: bring back none/http2
  #+not-ready-yet  (do-new-connection (method nil (t t (eql :none/http2))))
  (do-new-connection (method nil (t (eql nil) (eql :async))))
  (do-new-connection (method nil (t (eql :tls) (eql :async))))
#+not-ready-yet  (do-new-connection (method nil (t (eql :nonblock) (eql :async))))
  (do-new-connection (method nil (t (eql :tls) (eql :async-custom)))))

(defsection @todos
    (:title "Bugs and considered improvements")
  "Test in another implementation"
  "See TODO: and FIXME: in the code"

  "Try to fit the observer performance to some formula,"
  "Do some measurements for real clients and specific cases"
  "Try Quic/HTTP3")

(defsection @measurements
  (:title "Some metrics")
  "Now it is possible to do some measurements; assuming you have clip library
  installed (not in quicklisp), you can run scripts [./clip/measure.lisp] to obtain how fast some
  combinations of number of clients, size of pipeline and implementation is."
  "If you have adw-charting library installed (surprisingly presently also not in quicklisp), you can then generate some graphs from the data using [./clip/report.lisp]"
  "Or you can just check [./images] that reflect the state at some point of time on some
  particular machine.")

(defsection @experiments
    (:title "Experiments")
  (clips section))
