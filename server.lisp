(in-package #:tls-server)

(mgl-pax:defsection @server-actions
    (:title "Generic server interface")
  "The functions below implement server creation on an abstract level. Individual
server types implement appropriate methods to ensure desired behaviour."
  (create-server function)
  (do-new-connection generic-function)
  (kill-server restart)
  (kill-server function)
  (callback-on-server function)
  (url-from-socket function))

(defun create-server (port tls dispatch-method
                      &key
                        (host "127.0.0.1")
                        (announce-url-callback (constantly nil)))
  "Create a server on HOST and PORT that handles connections (possibly with TLS) using
DISPATCH-METHOD.

ANNOUNCE-URL-CALLBACK is called when server is set up on the TCP level and
receives one parameter, URL that server listens on. The idea is to be able to connect
to server when PORT is 0, that is, random port.

Establishes restart KILL-SERVER to close the TCP connection and return.

Calls DO-NEW-CONNECTION to actually handle the connections after the callback
returns This function also receives the listening socket ad TLS and
DISPATCH-METHOD as parameters."
  (restart-case
      (usocket:with-socket-listener (listening-socket host port
                                                      :reuse-address t
                                                      :element-type '(unsigned-byte 8))
        (funcall announce-url-callback (url-from-socket listening-socket host tls))
        (loop
          (do-new-connection listening-socket tls dispatch-method)))
    (kill-server (&optional value) :report "Kill server" value)))

(defun url-from-socket (socket host tls)
  "Return URL that combines HOST with the port of the SOCKET.

This is to be used as callback fn on an open server for testing it."
  (make-instance 'puri:uri
                 :scheme (if tls :https :http)
                 :port (usocket:get-local-port socket)
                 :host host))

(defgeneric do-new-connection (listening-socket tls dispatch-method)
  (:documentation
   "This method is implemented for the separate connection types. It waits on
new (possibly tls) connection to the LISTENING-SOCKET and start handling it
using DISPATCH-METHOD.

DISPATCH-METHOD can presently be either :NONE, :NONE/HTTP2, :THREAD or
:ASYNC (w/o TLS only for now), see appropritate methods. Methods can be created
for new dispatch methods.

TLS is either NIL or :TLS. Note that when using HTTP/2 without TLS, most clients have to be instructed to
use tls - e.g., --http2-prior-knowledge for curl."))
