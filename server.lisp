(in-package #:tls-server)

(mgl-pax:defsection @server-actions
    (:title "Generic server interface")
  "The functions below implement server creation on an abstract level. Individual
server types implement appropriate methods to ensure desired behaviour."
  (create-server function)
  (do-new-connection generic-function)
  (kill-server restart)
  (kill-server function)
  (kill-parent restart)
  (go-away restart)
  (callback-on-server function)
  (url-from-socket function)
  (url-from-port function)
  ;; TODO: remove/fix
  (*buffer* variable))

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

(defun url-from-port (port host tls)
  "Return URL that combines HOST with the port of the SOCKET.

This is to be used as callback fn on an open server for testing it."
  (make-instance 'puri:uri
                 :scheme (if tls :https :http)
                 :port port
                 :host host))

(defgeneric do-new-connection (listening-socket tls dispatch-method)
  (:documentation
   "This method is implemented for the separate connection types. It waits on
new (possibly tls) connection to the LISTENING-SOCKET and start handling it
using DISPATCH-METHOD.

See @IMPLEMENTATIONS for available DISPATCH-METHOD.

TLS is either NIL or :TLS. Note that when using HTTP/2 without TLS, most clients have to be instructed to
use tls - e.g., --http2-prior-knowledge for curl."))

(defun callback-on-server (fn &key (thread-name "Test client for a server"))
  "Return a function that takes one parameter, URL, as a parameter and calls FN on
it in a separate thread. Then it kills the server by invoking KILL-SERVER restart.

This is to be used as callback on an open server for testing it."
  (lambda (url)
    (let ((parent (bt:current-thread)))
      (bt:make-thread
       (lambda ()
         (bt:interrupt-thread parent #'kill-server
                              (with-simple-restart (kill-parent "Kill parent")
                                (funcall fn url))))
       :name thread-name))))

(defun kill-server (&optional res)
  "Kill server by invoking KILL-SERVER restart, if it exists."
  (let ((restart
          (find-restart 'kill-server)))
    (if restart
        (invoke-restart restart res))))

(mgl-pax:define-restart kill-server (&optional value)
  "Restart established in CREATE-SERVER that can be invoked to terminate the server
properly and return VALUE.")

(mgl-pax:define-restart kill-parent (&optional value)
  "Restart established in CREATE-SERVER that TODO")

;;;; TODO: document and review
(mgl-pax:define-restart go-away (&optional value)
  "Handler to be invoked to close HTTP connection from our side.

It is established either in TLS-SERVER/SYNCHRONOUS:DO-CONNECTION.

TODO: Should we have it in async-cffi loop as well?")

;; TODO: why is this needed here?
(defvar *buffer* nil
  "Preallocated buffer for reading from stream. This is initialized for each
connection depending on the dispatch method.")
