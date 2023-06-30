(in-package mini-http2)

(defmethod do-new-connection (listening-socket tls (dispatch-method (eql :none)))
  "Handle the connection while doing nothing else.

This version (DISPATCH-METHOD being :none) gives up on trying to serve more
clients: when it gets connection, it reads the requests and handles them as they
arrive. When the client sends go-away frame, it closes connection and is ready
to serve another client.

Obviously, there is little overhead and this version is pretty fast, especially
with requet pilelining."

  (usocket:with-connected-socket (plain (usocket:socket-accept listening-socket
                                                               :element-type '(unsigned-byte 8)))
    (do-connection (get-stream plain tls))))

(defmethod do-new-connection (listening-socket tls (dispatch-method (eql :thread)))
  "Handle the connection in a new thread.

This version (DISPATCH-METHOD being :thread) has supposedly disadvantage when
there are too many clients/threads (RAM for threads needed, .

The speed for single client is comparable to the single-client version.

Also, this version (as well as the single client one) can be ported to most CL
implementations, as it uses standard libraries - bordeaux-threads and usocket."
  (let ((socket (usocket:socket-accept listening-socket
                                       :element-type '(unsigned-byte 8))))
    (bt:make-thread
     (lambda ()
       (unwind-protect
            (do-connection (get-stream socket tls))
         (usocket:socket-close socket)))
     :name "HTTP2 connection server")))
