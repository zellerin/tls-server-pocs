(in-package mini-http2)

(mgl-pax:defsection @http2-protocol (:title "HTTP/2 protocol.")
  "Simplified - and incorrect in many corner cases - HTTP/2 protocol implemented
here is is as follows. This should be sufficient to respond to a browser, curl
or h2load."
  (+client-preface-start+ variable)
  (*settings-frame* variable)
  (*ack-frame* variable)
  (stream-id type)
  (*data-frame* variable)
  (+goaway-frame-type+ variable)
  (get-frame-size function)
  (get-stream-id function)
  (get-frame-type function)
  (get-frame-flags function)
  (get-stream-id-if-ends function)
  (buffer-with-changed-stream function))

(defvar +client-preface-start+
  #(80 82 73 32 42 32 72 84 84 80 47 50 46 48 13 10 13 10 83 77 13 10 13 10)
  "Clients send 24 octets of +CLIENT-PREFACE-START+, which in hex
notation is this. That is, the connection preface starts with the string \"PRI *
 HTTP/2.0\\r\\n\\r\\nSM\\r\\n\\r\\n\".")

(defconstant +client-preface-length+ 24)

(defconstant +goaway-frame-type+ 7
  "When client is done (or after an error) it sends goaway frame, and both client
and server terminate the connection socket. This is kind of courtesy, and any
side should be ready for the other side terminating connection without goaway
frame.

Server can send goaway frame as well, but our servers do not.")

(defvar *settings-frame* (octetize #(0 0 0 4 0 0 0 0 0))
  "After client preface, both client and server send their settings frame
(*SETTINGS-FRAME*). The frame here is empty settings frame.")
(defvar *ack-frame* (octetize #(0 0 0 4 1 0 0 0 0))
  "Settings frame should be acknowledged by sending *ACK-FRAME* (type+flag)")
(defvar *header-frame* (octetize #(0 0 11 1 4 0 0 0 0 136 15 16 135 73
                                   124 165 137 211 77 31)))
(defvar *result-text* (format nil "Hello World~%"))
(defvar *data-frame* (octetize `(0 0 ,(length *result-text*) 0 1 0 0 0 0
                                 ,@(map 'list 'char-code *result-text*)))
  ;; This assumes *result-text* is at most 255 chars long.
  "Server replies to header frame with header and data frames with same stream
ID. The data frame here is a data frame with zero stream ID (to be patched
before sending) and payload short ASCII text from *result-text*.")

(deftype stream-id ()
  "Client sends HTTP2 requests as a headers (and possibly data) frame, with last
packet having END-OF-HEADERS flag
particular stream. Each stream is a 23 bit integer."
  '(unsigned-byte 23))

(deftype frame-size ()
  "Frame size parameter can be 32 bits long; however, values above 2^14 are an error."
  '(unsigned-byte 32))

(mgl-pax:defsection @server-actions
    (:title "Serving HTTP/2")
  "This framework is used by all server implementation:"
  (create-server function)
  (do-new-connection generic-function)
  (kill-server restart))

(mgl-pax:defsection @synchronous
    (:title "Synchronous implementation")
  (do-new-connection (method () (t t (eql :none))))
  (do-new-connection (method () (t t (eql :thread))))
  (do-connection function)
  (*buffer* variable)
  (read-client-preface function)
  (send-response function))

(defvar *buffer* nil
  "Preallocated buffer for reading from stream. This is initialized for each
connection depending on the dispatch method.")

(defun read-client-preface (stream)
  (fully-read-array stream *buffer* +client-preface-length+)
  (assert (null
           (mismatch +client-preface-start+ *buffer* :end2 +client-preface-length+))
          ()
          "Expected client preface, got ~s/~%~a"
          (map 'string 'code-char (subseq *buffer* 0 +client-preface-length+))
          (subseq *buffer* 0 +client-preface-length+)))

(defun buffer-with-changed-stream (buf stream-id)
  "Change stream id of a frame in BUF to STREAM-ID."
  (declare (octet-vector buf))
  (setf (aref buf 8) (ldb (byte 8 0) stream-id))
  (setf (aref buf 7) (ldb (byte 8 8) stream-id))
  (setf (aref buf 6) (ldb (byte 8 16) stream-id))
  (setf (aref buf 5) (ldb (byte 7 23) stream-id))
  buf)

(defun get-frame-size (header)
  "Get frame size of a frame from frame header."
  (let ((frame-size 0))
    (setf (ldb (byte 8 16) frame-size) (aref header 0)
          (ldb (byte 8 8) frame-size) (aref header 1)
          (ldb (byte 8 0) frame-size) (aref header 2))
    frame-size))

(defun get-stream-id (header)
  (let ((stream-id 0))
    (setf (ldb (byte 7 23) stream-id) (aref header 5)
          (ldb (byte 8 16) stream-id) (aref header 6)
          (ldb (byte 8 8) stream-id) (aref header 7)
          (ldb (byte 8 0) stream-id) (aref header 8))
    stream-id))

(defun get-frame-type (header) (aref header 3))
(defun get-frame-flags (header) (aref header 4))

(defun get-stream-id-if-ends (header)
  "Stream id when header closes the stream on client side."
  (when (and (plusp (logand (get-frame-flags header) 1))
             ;; only data frame and header frame has end-stream
             (<= (get-frame-type header) 1))
    (get-stream-id header)))

(defun send-response (stream stream-id)
  "Write response to the request with STREAM-ID."
  (write-sequence (buffer-with-changed-stream *header-frame* stream-id) stream)
  (write-sequence (buffer-with-changed-stream *data-frame* stream-id) stream)
  (force-output stream))

(defun read-frame-check-end-stream (stream)
  (let ((header (fully-read-array stream *buffer* 9)))
    (declare (octet-vector header))
    (let* ((frame-size (get-frame-size header))
           (type (get-frame-type header)))
      (declare (frame-size frame-size))
      (when (= type +goaway-frame-type+)
        (invoke-restart 'go-away))
      (unless (>= 16384 frame-size)
        (close stream)
        (error "Too big frame (~d)" frame-size))
      (prog1
          (get-stream-id-if-ends header)
        (fully-read-array stream *buffer* frame-size)))))

(defun do-connection (stream)
  "Process a HTTP2 connection naively: handle preface, and read frames till there
  is end of stream; write static response in that case.

Terminate if either SSL error occurs, or go-away restart is invoked."
  (restart-case
      (handler-case
          (let ((*buffer*  (make-array 16385
                                       :element-type '(unsigned-byte 8)
                                       :initial-element 0)))
            (read-client-preface stream)
            (write-sequence *settings-frame* stream)
            (write-sequence *ack-frame* stream)
            (loop
              for stream-to-send-response = (read-frame-check-end-stream stream)
              when stream-to-send-response
                do
                   (send-response stream stream-to-send-response)))
        (cl+ssl::ssl-error (e)
          ;; ended by ssl error
          (unless (member (type-of e) '(cl+ssl::ssl-error-syscall))
            (describe e)))
        (stream-error (e)
          (unless (member (type-of e) '(sb-int:broken-pipe))
            (describe e))))
    (go-away ())))

(defgeneric get-stream (socket tls)
  (:documentation "Return either plain (if tls is nil) or TLS (if :tls) stream build upon SOCKET")
  (:method (socket (tls (eql :tls)))
    (wrap-to-tls socket))
  (:method (socket (tls (eql nil)))
    (usocket:socket-stream socket)))

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

(mgl-pax:define-restart kill-server (&optional value)
  "Restart established in CREATE-SERVER that can be invoked to terminate the server
properly and return VALUE.")

(defun url-from-socket (socket host tls)
  "Return a function that takes one parameter, socket, and call FN on socket port
number and additional ARGS as parameters in a separate thread. Then it kills the
server.

This is to be used as callback fn on an open server for testing it."
  (make-instance 'puri:uri
                 :scheme (if tls :https :http)
                 :port (usocket:get-local-port socket)
                 :host host))

(defun create-server (port tls dispatch-method
                      &key
                        (host "127.0.0.1")
                        (announce-url-callback (constantly nil)))
  "Create a server on HOST and PORT that handles connections (possibly with TLS) using
DISPATCH-METHOD.

ANNOUNCE-URL-CALLBACK is called when server is set up and receives one
parameter, URL server listens to. The idea is to be able to connect to server
when PORT is 0, that is, random port.

Calls DO-NEW-CONNECTION to handle the connections with restart KILL-SERVER available."
  (restart-case
      (usocket:with-socket-listener (listening-socket host port
                                                      :reuse-address t
                                                      :element-type '(unsigned-byte 8))
        (funcall announce-url-callback (url-from-socket listening-socket host tls))
        (loop
          (do-new-connection listening-socket tls dispatch-method)))
    (kill-server (&optional value) :report "Kill server" value)))
