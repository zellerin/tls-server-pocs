(in-package #:tls-server/mini-http2)

(mgl-pax:defsection @http2-protocol (:title "HTTP/2 protocol.")
  "Simplified - and incorrect in many corner cases - HTTP/2 protocol implemented
here is is as follows. This should be sufficient to respond to a browser, curl
or h2load."
  (+client-preface-start+ variable)
  (+client-preface-length+ constant)
  (read-client-preface function)
  (*settings-frame* variable)
  (*ack-frame* variable)
  (stream-id type)
  (*data-frame* variable)
  (*header-frame* variable)
  (+goaway-frame-type+ variable)
  (get-frame-size function)
  (frame-size type)
  (get-stream function)
  (get-stream-id function)
  (get-frame-type function)
  (get-frame-flags function)
  (get-stream-id-if-ends function)
  (buffer-with-changed-stream function)
  (send-response function))

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
  "Write response to the request with STREAM-ID to Common Lisp output STREAM."
  (write-sequence (buffer-with-changed-stream *header-frame* stream-id) stream)
  (write-sequence (buffer-with-changed-stream *data-frame* stream-id) stream)
  (force-output stream))

(defgeneric get-stream (socket tls)
  (:documentation "Return either plain (if tls is nil) or TLS (if :tls) stream build upon SOCKET")
  (:method (socket (tls (eql :tls)))
    (wrap-to-tls socket))
  (:method (socket (tls (eql nil)))
    (usocket:socket-stream socket)))
