(in-package :tls-server/synchronous)

(mgl-pax:defsection @synchronous
    (:title "Synchronous implementations")
  "Simplest implementation is a single thread that reads and handles the
requests. Once the request from a client is received, this client is listened to
until finished, and then can next client connect.

Obviously not ideal, but simple."
  (do-new-connection (method () (t t (eql :none))))

  "Another implementation is a thread for the listener, and new thread for each
client. Now this is used by Hunchentoot and other non-lisp implementations, and
works quite well under many conditions.

This version has supposedly disadvantage when there are too many
clients/threads (RAM for threads needed, etc).

The speed for single client is comparable to the single-client version.

Also, this version (as well as the single client one) can be ported to most CL
implementations, as it uses standard libraries - bordeaux-threads, cl+ssl and
usocket."
  (do-new-connection (method () (t t (eql :thread))))
  (do-connection function)
  (go-away restart)
  (*buffer* variable))

(defun read-frame-check-end-stream (stream)
  "Read a frame and check whether it is an end stream name. If so, return the
stream number.

Check header constrains to some extent."
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

Terminate if either SSL error occurs, or GO-AWAY restart is invoked."
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
            (error e)))
        (stream-error (e)
          (unless (member (type-of e) '(sb-int:broken-pipe))
            (error e))))
    (go-away ())))

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
  "Handle the connection in a new thread."
  (let ((socket (usocket:socket-accept listening-socket
                                       :element-type '(unsigned-byte 8))))
    (bt:make-thread
     (lambda ()
       (unwind-protect
            (do-connection (get-stream socket tls))
         (usocket:socket-close socket)))
     :name "HTTP2 connection server")))
