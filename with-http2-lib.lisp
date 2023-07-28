(in-package mini-http2)

(defclass naive-server-connection (http2::server-http2-connection)
  ()
  (:default-initargs :stream-class 'naive-server-stream))

(defclass naive-server-stream (http2::server-stream)
  ())

(defmethod http2::peer-ends-http-stream ((stream naive-server-stream))
  (send-response (http2::get-network-stream stream)
                 (http2::get-stream-id stream))
  (setf  (http2::get-streams (http2::get-connection stream))
         (remove stream (http2::get-streams (http2::get-connection stream))))
  #+nil ((http2::write-headers-frame stream
                                     (load-time-value
                                      (list (octetize #(136 15 16 135 73 124 165 137 211 77 31))))
                                     :end-headers t)
         (apply #'http2::write-data-frame stream (load-time-value
                                                  (octetize (map 'list 'char-code *result-text*)))
                '(:end-stream t))))

(defmethod http2::add-header (connection (stream naive-server-stream) name value)
  (when (keywordp name) (call-next-method)))

(defun do-connection/2 (stream)
  "Process a HTTP2 connection naively: handle preface, and read frames till there
  is end of stream; write static response in that case.

Terminate if either SSL error occurs, or go-away restart is invoked."
  (restart-case
      (handler-case
          (loop with connection = (make-instance 'naive-server-connection
                                                 :network-stream stream)
                initially (http2::read-client-preface connection)
                do (http2:read-frame connection stream))
        (cl+ssl::ssl-error (e)
          ;; ended by ssl error
          (unless (member (type-of e) '(cl+ssl::ssl-error-syscall))
            (describe e)))
        (stream-error (e)
          (unless (member (type-of e) '(sb-int:broken-pipe))
            (describe e))))
    (go-away ())))

(defmethod do-new-connection (listening-socket tls (dispatch-method (eql :none/http2)))
  "Handle the connection while doing nothing else.

This version (DISPATCH-METHOD being :none) gives up on trying to serve more
clients: when it gets connection, it reads the requests and handles them as they
arrive. When the client sends go-away frame, it closes connection and is ready
to serve another client.

Obviously, there is little overhead and this version is pretty fast, especially
with requet pilelining."

  (usocket:with-connected-socket (plain (usocket:socket-accept listening-socket
                                                               :element-type '(unsigned-byte 8)))
    (do-connection/2 (get-stream plain tls))))
