(in-package tls-server/mini-http2)

(mgl-pax:defsection @use-http2-lib
    (:title "Server built using HTTP2 package.")
  "Server implementations so far used the simplified HTTP/2 protocol described
[above][tls-server/mini-http2/core::@http2-protocol]. Now we do the same using
[HTTP2][asdf/system:system], still synchronously to compare the ease of
implementation and speed."
  (do-new-connection (method () (t t (eql :none/http2)))))

(defclass naive-server-connection (http2/core::server-http2-connection)
  ()
  (:default-initargs :stream-class 'naive-server-stream))

(defclass naive-server-stream (http2/core::server-stream)
  ())

(defmethod http2/core::peer-ends-http-stream ((stream naive-server-stream))
  (send-response (http2/core::get-network-stream stream)
                 (http2/core::get-stream-id stream))
  (setf  (http2/core::get-streams (http2/core::get-connection stream))
         (remove stream (http2/core::get-streams (http2/core::get-connection stream))))
  #+nil ((http2/core::write-headers-frame stream
                                     (load-time-value
                                      (list (octetize #(136 15 16 135 73 124 165 137 211 77 31))))
                                     :end-headers t)
         (apply #'http2/core::write-data-frame stream (load-time-value
                                                  (octetize (map 'list 'char-code *result-text*)))
                '(:end-stream t))))

(defmethod http2/core::add-header (connection (stream naive-server-stream) name value)
  (when (keywordp name) (call-next-method)))

#+nil(defun do-connection/2 (stream)
  "Process a HTTP2 connection naively: handle preface, and read frames till there
  is end of stream; write static response in that case.

Terminate if either SSL error occurs, or go-away restart is invoked."
  (restart-case
      (handler-case
          (loop with connection = (make-instance 'naive-server-connection
                                                 :network-stream stream)
                initially (http2/server::parse-client-preface connection)
                do (http2/core:read-frame connection stream))
        (cl+ssl::ssl-error (e)
          ;; ended by ssl error
          (unless (member (type-of e) '(cl+ssl::ssl-error-syscall))
            (describe e)))
        (stream-error (e)
          (unless (member (type-of e) '(sb-int:broken-pipe))
            (describe e))))
    (go-away ())))

(defmethod do-new-connection (listening-socket tls (dispatch-method (eql :none/http2)) &key)
  "Handle the connection while doing nothing else using HTTP2 asdf library for
actual work. Otherwise it is same as the :NONE method (i.e., serving a single
client at time)."

  (usocket:with-connected-socket (plain (usocket:socket-accept listening-socket
                                                               :element-type '(unsigned-byte 8)))
    (do-connection/2 (maybe-add-tls plain tls))))
