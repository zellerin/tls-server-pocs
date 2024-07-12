(in-package #:tls-server/mini-http2)

;;; taken and adapted from http2 package

(defvar *request-headers* #(130 135 132 1 134 160 228 29 19 157 9)
  "GET / localhost")

(defun dummy-client-1 (url)
  (unless (eql (puri:uri-scheme url) :https)
    (error "Only HTTPS supported."))
  (let (res)
    (with-simple-restart (kill-client "Kill client")
      (usocket:with-client-socket (socket stream (puri:uri-host url)
                                          (or (puri:uri-port url) 443)
                  :element-type '(unsigned-byte 8))
        (with-open-stream (tls-stream
                           (cl+ssl:make-ssl-client-stream stream :alpn-protocols '("h2")))
          (write-sequence  #(1 2 3 4 5 6 7 8 9) tls-stream)
          (let ((connection
                  (make-instance 'http2:vanilla-client-connection :network-stream tls-stream)))
            (http2::process-pending-frames connection nil)))))
    res))



(defclass my-vanilla-client-stream (http2::vanilla-client-stream)
  ())

(defclass my-client (http2:vanilla-client-connection)
  ()
  (:default-initargs :stream-class 'my-vanilla-client-stream))

(defmethod http2::apply-text-data-frame ((stream my-vanilla-client-stream) payload)
  (princ payload))

(defmethod http2::peer-ends-http-stream ((stream my-vanilla-client-stream))
  (invoke-restart 'kill-client)
  ;; 20240609 TODO: send goaway frame first
  )

(defun dummy-client (url)
  (unless (eql (puri:uri-scheme url) :https)
    (error "Only HTTPS supported."))
  (let (res)
    (with-simple-restart (kill-client "Kill client")
      (usocket:with-client-socket (socket stream (puri:uri-host url)
                                          (or (puri:uri-port url) 443)
                                          :element-type '(unsigned-byte 8))
        (with-open-stream (tls-stream
                           (cl+ssl:make-ssl-client-stream stream :alpn-protocols '("h2")))
          (http2/client::retrieve-url-using-network-stream
           tls-stream url
           :connection-class 'my-client
           :end-headers-fn (constantly t)))))
    res))
