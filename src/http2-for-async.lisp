(mgl-pax:define-package tls-server/async/http2
  (:use #:cl #:http2 #:tls-server/async/tls #:mgl-pax)
  (:shadow #:client #:make-client-object))

(in-package tls-server/async/http2)

(defsection @full-async-http
    (:title "Asynchronous HTTP/2 using the http2 library")
  "Here we match together all the pieces we already have to provide full
single-thread multiple clients HTTP/2 server that supports most of the HTTP/2
features."

  "We need a dedicated HTTP/2 connection and stream classes to specialize the behaviour."
  (async-stream class)
  (async-server class)
  "Async server needs initial callback function to call when client data
arrive. This would be HTTP2-HELLO."
  (http2-hello function)
  )

(defclass async-stream (http2::server-stream body-collecting-mixin http2::multi-part-data-stream)
  ())

(defclass async-server (http2:write-buffer-connection-mixin http2::server-http2-connection
                        http2::dispatcher-mixin)
  ((tls-server    :accessor get-tls-server    :initarg :tls-server)
   (padded        :accessor get-padded        :initarg :padded)
   (active-stream :accessor get-active-stream :initarg :active-stream)
   (flags :accessor get-flags :initarg :flags)
   (end-of-stream :accessor get-end-of-stream :initarg :end-of-stream))
  (:default-initargs :stream-class 'async-stream))

(defun http2-hello (client vec)
  "Process client hello and set up the connection instance.

Next would be reading the OPTIONS frame."
  (process-client-hello client vec)
  (setf (client-application-data client)
        (make-instance 'async-server
                       :tls-server client))
  (set-next-action client #'check-options-process-header 9))

(defparameter *full-http-process-client-hello* #'http2-hello)

(defun wrap-http2-callback (callback)
  (lambda (client header)
    ;; TODO: catch errors such as connection error and handle them
    (let ((connection (client-application-data client)))
      (multiple-value-bind (fn size)
          (funcall callback connection header)
        (declare (type compiled-function fn)
                 (fixnum size))
        (set-next-action client (wrap-http2-callback fn) size)))))

(defmethod http2::queue-frame ((connection async-server) frame)
  (send-unencrypted-bytes (get-tls-server connection) frame 'payload))

(defun process-header-by-wrapping (client header)
  "Read next frame header and process it."
  (funcall (wrap-http2-callback #'http2::parse-frame-header) client header))

(defun check-options-process-header (client header)
  "Read next frame, check it is options frame, and process it."
  (assert (= (aref header 3) 4))
  (process-header-by-wrapping client header))

(defmethod peer-expects-settings-ack ((connection async-server))
  (let ((buffer (http2::make-octet-buffer 9)))
    (declare (dynamic-extent buffer))
    (send-unencrypted-bytes (get-tls-server connection)
                            (http2::write-frame-header-to-vector
                             buffer 0 0 http2::+settings-frame+ 1 0 nil)
                            'ack)))

(defmethod http2::peer-ends-http-stream ((stream async-stream))
  (with-slots (connection) stream
     (funcall (http2::find-matching-handler (get-path stream) connection) connection stream)))

(defmethod add-header ((connection async-server) stream (name string) value)
  (declare (ignore name value))
  nil)
