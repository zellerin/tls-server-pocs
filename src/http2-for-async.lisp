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

(defclass async-stream (http2::server-stream)
  ())

(defclass async-server (http2::server-http2-connection)
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
                       :tls-server client
                       ;; this class is defined in http2/tests, move to better place
                       :network-stream (make-instance 'http2::pipe-end-for-write
                                                      :write-buffer (make-array 4096 :fill-pointer 0
                                                                                     :element-type '(unsigned-byte 8)))))
  (set-next-action client #'check-options-process-header 9))

(defparameter *full-http-process-client-hello* #'http2-hello)

(defun wrap-http2-callback (callback)
  (lambda (client header)
    (multiple-value-bind (fn size)
        (funcall callback (client-application-data client) header)
      (declare (type compiled-function fn)
               (fixnum size))
      (set-next-action client (wrap-http2-callback fn) size))))

(defun process-header-by-wrapping (client header)
  "Read next frame header and process it."
  (funcall (wrap-http2-callback #'http2::parse-frame-header) client header))

(defun check-options-process-header (client header)
  "Read next frame and process it."
  (assert (= (aref header 3) 4))
  (process-header-by-wrapping client header))

(defun get-32bit (vector idx)
  (let ((stream-id 0))
    (setf (ldb (byte 8 24) stream-id) (aref vector idx)
          (ldb (byte 8 16) stream-id) (aref vector (incf idx))
          (ldb (byte 8 8) stream-id) (aref vector (incf idx))
          (ldb (byte 8 0) stream-id) (aref vector (incf idx)))
    stream-id))

(defun get-16bit (vector idx)
  (let ((stream-id 0))
    (setf
     (ldb (byte 8 8) stream-id) (aref vector idx)
     (ldb (byte 8 0) stream-id) (aref vector (incf idx)))
    stream-id))

(defun process-settings-content (client vector)
  (loop
    with connection = (client-application-data client)
    for idx from 0 to (1- (length vector)) by 6
    for identifier = (get-16bit vector idx)
    and value = (get-32bit vector (+ idx 2))
    for name = (find-setting-by-id identifier)
    ;;    An endpoint that receives a SETTINGS frame with any unknown or
    ;;    unsupported identifier MUST ignore that setting.
    when name
      do (set-peer-setting connection name value)
    finally (peer-expects-settings-ack connection)))

(defmethod peer-expects-settings-ack ((connection async-server))
  (let ((buffer (http2::make-octet-buffer 9)))
    (declare (dynamic-extent buffer))
    (send-unencrypted-bytes (get-tls-server connection)
                            (http2::write-frame-header-to-vector
                             buffer 0 0 http2::+settings-frame+ 1 0 nil)
                            'ack)))

(defmethod http2::peer-ends-http-stream ((stream async-stream))
  (let ((client (get-tls-server (get-connection stream)))
        (id-to-process (http2::get-stream-id stream)))
    (send-unencrypted-bytes client
                            (tls-server/async/tls::buffer-with-changed-stream tls-server/async/tls::*header-frame* id-to-process)
                            'response-headers)
    (send-unencrypted-bytes client
                            (tls-server/async/tls::buffer-with-changed-stream tls-server/async/tls::*data-frame* id-to-process)
                            'response-payload)))

(defmethod add-header ((connection async-server) stream (name string) value)
  (declare (ignore name value))
  nil)
