(mgl-pax:define-package tls-server/async/http2
  (:use :cl :http2 :tls-server/async/tls)
  (:shadow #:client #:make-client-object))

(in-package tls-server/async/http2)

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
  (process-client-hello client vec)
  (setf (client-application-data client)
        (make-instance 'async-server :tls-server client))
  (set-next-action client #'check-options-process-header 9))

(defun next-action-wrapper (fn)
  (lambda (client data)
    (let* ((stream (make-instance 'http2::pipe-end-for-read  :buffer data :index 0))
           (connection (client-application-data client))
           (padded (get-padded connection))
           (length (length data))
           (padding-size (when padded (aref data 0))))
      (setf (http2::get-network-stream connection) stream)
      (when (and padded (>= padding-size length))
        (http2:connection-error 'too-big-padding connection))

      (funcall fn stream connection (get-active-stream connection)
               (if padded (- length 1 padding-size) length)
               (get-flags connection))
      (setf (http2::get-network-stream connection) client)
      (http2::maybe-end-stream (get-end-of-stream connection) (get-active-stream connection))
      (set-next-action client #'process-header-by-wrapping 9))))

(defun process-header-by-wrapping (client header)
  (multiple-value-bind (fn connection stream-or-connection length flags
                        padding-size end-of-stream)
      (http2::process-frame-header-only header (client-application-data client))
    (setf (get-active-stream connection) stream-or-connection
          (get-flags connection) flags
          (get-padded connection) padding-size
          (get-end-of-stream connection) end-of-stream)
    (cond
      ((zerop length)
       (funcall (next-action-wrapper fn) client #()))
      (t (set-next-action client (next-action-wrapper fn) length)))))

(defun check-options-process-header (client header)
  (assert (= (aref header 3) 4))
  (process-header-by-wrapping client header))

(defun process-settings-header (client header)
  (error "No longer used"))

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
  (let ((client (http2::get-network-stream (get-connection stream)))
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
