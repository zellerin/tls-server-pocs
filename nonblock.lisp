(in-package tls-server/nonblock)

(mgl-pax:defsection @nonblock-server (:title "Nonblocking server")
  (tls-server/poll-dispatcher::client-data mgl-pax:class)
  (call-with-pollfds function))

(defstruct (connection-fd-info (:include fd-info)) buffer (bytes-read 0)
  bytes-needed)

(defun have-enough-bytes (fd-info)
  (let* ((needed (connection-fd-info-bytes-needed fd-info))
         (read-octets
           (if (zerop needed) 0
               (read-sequence
                (connection-fd-info-buffer fd-info)
                (tls-server/poll-dispatcher::fd-info-stream fd-info) :end needed))))
    (when (< read-octets needed)
      (format t "EOF detected, leaving~%")
      (invoke-restart 'go-away fd-info))
    t))

(defun process-new-request (client-data fd-info)
  (declare (ignore client-data))
  (when (have-enough-bytes fd-info)
    (let ((stream (fd-info-stream fd-info)))
      (write-sequence *settings-frame* stream) ; our settings / empty
      (write-sequence *ack-frame* stream)      ;ack
      (force-output stream))
    (setf (fd-info-read-action fd-info)  #'process-http2-frame-header
          (connection-fd-info-bytes-needed fd-info) 9)))

(defun process-http2-frame-header (client-data fd-info)
  (declare (ignore client-data) (optimize speed))
  (when (have-enough-bytes fd-info)
    (let ((header (connection-fd-info-buffer fd-info)))
      (declare (octet-vector header))
      (let* ((frame-size (get-frame-size header))
             (type (get-frame-type header)))
        (declare ((unsigned-byte 8) type)
                 (frame-size frame-size))
        (when (= type +goaway-frame-type+)
          (finish-output (fd-info-stream fd-info))
          (format t "got goaway frame, leaving~%")
          (invoke-restart 'go-away fd-info))
        (unless (>= 16384 frame-size)
          (format t "got too big frame, leaving~%")
          (invoke-restart 'go-away fd-info))
        (let ((id-to-process (get-stream-id-if-ends header)))
          (when id-to-process
            (mini-http2::send-response (fd-info-stream fd-info) id-to-process))
          (setf (fd-info-read-action fd-info)
                #'read-and-ignore-stream
                (connection-fd-info-bytes-needed fd-info) frame-size))))))

(defun read-and-ignore-stream (client-data fd-info)
  (declare (ignore client-data))
  (when (have-enough-bytes fd-info)
    (setf (fd-info-read-action fd-info)  #'process-http2-frame-header
          (connection-fd-info-bytes-needed fd-info) 9)))

(defun make-unitialized-connection-fd-info (socket)
    (make-connection-fd-info
     :read-action #'process-new-request
     :direction :input
     :stream socket
     :buffer (make-array 16385
                         :element-type '(unsigned-byte 8)
                         :initial-element 0)
     :bytes-needed +client-preface-length+))

(defun accept-connection (client-data fd-info)
  (let ((plain (usocket:socket-accept
                (fd-info-stream fd-info)
                :element-type '(unsigned-byte 8))))
    (let ((fd (sb-bsd-sockets::socket-file-descriptor
               (usocket:socket plain))))
      (add-fd client-data fd
              (make-unitialized-connection-fd-info
               (usocket:socket-stream plain)))
      nil)))

(defun make-server-socker-info (stream)
  (make-fd-info :read-action #'accept-connection
                :direction :input
                :stream stream))

(defun run-h2-with-fd (client-data listening-socket announce-open-fn)
  (add-fd client-data (sb-bsd-sockets::socket-file-descriptor
                       (usocket:socket listening-socket))
          (make-server-socker-info listening-socket))
  (funcall announce-open-fn listening-socket)
  (unwind-protect
       (let ((*buffer* (make-array 16385
                                   :element-type '(unsigned-byte 8)
                                   :initial-element 0)))
         (loop
           (restart-case
               (wait-for-fd client-data)
             (go-away (fd-info)
               (remove-fd client-data fd-info)
               (handler-case
                   (close (fd-info-stream fd-info))
                 (sb-int:broken-pipe ()
                   ; ignore, we were slower
                   )
                 (condition (c) (describe c)))))))))

(defmethod do-new-connection (listening-socket tls (dispatch-method (eql :poll)))
  "Handle the incoming connections in one thread, polling.

Poll all clients and listening socket, handle received frame and poll again.

Malicious client can block all other clients, e.g., by sending just part of the
frame header."
  (call-with-pollfds #'run-h2-with-fd 10 listening-socket (constantly nil)))
