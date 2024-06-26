(in-package tls-server/async)

(mgl-pax:defsection @async
    (:title "CL-ASYNC based implementation (asynchronous)")
  (do-new-connection (method () (t (eql nil) (eql :async))))
  (run-uv-server function))
"How to process data:
- fixed data size needed: either we got enough and process, or we got less and
  we allocate array to process it."

(defstruct client-data octets index callback)

(defun ensure-data (socket new-data needed)
  "Provide NEEDED data octets from both SOCKET-DATA and NEW-DATA.

Streamlined for cases where the data \"fit\" exactly needed amount, and event in
that case not optimized."
  (let* ((old-data (client-data-octets (socket-data socket)))
         (new-data-size (length new-data)))
    (when (>= (length old-data) needed)
      (cerror "Ok." "This should not happen."))
    (cond
      ((and (null old-data) (= new-data-size needed))
       new-data)
      ((and (null old-data) (> new-data-size needed))
       ;; we got more than we asked for
       (setf (client-data-octets (socket-data socket)) (subseq  new-data needed))
       (subseq new-data 0 needed))
      (t (error "Not handled yet: old ~a, needed ~d, got ~d"
                old-data needed new-data-size)))))

(defun on-connect (socket)
  "Send server side initial data.

Yes, sending ACK before we get the settings frame is cheating, but we dont
really care about the settings."
  (setf (socket-data socket) (make-client-data :callback #'expect-client-preface))
  (write-socket-data socket #.(concatenate '(vector (unsigned-byte 8)) *settings-frame* *ack-frame*)))

(defun callback (socket bytes)
  (funcall (client-data-callback (socket-data socket)) socket bytes))

(defun run-or-set-callback (socket fn)
  "We processed something and we want to run FN next.

Possibly there are some leftovers in socket-data, so we either process and clear
them, or wait for new data."
  (let ((old-data (client-data-octets (socket-data socket))))
    (cond (old-data
           (setf (client-data-octets (socket-data socket)) nil)
           (funcall fn socket old-data))
          (t
           (setf (client-data-callback (socket-data socket)) fn)))))

(defun async-read-and-ignore-stream (socket frame-size)
  "Read FRAME-SIZE octets from socket, possibly in pieces, and ensure frame header
is read next.

Originally, we have some SOCKET-DATA available, they can be read. Then, we might
 need to read more in chunks."
  (flet ((chunk-reader (socket data)
           (let ((length (length data)))
             (cond
               ((< length frame-size)   ; do nothing, we need more
                (decf frame-size (length data)))
               ((= length frame-size)
                (run-or-set-callback socket #'read-frame-header))
               (t
                (setf (client-data-octets (socket-data socket)) (subseq data frame-size))
                (run-or-set-callback socket #'read-frame-header))))))
    ;; first process existing socket-data and clean it.
    (let ((data nil))
      (rotatef data (client-data-octets (socket-data socket)))
      (cond ((= (length data) frame-size)
             #'read-frame-header)
            ((> (length data) frame-size)
             (setf (client-data-octets (socket-data socket))
                   (subseq data frame-size))
             #'read-frame-header)
            (t
             (funcall #'chunk-reader socket data)
             #'chunk-reader)))
))

(defun read-frame-header (socket new-data)
  (let ((header (ensure-data socket new-data 9)))
    (let* ((frame-size (get-frame-size header))
           (type (get-frame-type header)))
      (declare ((unsigned-byte 8) type)
               (frame-size frame-size))
      (when (= type +goaway-frame-type+)
        (close-socket socket))
      (unless (>= 16384 frame-size)
        (format t "got too big frame, leaving~%")
        (format t "~s~%" header)
        (break  "Too big header! go-away"))
      (let ((id-to-process (get-stream-id-if-ends header)))
        (when id-to-process
          (cl-async:delay
           (lambda ()
             (write-socket-data socket (buffer-with-changed-stream *header-frame* id-to-process))
             (write-socket-data socket (buffer-with-changed-stream *data-frame* id-to-process))))))
      (run-or-set-callback socket
                           (async-read-and-ignore-stream socket frame-size)))))

(defun expect-client-preface (socket new-data)
  (let ((data (ensure-data socket new-data +client-preface-length+)))
    (unless (null (mismatch +client-preface-start+ data))
      (error "Bad prefix")))
  (run-or-set-callback socket #'read-frame-header))

(defun run-uv-server (&key (port 2022))
  (start-event-loop
   (lambda ()
     (describe (tcp-server nil port
                           #'expect-client-preface
                           :connect-cb #'on-connect
                           :event-cb (lambda (err) (format t "--> ~s~%" err)))))))

#+sbcl
(defmethod do-new-connection (socket (tls (eql nil)) (dispatch-method (eql :async)) &key)
  "Handle new connections using cl-async event loop.

Pros: This version can be run in one thread and process many clients.

Cons: requires a specific C library, and the implementation as-is depends on
SBCL internal function - we re-use the file descriptor of socket created by
usocket package, as otherwise access to the port of server is complicated."
  (start-event-loop
   (lambda ()
     (tcp-server nil nil
                 #'callback
                 :connect-cb #'on-connect
                 :event-cb (lambda (err) (format t "--> ~s~%" err))
                 :fd (sb-bsd-sockets:socket-file-descriptor
                      (usocket:socket socket)))))
  (kill-server))

(defmethod do-new-connection (socket (tls (eql :tls)) (dispatch-method (eql :async)) &key)
  "Handle new connections using cl-async event loop.

Pros: This version can be run in one thread and process many clients.

Cons: requires a specific C library, and the implementation as-is depends on
SBCL internal function - we re-use the file descriptor of socket created by
usocket package, as otherwise access to the port of server is complicated."
  (let ((ctx (tls-server/mini-http2::make-http2-tls-context)))
    ;; FIXME: leaks ctx?
    (start-event-loop
     (lambda ()
       (cl-async-ssl:tcp-ssl-server nil nil
                                    #'callback
                                    :connect-cb #'on-connect
                                    :ssl-ctx ctx
                                    :event-cb (lambda (err) (format t "--> ~s~%" err))
                                    :fd (sb-bsd-sockets:socket-file-descriptor
                                         (usocket:socket socket))))))
  ;; there is an outer loop in create-server that we want to skip
  (invoke-restart 'kill-server))

(defmethod do-new-connection (socket (tls (eql :nonblock)) (dispatch-method (eql :async)) &key)
  "Handle new connections using cl-async event loop.

Pros: This version can be run in one thread and process many clients.

Cons: requires a specific C library, and the implementation as-is depends on
SBCL internal function - we re-use the file descriptor of socket created by
usocket package, as otherwise access to the port of server is complicated."
  (let ((ctx (tls-server/mini-http2::make-http2-tls-context)))
    (start-event-loop
     (lambda ()
       (cl-async-ssl:tcp-ssl-server nil nil
                                    #'callback
                                    :connect-cb #'on-connect
                                    :ssl-ctx ctx
                                    :event-cb (lambda (err) (format t "--> ~s~%" err))
                                    :fd (sb-bsd-sockets:socket-file-descriptor
                                         (usocket:socket socket))))))
  (invoke-restart 'kill-server)   ; there is an outer loop in create-server that
                                        ; we want to skip
  )
