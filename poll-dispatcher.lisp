(in-package tls-server/poll-dispatcher)

(mgl-pax:defsection @poll-dispatcher
    (:title "Interface to poll(2)")
  "Interface to poll:
- allocate C fdinfo array and on stack with CALL-WITH-POLLFDS,
- keep data relevant to each client in FD-INFO, possibly after extending it to suit needs of the application,
- Add new clients with ADD-FD
- Wait for activity with WAIT-FOR-FD
- Remove clients with REMOVE-FD,"
  (call-with-pollfds function)
  (fd-info class)
  (add-fd function)
  (wait-for-fd function)
  (remove-fd function)
  (fd-info-stream mgl-pax:structure-accessor)
  (fd-info-read-action mgl-pax:structure-accessor)
  (fd-info-buffer mgl-pax:structure-accessor)
  (fd-info-bytes-needed mgl-pax:structure-accessor)

  "For SBCL: SB-IMPL::FD-STREAM has IBUF slot that is SB-IMPL::BUFFER structure of size (* 8 1024) that has buffered read data; slots HEAD and TAIL indicate how much space is inside.

So what should work when we get data is to read once, and then check HEAD and TAIL if we have enough. Of course, we might have a problem if we need more than what fits the buffer. SB-IMPL::ALLOC-BUFFER can be used to allocate bigger buffer.")

#+nil(defun refill-buffer (ibuf fd)
  ;; Taken from SBCL sb-impl.lisp
  (let* ((sap (sb-impl::buffer-sap ibuf))
         (length (sb-impl::buffer-length ibuf))
         (head (sb-impl::buffer-head ibuf))
         (tail (sb-impl::buffer-tail ibuf)))
    (declare (fixnum length head tail)
             (inline sb-unix:unix-read))
    (unless (zerop head)
      (cond ((eql head tail)
             ;; Buffer is empty, but not at yet reset -- make it so.
             (setf head 0
                   tail 0)
             (reset-buffer ibuf))
            (t
             ;; Buffer has things in it, but they are not at the
             ;; head -- move them there.
             (let ((n (- tail head)))
               (sb-kernel:system-area-ub8-copy sap head sap 0 n)
               (setf head 0
                     (sb-impl::buffer-head ibuf) head
                     tail n
                     (sb-impl::buffer-tail ibuf) tail)))))
    (setf (values size errno)
          (sb-unix:unix-read fd (sb-sys::sap+ sap tail) (- length tail)))
    (cond ((null size)
           (if (eql ::errno
                    #+win32 sb-unix:eintr
                    #-win32 sb-unix:ewouldblock)
               (return :wait-for-input)
               (return :read-error)))
          ((zerop size)
           (invoke-restart 'go-away))
          (t
           ;; Success! (Do not use INCF, for sake of other threads.)
           (setf (buffer-tail ibuf) (+ size tail))))))

(defstruct (fd-info (:print-object print-fd-info))
  "Information related to a client. This includes:
- READ-ACTION and WRITE-ACTION that would be called when data for read are available,"
  fd (pollfd-index 0 :type fixnum)
  (read-action (constantly nil) :type compiled-function)
  (write-action (constantly nil) :type compiled-function)
  direction stream
  buffer bytes-needed)

(defun print-fd-info (object stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~d (~a)" (fd-info-fd object) (fd-info-stream object))))

(defcstruct (pollfd/c)
  (fd :int)
  (events :short)
  (revents :short))

(defcfun ("poll" unix-poll)
    :int
  (fds :pointer)
  (size :int)
  (timeout :int))

(defcfun ("read" unix-read)
    :int
  (fd :int)
  (buffer :pointer)
  (size :int))

(defstruct client-data
  "All data needed for polling.

POLLFD is allocated and filled array of POLLFD-SIZE pollfd objects to pass to poll.

SOCKET-LIST is list of information about monitored sockets.

AVAILABLE-LIST is list of unused indices in the POLLFD. It is used when adding
new fd to monitor."
  pollfd socket-list available-list pollfd-size)

(defun call-with-pollfds (action n &rest args)
  "Call ACTION with a new CLIENT-DATA object and additional ARGS.

Intention is to have and keep allocated pollfd array during the lifetime of the
call to ACTION, and clean it up afterwards."
  (with-foreign-object (v '(:struct pollfd/c) n)
    (dotimes (i n)
      (with-foreign-slots ((fd) (mem-aptr v '(:struct pollfd/c) i) (:struct pollfd/c))
        (setf fd -1)))
    (let ((client-data (make-client-data :pollfd v
                                         :available-list (alexandria:iota n)
                                         :pollfd-size n)))
      (unwind-protect
           (apply action client-data args)
        (client-data-cleanup-sockets client-data)))))

(defun add-fd (client-data new-fd fd-info)
  "Add client with NEW-FD to the CLIENT-DATA.

Add FD to an empty slot in pollfd array, with EVENTS value based on direction slot
  in FD-INFO,

Fix FD slot in FD-INFO to the NEW-FD.

Update list of connected clients."
  (let ((idx (pop (client-data-available-list client-data))))
    ;; it is possible to handle no fd available by reallocation, but maybe later?
    (assert idx () "Cannot allocate new idx")
    (with-foreign-slots ((fd events)
                         (mem-aptr (client-data-pollfd client-data) '(:struct pollfd/c) idx)
                         (:struct pollfd/c))
      (setf fd new-fd
            events (ecase (fd-info-direction fd-info)
                     (:input (logior 1))
                     (:output 4)))
      (setf (fd-info-fd fd-info) new-fd
            (fd-info-pollfd-index fd-info) idx)
      (push fd-info (client-data-socket-list client-data)))))

(defun remove-fd (client-data fd-info)
  "Remove monitored fd identified by FD-INFO from the CLIENT-DATA.

Remove the appropriate pollfd record, and make necessary bookkeeping in
CLIENT-DATA."
  (with-foreign-slots ((fd) (mem-aptr (client-data-pollfd client-data) '(:struct pollfd/c) (fd-info-pollfd-index fd-info))
                       (:struct pollfd/c))
    (setf fd -1))
  (push (fd-info-pollfd-index fd-info) (client-data-available-list client-data))
  (setf (client-data-socket-list client-data)
        (remove fd-info (client-data-socket-list client-data))))

(defun have-enough-bytes (fd-info needed)
  (map-into (fd-info-buffer fd-info) (constantly 0))
  (when (plusp needed)
    (let* ((read-octets
             (read-sequence
              (fd-info-buffer fd-info)
              (tls-server/poll-dispatcher::fd-info-stream fd-info) :end needed)))
      (cond ((zerop read-octets)
             (format t "EOF detected, leaving~%")
             (invoke-restart 'go-away fd-info))
            ((/= read-octets needed)
             (format t "Partial read detected, leaving~%")
             (invoke-restart 'go-away fd-info)))))
  t)

(defun enough-data-available (fd-info)
  "When we read more data than needed, is it enough for next chunk?"
  #-sbcl nil                            ; we do not know
  #+sbcl (when (tls-server/nonblock::connection-fd-info-p fd-info)
           (let ((ibuf (SB-IMPL::FD-STREAM-ibuf (fd-info-stream fd-info))))
             (and ibuf
                  (>= (- (sb-impl::buffer-tail ibuf)
                         (sb-impl::buffer-tail ibuf))
                      (tls-server/nonblock::connection-fd-info-bytes-needed fd-info))))))

(defun wait-for-fd (client-data)
  "Wait until any of monitored clients is available as required.

Call appropriate callback for such connection."
  (declare (optimize speed))
  (unix-poll (client-data-pollfd client-data)
             (client-data-pollfd-size client-data) -1)
  (loop for fd-info in (client-data-socket-list client-data)
        and bad = nil
        do
           (with-foreign-slots ((revents)
                                (mem-aptr (client-data-pollfd client-data)
                                          '(:struct pollfd/c) (fd-info-pollfd-index fd-info))
                                (:struct pollfd/c))
             (when (logtest revents 32)           ; pollnval
               (error "FD no longer available"))  ;; this should not happen
             #+nil      (when (logtest revents 8) ;sb-unix:pollhup
                          (remove-fd client-data fd-info))
             (when (logtest revents (logior 1 8 16)) ; sb-unix:pollin pollhup
               (handler-case
                   (when (have-enough-bytes fd-info (fd-info-bytes-needed fd-info))
                     (funcall (fd-info-read-action fd-info) client-data fd-info)
                     #+nil                     (loop while (print (enough-data-available fd-info))
                                                     do
                                                        (funcall (fd-info-read-action fd-info) client-data fd-info)))
                 (stream-error ()
                   (push fd-info bad))
                 (cl+ssl::ssl-error (e)
                   ;; ended by ssl error
                   (push fd-info bad)
                   (unless (member (type-of e) '(cl+ssl::ssl-error-syscall))
                     (describe e)))
                 (error (e)
                   (unless (member (type-of e) '(sb-int:broken-pipe))
                     (describe e)))))
             (when (logtest revents (logior 4)) ; sb-unix:pollin
               (funcall (fd-info-write-action fd-info) client-data fd-info)))
        finally
           (loop for fd-info in bad do
             (format t "Bad fd: ~a~%" fd-info)
             (remove-fd client-data fd-info)
             (close (fd-info-stream fd-info))
             (break))))

(defun client-data-cleanup-sockets (client-data)
  (loop for item in (client-data-socket-list client-data)
        when (typep (fd-info-stream item) 'stream)
          do (ignore-errors (close (fd-info-stream item)))))
