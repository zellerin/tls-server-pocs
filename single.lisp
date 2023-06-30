(in-package tls-server)

(defun wrap-to-tls (raw-stream key cert)
  "Establish server TLS connection over RAW-STREAM.

Use TLS KEY and CERT for server identity."
  (let ((tls-stream
          (cl+ssl:make-ssl-server-stream
           (usocket:socket-stream raw-stream)
           :certificate cert
           :key key)))
    tls-stream))

(define-condition go-away (serious-condition)
  ()
  (:documentation
   "Signalled when GOAWAY frame is recevied."))

(defvar *buffer* nil
  "Preallocated buffer for reading from stream")

(defun octetize (array)
  "Make an (unsigned-byte 8) vector/simple array of same content as ARRAY."
  (make-array (length array) :element-type '(unsigned-byte 8)
                             :initial-contents array))

(declaim ((function * (simple-array (unsigned-byte 8) *)) octetize))

(defun read-tls-sequence (stream buf start end)
  "Read octets from binary STREAM to BUF between START (inclusive) and
END (exclusive).

Return number of read octets; this can be less than (end-start) or even zero.

This is faster than CL+SSL out of box read-sequence, but cannot be mixed with
the other ways to read octets (read-byte etc)."
  (declare
   (fixnum start end)
   ((simple-array (unsigned-byte 8) *) buf))
  (handler-case
      (let ((handle (cl+ssl::ssl-stream-handle stream)))
        (cffi:with-pointer-to-vector-data (ptr buf)
          (cl+ssl::ensure-ssl-funcall
           stream #'plusp #'cl+ssl::ssl-read handle (cffi:inc-pointer ptr start)
           (- end start))))
    (cl+ssl::ssl-error-zero-return ()
      0)))

(defun write-tls-sequence (stream buf start end)
  "Write octets in the BUF from START till END to the STREAM."
  (declare
   (fixnum start end)
   ((simple-array (unsigned-byte 8) *) buf)
   (cl+ssl::ssl-stream stream))
  (let ((handle (cl+ssl::ssl-stream-handle stream)))
    (unless handle
      (error "output operation on closed SSL stream"))
    (cffi:with-pointer-to-vector-data (ptr buf)
      (cl+ssl::ensure-ssl-funcall stream #'plusp #'cl+ssl::ssl-write handle
                                  (cffi:inc-pointer ptr start)
                                  (- end start)))))

(locally (declare (optimize speed)
                  ((simple-array (unsigned-byte 8) (*)) *buffer*))

  (defun process-request (tls-stream)
    "Process a HTTP2 request naively: handle preface, and read frames till there is
  end of stream; write static response in that case."
    (read-client-preface tls-stream)
    (write-tls-sequence  tls-stream (octetize #(0 0 0 4 0 0 0 0 0)) 0 9) ; our settings / empty
    (write-tls-sequence  tls-stream (octetize #(0 0 0 4 1 0 0 0 0)) 0 9) ;ack
    (handler-case
        (loop
          with preface-sent
          for stream-to-send-response =  (read-frame-check-end-stream tls-stream)
          when stream-to-send-response
            do
               (send-response tls-stream stream-to-send-response))
      (go-away ()
        (print "Going away")
        (close tls-stream))))

  (defun buffer-with-changed-stream (buf stream-id)
    (declare ((simple-array (unsigned-byte 8) *) buf))
    (setf (aref buf 8) (ldb (byte 8 0) stream-id))
    (setf (aref buf 7) (ldb (byte 8 8) stream-id))
    (setf (aref buf 6) (ldb (byte 8 16) stream-id))
    (setf (aref buf 5) (ldb (byte 8 24) stream-id))
    buf)

  (defun send-response (tls-stream stream-id)
    (let ((header-stream (load-time-value (octetize #(0 0 11 1 4 0 0 0 0 136 15 16 135 73 124 165 137 211 77 31))))
          (data-stream (load-time-value (octetize #(0 0 2 0 1 0 0 0 0 65 66)))))
      (write-sequence (buffer-with-changed-stream header-stream stream-id) tls-stream)
      (write-sequence (buffer-with-changed-stream data-stream stream-id) tls-stream)
      (finish-output tls-stream)))

  (defun fully-read-array (stream vector to-read)
    "Read TO-READ octets to the octet VECTOR.

The idea is to replace this later by a single thread for reading from several
sources."
    (declare (fixnum to-read))
    (loop with read fixnum = 0
          do (setf read (read-tls-sequence stream vector read to-read))
          until (= read to-read)
          finally
             (return vector)))

  (defun read-client-preface (stream)
    (fully-read-array stream *buffer* 24))

  (defun read-frame-check-end-stream (stream)
    (let ((header (fully-read-array stream *buffer* 9)))
      (declare ((simple-array (unsigned-byte 8) *) header))
      (let* ((frame-size 0)
             (stream-id 0)
             (type (aref header 3))
             (has-end-stream-flag
               (and (plusp (logand (aref header 4) 1))
                    ;; only data frame and header frame has end-stream
                    (<= type 1))))
        (when (= type 7)
          (error 'go-away))
        (setf (ldb (byte 8 16) frame-size) (aref header 0)
              (ldb (byte 8 8) frame-size) (aref header 1)
              (ldb (byte 8 0) frame-size) (aref header 2))

        (setf (ldb (byte 7 23) stream-id) (aref header 5)
              (ldb (byte 8 16) stream-id) (aref header 6)
              (ldb (byte 8 8) stream-id) (aref header 7)
              (ldb (byte 8 0) stream-id) (aref header 8))
        (unless (>= 16384 frame-size)
          (error "Too big frame (~d)" frame-size))
        #+nil
        (format t "Stream id: ~d, size: ~d type ~d flags ~d~%" stream-id frame-size (aref header 3) (aref header 4))
        (fully-read-array stream *buffer* frame-size)
        (when has-end-stream-flag
          stream-id)))))

(cffi:defcallback select-h2-callback
    :int
    ((ssl :pointer)
     (out (:pointer (:pointer :char)))
     (outlen (:pointer :char))
     (in (:pointer :char))
     (inlen :int)
     (args :pointer))
  ;; this is basically reimplemented SSL_select_next_proto, but easier than to
  ;; use that one in ffi world.
  "Set ALPN to h2 if it was offered, otherwise to the first offered."
  (declare (ignore args ssl))
  #+nil
  (cffi:with-foreign-string ((server serverlen) (make-alpn-proto-string '("h2")))
    (ssl-select-next-proto out outlen server (print (1- serverlen)) in inlen)
    0)
  (loop for idx = 0 then (+ (cffi:mem-ref in :char idx) idx)
        while (< idx inlen)
        when (and (= (cffi:mem-ref in :char idx) 2)
                  (= (cffi:mem-ref in :char (+ 1 idx)) (char-code #\h))
                  (= (cffi:mem-ref in :char (+ 2 idx)) (char-code #\2)))
          do
             (setf
              (cffi:mem-ref outlen :char) 2
              (cffi:mem-ref out :pointer) (cffi:inc-pointer in (1+ idx)))
             (return 0)
        finally
           (return 1)))

cl+ssl::(define-ssl-function ("SSL_CTX_set_alpn_select_cb" ssl-ctx-set-alpn-select-cb)
  :void
  (ctx ssl-ctx)
  (alpn-select-cb :pointer)
  #+nil  (args :pointer))

(defun make-http2-tls-context ()
  "Make TLS context suitable for http2.

Practically, it means:
- ALPN callback that selects h2 if present,
- Do not request client certificates
- Do not allow ssl compression adn renegotiation.
We should also limit allowed ciphers, but we do not."
  (let ((context
          (cl+ssl:make-context
           ;; Implementations of HTTP/2 MUST use TLS
           ;; version 1.2 [TLS12] or higher for HTTP/2
           ;; over TLS.
           :min-proto-version cl+ssl::+TLS1-2-VERSION+

           :options (list #x20000       ; +ssl-op-no-compression+
                          cl+ssl::+ssl-op-all+
                          #x40000000)   ; no renegotiation
           ;; do not requiest client cert
           :verify-mode cl+ssl:+ssl-verify-none+)))
    (cl+ssl::ssl-ctx-set-alpn-select-cb
     context
     (cffi:get-callback 'select-h2-callback))
    context))

(defun measure-and-call (measure fn &rest args)
  (ecase measure
    ((nil) (apply fn args))
    ((:alloc :time :cpu)
     (sb-sprof:with-profiling (:mode measure) (apply fn args)))
    ((t) (time (apply fn args)))))

(defun single-client-server (port &key
                                    (announce-open-fn (constantly nil))
                                    (host "127.0.0.1")
                                    (topdir (asdf:component-pathname (asdf:find-system "tls-server")))
                                    (key (namestring (merge-pathnames "certs/server.key" topdir)))
                                    (cert (namestring (merge-pathnames "certs/server.crt" topdir))))

  (cl+ssl:with-global-context ((make-http2-tls-context) :auto-free-p t)
    (restart-case
        (usocket:with-server-socket (socket (usocket:socket-listen host port
                                                                   :reuse-address t
                                                                   :backlog 200
                                                                   :element-type '(unsigned-byte 8)))
          (cl+ssl:with-global-context ((make-http2-tls-context) :auto-free-p t)
            (funcall announce-open-fn socket)
            (loop for raw = (handler-case
                                (usocket::socket-accept socket :element-type '(unsigned-byte 8))
                              (usocket:connection-aborted-error ()))
                  for tls = (wrap-to-tls raw key cert)
                  do
                     (let ((*buffer* (make-array 16385
                                                 :element-type '(unsigned-byte 8)
                                                 :initial-element 0)))
                       (measure-and-call :alloc #'process-request tls)))))

      (kill-server (&optional value)
        :report "Kill server"
        value))))
