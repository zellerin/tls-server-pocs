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

(defclass my-vanilla-client-stream (http2::gzip-decoding-mixin http2::vanilla-client-stream)
  ((broken-char :accessor get-broken-char :initarg :broken-char :initform nil)))

(defclass my-client (http2:vanilla-client-connection)
  ()
  (:default-initargs :stream-class 'my-vanilla-client-stream))

(defun utf-is-first-char (octet)
  (not (= (logand octet #xc0) #x80)))

(defun utf-first-char-size (octet)
  (cond
    ((= #x00 (logand octet #x80)) 1)
    ((= #xc0 (logand octet #xe0)) 2)
    ((= #xe0 (logand octet #xf0)) 3)
    ((= #xf0 (logand octet #xf8)) 4)
    ;; 20240607 TODO: Better UTF error
    (t (error "UTF error"))))

(defmethod http2:apply-data-frame ((stream my-vanilla-client-stream) payload start end)
  ;; Handle correctly broken (into pieces) multi-octet letters
  ;;
  ;; 1) find first start char, and if it is not in the
  ;; beginning, add all before it to the broken-char
  ;; this might be slow but rare enough
  (let* ((first-start-char-position
           (position-if #'utf-is-first-char payload :start start :end end)))
    (when (null first-start-char-position)
      (error "FIXME: no start-char in buffer"))
    (unless (= first-start-char-position start)
      ;; FIXME: print broken char
      (assert (get-broken-char stream))
      (princ (trivial-utf-8:utf-8-bytes-to-string
              (concatenate '(vector (unsigned-byte 8)) (get-broken-char stream)
                           (subseq payload start first-start-char-position))))
      (setf start first-start-char-position))
    ;; 2) find whether last starting char is fully in the buffer
    (let* ((last-start-char-position
             (position-if #'utf-is-first-char payload :from-end t
                                                      :start start
                                                      :end end))
           (last-start-char (aref payload last-start-char-position)))
      (unless (= (+ last-start-char-position (utf-first-char-size last-start-char))
                 end)
        (setf (get-broken-char stream) (subseq payload last-start-char-position end)
              end last-start-char-position))
      (princ (trivial-utf-8:utf-8-bytes-to-string payload :start start :end  end)))))

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
