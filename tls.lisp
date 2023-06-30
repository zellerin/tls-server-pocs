(in-package #:mini-http2)

(mgl-pax:defsection @tls ()
  (wrap-to-tls function))

(cl+ssl::define-ssl-function ("SSL_CTX_set_alpn_select_cb" ssl-ctx-set-alpn-select-cb)
  :void
  (ctx cl+ssl::ssl-ctx)
  (alpn-select-cb :pointer))

(defconstant +SSL-CTRL-SET-READ-AHEAD+ 41)

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
                                        ;    (cl+ssl::ssl-ctx-ctrl context +ssl-ctrl-set-read-ahead+ 0 (cffi:null-pointer))
    (ssl-ctx-set-alpn-select-cb
     context
     (cffi:get-callback 'select-h2-callback))
    context))

(defvar *http2-tls-context* (make-http2-tls-context))

(defun wrap-to-tls (raw-stream)
  "Establish server TLS connection over RAW-STREAM.

Use TLS KEY and CERT for server identity."
  (cl+ssl:with-global-context (*http2-tls-context* :auto-free-p nil)
    (let* ((topdir (asdf:component-pathname (asdf:find-system "tls-server")))
           (tls-stream
             (cl+ssl:make-ssl-server-stream
              (usocket:socket-stream raw-stream)
              :certificate
              (namestring (merge-pathnames "certs/server.crt" topdir))
              :key (namestring (merge-pathnames "certs/server.key" topdir)))))
      tls-stream)))

(defun read-tls-vector (buf stream start end)
  (declare (optimize speed)
           (mini-http2:frame-size start end)
           (mini-http2:octet-vector buf))
  (let ((handle (cl+ssl::ssl-stream-handle stream)))
    (handler-case
        (cffi:with-pointer-to-vector-data (ptr buf)
          (cl+ssl::ensure-ssl-funcall
           stream #'plusp #'cl+ssl::ssl-read handle (cffi:inc-pointer ptr start)
           (- end start)))
      (cl+ssl::ssl-error-zero-return () ;SSL_read returns 0 on end-of-file
        start))))
