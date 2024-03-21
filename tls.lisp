(in-package :tls-server/mini-http2)

(mgl-pax:defsection @tls (:title "TLS")
  "HTTP/2 is deeply connected with \\TLS. This interaction is represented with
function WRAP-TO-TLS"
  (wrap-to-tls function)
  (*http2-tls-context* variable)
  (make-http2-tls-context function)
                                        ; Note & TODO: no object for cffi callbacks
  #+nil(select-h2-callback function))

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

(cl+ssl::define-ssl-function ("SSL_CTX_use_certificate_chain_file" ssl-ctx-use-certificate-chain-file)
  :int
  (ctx cl+ssl::ssl-ctx)
  (filename :string))

(cl+ssl::define-ssl-function ("SSL_CTX_use_PrivateKey_file" ssl-ctx-use-private-key-file)
  :int
  (ctx cl+ssl::ssl-ctx)
  (filename :string)
  (type :int))

(defun make-http2-tls-context ()
  "make a \\TLS context suitable for http2.

practically, it means:

- ALPN callback that selects h2 if present,
- do not request client certificates
- do not allow ssl compression adn renegotiation.
we should also limit allowed ciphers, but we do not."
  (let ((context
          (cl+ssl:make-context
           ;; implementations of http/2 must use tls
           ;; version 1.2 [tls12] or higher for http/2
           ;; over tls.
           :min-proto-version cl+ssl::+tls1-2-version+

           :options (list #x20000       ; +ssl-op-no-compression+
                          cl+ssl::+ssl-op-all+
                          #x40000000)   ; no renegotiation
           ;; do not request client cert
           :verify-mode cl+ssl:+ssl-verify-none+)))
    ;; (cl+ssl::ssl-ctx-ctrl context +ssl-ctrl-set-read-ahead+ 0 (cffi:null-pointer))
    (ssl-ctx-set-alpn-select-cb
     context
     (cffi:get-callback 'select-h2-callback))
    (let ((topdir (asdf:component-pathname (asdf:find-system "tls-server"))))
      (print (ssl-ctx-use-certificate-chain-file context (namestring (merge-pathnames "certs/server.crt" topdir))))
      (print (ssl-ctx-use-private-key-file context (namestring (merge-pathnames "certs/server.key" topdir)) cl+ssl::+ssl-filetype-pem+)))
    context))

(defvar *http2-tls-context* (make-http2-tls-context))

(defun wrap-to-tls (raw-stream)
  "Return a binary stream representing TLS server connection over RAW-STREAM.

Use TLS KEY and CERT for server identity, and *HTTP2-TLS-CONTEXT* for the contex.

This is a simple wrapper over CL+SSL."
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
           (frame-size start end)
           (tls-server/utils:octet-vector buf))
  (let ((handle (cl+ssl::ssl-stream-handle stream)))
    (handler-case
        (cffi:with-pointer-to-vector-data (ptr buf)
          (cl+ssl::ensure-ssl-funcall
           stream #'plusp #'cl+ssl::ssl-read handle (cffi:inc-pointer ptr start)
           (- end start)))
      (cl+ssl::ssl-error-zero-return () ;SSL_read returns 0 on end-of-file
        start))))
