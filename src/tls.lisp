(in-package :tls-server/mini-http2)

(mgl-pax:defsection @tls (:title "TLS with CL+SSL")
  "HTTP/2 in most cases needs \\TLS as an underlying layer.

Synchronous implementations (and in general implementation that use streams) use
CL+SSL with a thin wrapper (WRAP-TO-TLS) to hide complexity of TLS.

We use MAKE-HTTP2-TLS-CONTEXT to prepare a context appropriate for the HTTP/2
communication. The context is stored in *HTTP2-TLS-CONTEXT*.

 to have (some)
parameters set up properly

Servers using usocket and Lisp streams use WRAP-TO-TLS to establish TLS."
  (make-http2-tls-context function)
  (wrap-to-tls function)
  (*http2-tls-context* variable)
  (select-h2-callback tls-server/utils::callback))

(cl+ssl::define-ssl-function ("SSL_CTX_set_alpn_select_cb" ssl-ctx-set-alpn-select-cb)
  :void
  (ctx cl+ssl::ssl-ctx)
  (alpn-select-cb :pointer))

(defconstant +SSL-CTRL-SET-READ-AHEAD+ 41)

(define-documented-callback select-h2-callback
    :int
    ((ssl :pointer)
     (out (:pointer (:pointer :char)))
     (outlen (:pointer :char))
     (in (:pointer :char))
     (inlen :int)
     (args :pointer))
  "To be used as a callback in SSL_CTX_set_alpn_select_cb.

Chooses h2 as ALPN if it was offered by the client, otherwise the first offered.

This is basically reimplemented SSL_select_next_proto, but easier than to use
that one in ffi world."
  (declare (ignore args ssl))
  ;; map over
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
  "Make a \\TLS context suitable for HTTP/2 as per RFC9113 9.2

Specifically:
- Minimal TLS version 1.2,
- ALPN callback that selects h2 if present,
- do not request client certificates,
- do not allow TLS compression and renegotiation.

We should also limit allowed ciphers, but we do not.

As required, We do not sent out post-handshake TLS 1.3 CertificateRequest messages.

We ignore early data for now.
"
  (let ((context
          (cl+ssl:make-context
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
      (ssl-ctx-use-certificate-chain-file context (namestring (merge-pathnames "certs/server.pem" topdir)))
      (ssl-ctx-use-private-key-file context (namestring (merge-pathnames "certs/server.key" topdir)) cl+ssl::+ssl-filetype-pem+))
    context))

(defvar *http2-tls-context* (make-http2-tls-context)
  "TLS context to use for our servers.")

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
              (namestring (merge-pathnames #P"certs/server.pem" topdir))
              :key (namestring (merge-pathnames #P"certs/server.key" topdir)))))
      tls-stream)))
