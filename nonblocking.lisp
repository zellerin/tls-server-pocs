(mgl-pax:define-package tls-server/nonblock-tls
  (:use :cl))

(in-package tls-server/nonblock-tls)

(mgl-pax:defsection @nonblock-tls
    (:title "Nonblocking TLS over nonblocking TCP")
  (client-info struct)
  (make-client-info function)
  (cleanup-client-info function)
  (do-sock-read function)
  (do-sock-write function))

(cl+ssl::define-ssl-function ("BIO_s_mem" bio-s-mem)
  :pointer)

(cl+ssl::define-ssl-function ("BIO_write" bio-write)
  :int
  (bio :pointer)
  (data :pointer)
  (size :int))

(cl+ssl::define-ssl-function ("BIO_read" bio-read)
  :int
  (bio :pointer)
  (data :pointer)
  (size :int))

(cl+ssl::define-ssl-function ("BIO_should_retry" bio-should-retry)
  :int
  (bio :pointer))

(cl+ssl::define-ssl-function ("SSL_is_init_finished" ssl-is-init-finished)
  :int
  (ssl :pointer))

(cl+ssl::define-ssl-function ("read" native-read)
  :int
  (fd :int)
  (data :pointer)
  (size :int))

(cl+ssl::define-ssl-function ("write" native-write)
  :int
  (fd :int)
  (data :pointer)
  (size :int))

;;;; Simple non-blocking TLS server

;;;; Adapted from https://gist.github.com/darrenjs/4645f115d10aa4b5cebf57483ec82eca

(defconstant +default-buffer-size+ 64)

(defstruct (client-info (:constructor make-client-info%))
  "An instance of this object is created each time a client connection is
accepted. It stores the client file descriptor, the SSL objects, and data
which is waiting to be either written to socket or encrypted"
  fd
  ssl
  rbio ; /* SSL reads from, we write to. */ ;
  wbio ; /* SSL writes to, we read from. */ ;
  write-buf encrypt-buf
  data-available-callback)

(defun make-client-info (fd context callback)
  (let ((ssl  (cl+ssl::ssl-new context))
        (rbio (cl+ssl::bio-new (bio-s-mem)))
        (wbio (cl+ssl::bio-new (bio-s-mem))))
    (make-client-info% :fd fd
                       :data-available-callback callback
                       :rbio rbio
                       :wbio wbio
                       :ssl ssl
                       :write-buf (make-array +default-buffer-size+ :fill-pointer 0
                                              :adjustable t)
                       :encrypt-buf (make-array +default-buffer-size+ :fill-pointer 0
                                                                      :adjustable t))
    (cl+ssl::ssl-set-accept-state ssl)
    (cl+ssl::ssl-set-bio ssl rbio wbio)))

(defun cleanup-client-info (client-info)
  (cl+ssl::ssl-free client-info))


(defconstant SSL_ERROR_NONE                  0)
(defconstant SSL_ERROR_SSL                   1)
(defconstant SSL_ERROR_WANT_READ             2)
(defconstant SSL_ERROR_WANT_WRITE            3)
(defconstant SSL_ERROR_WANT_X509_LOOKUP      4)
(defconstant SSL_ERROR_SYSCALL               5) ; look at error stack/return value/errno */
(defconstant SSL_ERROR_ZERO_RETURN           6)
(defconstant SSL_ERROR_WANT_CONNECT          7)
(defconstant SSL_ERROR_WANT_ACCEPT           8)
(defconstant SSL_ERROR_WANT_ASYNC            9)
(defconstant SSL_ERROR_WANT_ASYNC_JOB       10)
(defconstant SSL_ERROR_WANT_CLIENT_HELLO_CB 11)
(defconstant SSL_ERROR_WANT_RETRY_VERIFY    12)

(defun get-ssl-status (ssl n)
  (case (cl+ssl::ssl-get-error ssl n)
    (#.SSL_ERROR_NONE :ok)
    ((#.SSL_ERROR_WANT_READ #.SSL_ERROR_WANT_WRITE :want-io))
    (t (error "FAIL"))))

(defun add-to-buffer (buf octets n)
  (let* (
         (new-size (+ (fill-pointer buf) n))
         (old-fp (fill-pointer buf)))
    (when (> new-size (array-total-size buf))
      (adjust-array buf new-size))
    (incf (fill-pointer buf) n)
    (replace buf octets :start1 old-fp)
    buf))

(defun send-encrypted-bytes (client octets n)
  (add-to-buffer (client-info-write-buf client) octets n))

(defun send-unencrypted-bytes (client octets n)
  (add-to-buffer (client-info-encrypt-buf client) octets n))

(defun ssl-client-want-write (client)
  (plusp (fill-pointer (client-info-write-buf client))))

(defun on-read-cb (client data data-size)
  "Process SSL bytes received from the peer. The data needs to be fed into the
SSL object to be unencrypted."
  (cffi:with-pointer-to-vector-data (ptr data)
    (loop with to-write = data-size
          with wbio = (client-info-wbio client)
          with buf = (make-array +default-buffer-size+ :element-type '(unsigned-byte 8))
          with n
          while (plusp to-write)
          do
             (when (minusp n)
               (error "Can't read!"))
             (setf n (bio-write wbio ptr to-write))
             (setf ptr (cffi:inc-pointer ptr n))
             (decf to-write n)
          when (zerop (ssl-is-init-finished (client-info-ssl client)))
            do
               (cffi:with-pointer-to-vector-data (buf-ptr buf)
                 (case (get-ssl-status
                        (cl+ssl::ssl-accept (client-info-ssl client)) n)
                   (:want-io
                    (loop
                      for n = (bio-read wbio buf-ptr +default-buffer-size+)
                      while (plusp n)
                      do
                         (send-encrypted-bytes client buf n)
                      finally
                         (unless (zerop (bio-should-retry wbio))
                           (error "FAIL #init")))))))))

(defun do-encrypt (info)
  "waiting data resides in encrypt_buf.  It needs to be passed into the SSL
object for encryption, which in turn generates the encrypted bytes that then
will be queued for later socket write."
  (cffi:with-pointer-to-vector-data (ptr (client-info-encrypt-buf info))
    (when (zerop (ssl-is-init-finished (client-info-ssl info)))
      (loop with idx = 0
            with ssl = (client-info-ssl info)
            with to-write = (length (client-info-encrypt-buf info))
            for n = (cl+ssl::ssl-write (client-info-ssl info) ptr to-write)
            for status = (get-ssl-status ssl n)
            when (plusp n)
              ;; consume the waiting bytes that have been used by SSL
              do (when (< n to-write)
                (decf to-write n)
                (cffi:inc-pointer ptr n))
              ;; take the output of the SSL object and queue it for socket write
              (loop with wbio = (client-info-wbio info)
                    with temp_buf  = (make-array 64 :element-type '(unsigned-byte 8))
                    for n = (cffi:with-pointer-to-vector-data (ptr2 temp_buf)
                              (bio-read wbio ptr2 64 ))
                    while (plusp n)
                    do (send-encrypted-bytes info temp_buf n)
                    finally (when (zerop (bio-should-retry wbio))
                              (error "Err 2")))
              when (zerop n)
                do (return 0)))))

(defun do-sock-read (socket data)
  (let ((buf (make-array 64 :element-type '(unsigned-byte 8))))
    (cffi:with-pointer-to-vector-data (ptr  buf)
      (let ((n (native-read (client-info-fd info) ptr 64)))
        (if (plusp n)
            (on-read-cb info buf n)
            (error "err 3"))))))

(defun do-sock-read* (info)
  (let ((buf (make-array 64 :element-type '(unsigned-byte 8))))
    (cffi:with-pointer-to-vector-data (ptr  buf)
      (let ((n (native-read (client-info-fd info) ptr 64)))
        (if (plusp n)
            (on-read-cb info buf n)
            (error "err 3"))))))

(defun send-octets (info octets)
  (send-unencrypted-bytes info octets (length octets)))

(defun do-sock-write (info)
  (let ((buf (client-info-write-buf info)))
    (cffi:with-pointer-to-vector-data (ptr buf)
      (let ((n (native-write (client-info-fd info) ptr 64)))
        (unless (plusp n)
          (error "err 4"))
        (setf (client-info-write-buf info) (subseq buf n))))))

#|

/* Load certificate and private key files, and check consistency  */
int err
err = SSL_CTX_use_certificate_file(ctx, "server.crt",  SSL_FILETYPE_PEM)
if (err != 1)
int_error("SSL_CTX_use_certificate_file failed")
else
printf("certificate file loaded ok\n")

/* Indicate the key file to be used */
err = SSL_CTX_use_PrivateKey_file(ctx, "server.key", SSL_FILETYPE_PEM)
if (err != 1)
int_error("SSL_CTX_use_PrivateKey_file failed")
else
printf("private-key file loaded ok\n")

/* Make sure the key and certificate file match. */
if (SSL_CTX_check_private_key(ctx) != 1)
int_error("SSL_CTX_check_private_key failed")
else
printf("private key verified ok\n")

/* Recommended to avoid SSLv2 & SSLv3 */
SSL_CTX_set_options(ctx, SSL_OP_ALL|SSL_OP_NO_SSLv2|SSL_OP_NO_SSLv3)
}
|#

#|
int main(int argc, char **argv)         ; ;
{                                       ; ;
                                        ; ;

struct pollfd fdset[2]                  ; ;
memset(&fdset, 0, sizeof(fdset))        ; ;
                                        ; ;
fdset[0].fd = STDIN_FILENO              ; ;
fdset[0].events = POLLIN                ; ;
                                        ; ;
ssl_init()                              ; ;
                                        ; ;
while (1) {                             ; ;
printf("waiting for next connection on port %d\n", port) ; ;
                                        ; ;
clientfd = accept(servfd, (struct sockaddr *)&peeraddr, &peeraddr_len) ; ;
if (clientfd < 0)                       ; ;
die("accept()")                         ; ;
                                        ; ;
ssl_client_init(&client)                ; ;
client.fd = clientfd                    ; ;
                                        ; ;
inet_ntop(peeraddr.sin_family, &peeraddr.sin_addr, str, INET_ADDRSTRLEN) ; ;
printf("new connection from %s:%d\n", str, ntohs(peeraddr.sin_port)) ; ;
                                        ; ;
fdset[1].fd = clientfd                  ; ;
                                        ; ;
/* event loop */                        ; ;
                                        ; ;
fdset[1].events = POLLERR | POLLHUP | POLLNVAL | POLLIN ; ;
#ifdef POLLRDHUP                        ; ;
fdset[1].events |= POLLRDHUP            ; ;
#endif                                  ; ;
                                        ; ;
while (1) {                             ; ;
fdset[1].events &= ~POLLOUT             ; ;
fdset[1].events |= (ssl_client_want_write(&client)? POLLOUT : 0) ; ;
                                        ; ;
int nready = poll(&fdset[0], 2, -1)     ; ;
                                        ; ;
if (nready == 0)                        ; ;
continue/* no fd ready */               ; ;
                                        ; ;
int revents = fdset[1].revents          ; ;
if (revents & POLLIN)                   ; ;
if (do_sock_read() == -1)               ; ;
break                                   ; ;
if (revents & POLLOUT)                  ; ;
if (do_sock_write() == -1)              ; ;
break                                   ; ;
if (revents & (POLLERR | POLLHUP | POLLNVAL)) ; ;
break                                   ; ;
                                        ; ;
#ifdef POLLRDHUP                        ; ;
if (revents & POLLRDHUP)                ; ;
break                                   ; ;
#endif                                  ; ;
                                        ; ;
if (fdset[0].revents & POLLIN)          ; ;
do_stdin_read()                         ; ;
                                        ; ;
if (client.encrypt_len>0)               ; ;
do_encrypt()                            ; ;
}                                       ; ;
                                        ; ;
close(fdset[1].fd)                      ; ;
ssl_client_cleanup(&client)             ; ;
}                                       ; ;
                                        ; ;
return 0                                ; ;
}                                       ; ;
@darrenjs                               ; ;
Author                                  ; ;
darrenjs commented Mar 22, 2017         ; ;
                                        ; ;
Also refer to https://github.com/darrenjs/openssl_examples which contains a client example. ; ;
@MaG21                                  ; ;
MaG21 commented Feb 16, 2018 •          ; ;
                                        ; ;
Where's the non-blocking setup snippet located ? I observed the code above and it seems it doesn't do any no-blocking IO whatsoever (in terms of what a socket descriptor concerns) but rather, it implements multiplexing using poll. am I wrong? ; ;
@darrenjs                               ; ;
Author                                  ; ;
darrenjs commented Mar 10, 2018         ; ;
                                        ; ;
correct .... any waiting of the main thread happens in the call to poll(). The thread will wait in there for any of the sockets to become ready to read or write. ; ;
@Mampoer                                ; ;
Mampoer commented Jun 27, 2018          ; ;
                                        ; ;
Would you be able to help with some openssl config issues? ; ;
@laybatin                               ; ;
laybatin commented Sep 20, 2018 •       ; ;
                                        ; ;
Thanks. It is a reference to my development. ; ;
so.. how can i close the ssl socket? graceful shutdown.. ; ;
ex) ssl_shutdown()                      ; ;
                                        ; ;
plz help me                             ; ;
@zhiming99                              ; ;
zhiming99 commented Nov 28, 2019        ; ;
                                        ; ;
at line 283, is it possible the error SSLSTATUS_WANT_IO means more bytes to read? ; ;
@darrenjs                               ; ;
Author                                  ; ;
darrenjs commented Nov 30, 2019         ; ;
                                        ; ;
Yes. Could be either SSL_ERROR_WANT_WRITE or SSL_ERROR_WANT_READ. If it wants write then the following block does the BIO_read. If it wants read, then we will stay within the outer while (len > 0) loop, so there will a further call to BIO_write. ; ;
@ramatin                                ; ;
ramatin commented Jun 27, 2020 •        ; ;
                                        ; ;
Where is the code to do the handshake: SSL_do_handshake? ; ;
@darrenjs                               ; ;
Author                                  ; ;
darrenjs commented Jul 29, 2020         ; ;
                                        ; ;
the handshake is initiated in the client (rather than in the server, which is shown above), see the client example at https://github.com/darrenjs/openssl_examples ; ;
@dionysus1016                           ; ;
dionysus1016 commented Sep 28, 2020     ; ;


|#
