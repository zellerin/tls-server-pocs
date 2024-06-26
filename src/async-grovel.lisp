(in-package #:tls-server/async/tls)
(include "poll.h")
(include "fcntl.h")
(include "sys/socket.h")
(include "netinet/in.h")
(include "netinet/tcp.h")
(include "errno.h")

(cstruct pollfd "struct pollfd"
         (fd "fd" :type :int)
         (events "events" :type :short)
         (revents "revents" :type :short))

(constant (f-setfl "F_SETFL"))
(constant (f-getfl "F_GETFL"))
(constant (o-nonblock "O_NONBLOCK"))
(constant (tcp-nodelay "TCP_NODELAY"))
(constant (tcp-cork "TCP_CORK"))
(constant (ipproto-tcp "IPPROTO_TCP"))

(constant (c-pollin "POLLIN"))
(constant (c-pollout "POLLOUT"))
(constant (c-pollerr "POLLERR"))
(constant (c-pollhup "POLLHUP"))
(constant (c-pollrdhup "POLLRDHUP") :optional t)
(constant (c-pollnval "POLLNVAL"))

(constant (eagain "EAGAIN"))

(include "openssl/ssl.h")
(constant (ssl-error-none "SSL_ERROR_NONE"))
(constant (ssl-error-want-write "SSL_ERROR_WANT_WRITE"))
(constant (ssl-error-want-read "SSL_ERROR_WANT_READ"))
(constant (ssl-error-ssl "SSL_ERROR_SSL"))
(constant (ssl-error-syscall "SSL_ERROR_SYSCALL"))
(constant (ssl-error-zero-return "SSL_ERROR_ZERO_RETURN"))

(constant (ssl-filetype-pem "SSL_FILETYPE_PEM"))
(constant (tls-1.2-version "TLS1_2_VERSION"))
(constant (ssl-op-all "SSL_OP_ALL"))

(constant (ssl-ctrl-set-min-proto-version "SSL_CTRL_SET_MIN_PROTO_VERSION"))

(constant (bio-flags-should-retry "BIO_FLAGS_SHOULD_RETRY"))
