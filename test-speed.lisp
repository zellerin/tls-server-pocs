(load "~/quicklisp/setup")
(asdf::load-asd (truename "./tls-server.asd"))
(ql:quickload "tls-server/test" :silent t)
(tls-server/tests::test-single)
(tls-server/tests::test-plain-nonblock)
