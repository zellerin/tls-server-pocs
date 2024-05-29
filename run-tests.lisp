#!/usr/bin/sbcl --script

(load "~/quicklisp/setup")
#+sbcl (ql:quickload 'sb-cover)
#+sbcl (declaim (optimize sb-cover:store-coverage-data))
(ql:quickload 'mgl-pax)

(defvar *old-mgl-pax-fn*  (macro-function 'mgl-pax:defsection))

(defmacro mgl-pax:defsection (&whole body &rest args &environment env)
  `(locally (declare (optimize (sb-cover:store-coverage-data 0)))
     ,(funcall *old-mgl-pax-fn* body env)))

(asdf:load-asd "tls-server.asd")

(asdf:oos 'asdf:load-op :tls-server :force t)
(asdf:oos 'asdf:load-op :tls-server/clip)

(compile-file "src/server.lisp")
(load "src/server.lisp")



(asdf:oos 'asdf:test-op 'tls-server)
(mgl-pax:update-asdf-system-html-docs tls-server:@index "tls-server")

#+sbcl (handler-bind ((warning #'muffle-warning))
         (sb-cover:report (merge-pathnames
                           "cover-report/" (asdf:system-source-directory "tls-server"))))
