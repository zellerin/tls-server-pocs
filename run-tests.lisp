#!/usr/bin/sbcl --script

(load "~/quicklisp/setup")
#+sbcl (ql:quickload 'sb-cover)
#+sbcl (declaim (optimize sb-cover:store-coverage-data))
(ql:quickload 'mgl-pax)
(defvar *old-mgl-pax-fn*  (macro-function 'mgl-pax:defsection))

(defmacro mgl-pax:defsection (&whole body &rest args &environment env)
  `(locally (declare (optimize (sb-cover:store-coverage-data 0)))
     ,(funcall *old-mgl-pax-fn* body env)))

(with-open-file (*error-output* #P "errors.log" :direction :output
                                :if-exists :supersede)
  (let* ((*error-output* (make-broadcast-stream))
         (*trace-output* *error-output*)
         (*query-io* (make-two-way-stream (make-concatenated-stream) *error-output*))
         (*debug-io* *query-io*)
         (*terminal-io* *query-io*))

    (asdf:load-asd (merge-pathnames "tls-server.asd" *load-pathname*))
    (asdf:load-asd (merge-pathnames "../http2/http2.asd" *load-pathname*))
    (asdf:oos 'asdf:load-op :tls-server :force t)
    (asdf:oos 'asdf:load-op :tls-server/clip)

    (handler-bind ((uiop/run-program:subprocess-error #'continue))
      (asdf:oos 'asdf:test-op 'tls-server :silent t))))


(mgl-pax:update-asdf-system-html-docs tls-server:@index "tls-server")

#+sbcl (handler-bind ((warning #'muffle-warning))
         (sb-cover:report (merge-pathnames
                           "cover-report/" (asdf:system-source-directory "tls-server"))))
