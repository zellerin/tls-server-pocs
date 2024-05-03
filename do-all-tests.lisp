;;;; Copyright 2022 by Tomáš Zellerin
(in-package cl-user)
(load "~/quicklisp/setup")

#+sbcl
(eval-when (:execute :compile-toplevel :load-toplevel)
  (require 'sb-cover))

#+sbcl
(eval-when (:compile-toplevel :load-toplevel)
  (declaim (optimize sb-cover:store-coverage-data debug safety)))

#+sbcl (asdf:initialize-output-translations
         `(:output-translations
           (,(merge-pathnames "**/*.*" (asdf:system-source-directory "http2"))
             #p"/tmp/fasl/cache/**/*.*")
           :inherit-configuration :enable-user-cache))

;;;;


(defvar *asdf-system-name* "tls-server")
(defvar *package-name* *asdf-system-name*)

(setf ql:*local-project-directories* (list (truename "~../")))
(setf asdf:*system-definition-search-functions*
      (list 'ql::local-projects-searcher
            'ql::system-definition-searcher))

(asdf:load-asd (truename (make-pathname :defaults "./foo.asd"
                                         :name *asdf-system-name*)))

(asdf:oos 'asdf:test-op *asdf-system-name* :silent t)
#+sbcl (sb-cover:report "/tmp/cover/")
