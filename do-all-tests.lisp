;;;; Copyright 2022 by Tomáš Zellerin
(in-package cl-user)
(load "~/quicklisp/setup")

(defvar *asdf-system-name* "tls-server")
(defvar *package-name* *asdf-system-name*)

(setf ql:*local-project-directories* (list (truename "../")))
(setf asdf:*system-definition-search-functions*
      (list 'ql::local-projects-searcher
            'ql::system-definition-searcher))

(asdf:load-asd (truename (make-pathname :defaults "./foo.asd"
                                         :name *asdf-system-name*)))

(asdf:oos 'asdf:test-op *asdf-system-name* :silent t)
