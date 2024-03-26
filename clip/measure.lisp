(load "~/.sbclrc")
(asdf:load-asd  (merge-pathnames "../tls-server.asd" *load-pathname*))
(asdf:load-asd  (merge-pathnames "../../clip-1.4/clip.asd" *load-pathname*))
(ql:quickload 'tls-server)
(ql:quickload 'clip)

(cl:defpackage #:tls-server/measure
  (:use :tls-server :clip :cl))

(in-package #:tls-server/measure)

(defvar *res* nil
  "Text output of a binary program (e.g., h2load")

(defvar *url* "https://localhost:8443")

(define-simulator load-system
  :system-name "Load test of TLS server"
  :start-system
  (let ((thread (bt:make-thread
                 (lambda ()
                   (ignore-errors (tls-server:create-server 8443 tls implementation)))
                 :name "Async TLS server")))
    (unwind-protect
         (setf *res* (uiop:run-program
                      `("h2load" ,*url*
                                 "-D" ,(princ-to-string duration)
                                 "-m" ,(princ-to-string multi-threads)
                                 "-c" ,(princ-to-string clients)
                                 "--warm-up-time" "100ms"
                                 "-T" "1"
                                 "-N" "1")
                      :output :string :ignore-error-status t))
      (bt:interrupt-thread thread #'kill-server)
      (bt:join-thread thread))))


;;; Following clips parse specific lines of standard output of h2load
(defclip h2load-parsed-requests ()
  (:components (total started done succeeded failed errored timeout))
  (multiple-value-bind (match val)
      (cl-ppcre:scan-to-strings "requests: ([0-9]+) total, ([0-9]+) started, ([0-9]+) done, ([0-9]+) succeeded, ([0-9]+) failed, ([0-9]+) errored, ([0-9]+) timeout" (princ *res*))
    (if match
        (apply 'values (map 'list 'identity val))
        (values 0 0 0 0 0 0 0))))

(defclip h2load-parsed-fin () (:components (finished req/s MB/s))
  (multiple-value-bind (match val)
      (cl-ppcre:scan-to-strings "finished in ([0-9.s]+), ([0-9.]+) req/s, (.*)" (princ *res*))
    (if match
        (apply 'values (map 'list 'identity val))
        (values 0 0 0))))

(defclip h2load-parsed-reqtime () (:components (tmin tmax mean sd pm))
  (multiple-value-bind (match val)
      (cl-ppcre:scan-to-strings "time for request: +([^ ]+) +([^ ]+) +([^ ]+) +([^ ]+) +([^ ]+)%" (princ *res*))
    (if match
        (apply 'values (map 'list 'identity val))
        (values 0 0 0 0 0))))

(define-experiment vary-threads (duration clients tls)
  :simulator load-system
  :instrumentation (h2load-parsed-requests h2load-parsed-reqtime h2load-parsed-fin)
  :variables ((multi-threads '(1 2 5 10 20 30 40 50 60 70 80 90 100))
              (implementation '(:async-custom :thread :async :none)))
  :after-trial (write-current-experiment-data))

(let ((clasp-file (merge-pathnames "data.clasp" (or *load-pathname* "/tmp/"))))
  (when (probe-file clasp-file)
    (delete-file clasp-file))
  (handler-bind ((error (lambda (e) (print e) (clip:shutdown-and-run-next-trial))))
    (clip:run-experiment 'vary-threads :args '(1 1 :tls) :output-file clasp-file)
    (clip:run-experiment 'vary-threads :args '(1 5 :tls) :output-file clasp-file)
    (let ((*url* "http://localhost:8443"))
      (clip:run-experiment 'vary-threads :args '(1 1 nil) :output-file clasp-file))))
