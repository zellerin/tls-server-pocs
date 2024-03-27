(require 'sb-sprof)
(load "~/.sbclrc")
(asdf:load-asd  (merge-pathnames "../tls-server.asd" *load-pathname*))
(asdf:load-asd  (merge-pathnames "../../clip-1.4/clip.asd" *load-pathname*))
(ql:quickload 'tls-server)
(ql:quickload 'clip)

(cl:defpackage #:tls-server/measure
  (:use :tls-server :clip :cl))

(in-package #:tls-server/measure)

(defvar *res*)

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


(define-simulator load-system-profiled
  :system-name "Load test of TLS server"
  :start-system
  (let ((thread (bt:make-thread
                 (lambda ()
                   (ignore-errors (tls-server:create-server 8443 tls implementation)))
                 :name "Async TLS server")))
    (sleep 0.1)
    (unwind-protect
         (sb-sprof:start-profiling :mode :time :threads (list thread))
      (setf *res* (uiop:run-program
                   `("h2load" "https://localhost:8443/"
                              "-D" ,(princ-to-string duration)
                              "-m" ,(princ-to-string multi-threads)
                              "-c" ,(princ-to-string clients)
                              "--warm-up-time" "100ms"
                              "-T" "1"
                              "-N" "1")
                   :output :string :ignore-error-status nil
;                   :error-output *standard-output*
                   ))
      (bt:interrupt-thread thread #'kill-server)
      (bt:join-thread thread))))

(define-experiment just-one (duration clients tls implementation multi-threads)
  :system-version (format nil "~a secs, ~a, ~a client~:p, ~a pipeline size" duration implementation clients multi-threads)
  :simulator load-system-profiled
  :instrumentation (h2load-parsed-requests h2load-parsed-reqtime h2load-parsed-fin)
  :after-trial (write-current-experiment-data))

(when (probe-file "/tmp/perf.txt")
  (delete-file "/tmp/perf.txt"))

;(run-experiment 'tls-server/measure::just-one :args '(1 1 :tls :none 120) :output-file "/tmp/perf.txt" :repetitions 2)
;(run-experiment 'tls-server/measure::just-one :args '(1 1 :tls :thread 120) :output-file "/tmp/perf.txt" :repetitions 2)
(sb-sprof:reset)
(run-experiment 'tls-server/measure::just-one :args '(1 1 :tls :async-custom 120) :output-file "/tmp/perf.txt" :repetitions 5)

(with-open-file (out "/tmp/slow.perf" :direction :output :if-exists :supersede)
  (sb-sprof:report :stream out))

#+nil (run-experiment 'tls-server/measure::just-one :args '(1 1 :tls :async-custom 5)
                                              :output-file "/tmp/perf.txt" :repetitions 2)
