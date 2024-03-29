(cl:defpackage #:tls-server/measure
  (:use :tls-server :clip :cl))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'sb-sprof))

(in-package #:tls-server/measure)

(defvar *res*)
(defvar *url* "https://localhost:8443")

(defun implementation-port (implementation tls)
  (unless (and (eq implementation :async-custom)
               (eq tls nil))
    (+
     (ecase implementation
       (:async-custom 0)
       (:async 1)
       (:thread 2)
       (:none 3))
     (if tls 8443 8080))))

(defun run-simulation (duration multi-threads clients tls implementation)
  (setf *res*
        (if (or (and (> clients 1) (eql implementation :none))
                (null (implementation-port implementation tls)))
            ""
            (uiop:run-program
             `("h2load" ,(princ-to-string
                          (tls-server::url-from-port
                           (implementation-port implementation tls)
                           "localhost" tls))
                        "-D" ,(princ-to-string duration)
                        "-m" ,(princ-to-string multi-threads)
                        "-c" ,(princ-to-string clients)
                        "--warm-up-time" "100ms"
                        "-T" "1"
                        "-N" "1")
             :output :string :ignore-error-status nil))))

;;;; Simulations
(define-simulator load-system
  :system-name "Load test of TLS server"
  :start-system (run-simulation duration multi-threads clients tls implementation))


;;;; Clips for h2load
;;; Following clips parse specific lines of standard output of h2load
(defclip h2load-parsed-requests ()
  (:components (total started done succeeded failed errored timeout))
  (multiple-value-bind (match val)
      (cl-ppcre:scan-to-strings "requests: ([0-9]+) total, ([0-9]+) started, ([0-9]+) done, ([0-9]+) succeeded, ([0-9]+) failed, ([0-9]+) errored, ([0-9]+) timeout" *res*)
    (if match
        (apply 'values (map 'list 'identity val))
        (values 0 0 0 0 0 0 0))))

(defclip h2load-parsed-fin () (:components (finished req/s MB/s))
  (multiple-value-bind (match val)
      (cl-ppcre:scan-to-strings "finished in ([0-9.s]+), ([0-9.]+) req/s, (.*)" *res*)
    (if match
        (apply 'values (map 'list 'identity val))
        (values 0 0 0))))

(defclip h2load-parsed-reqtime () (:components (tmin tmax mean sd pm))
  (multiple-value-bind (match val)
      (cl-ppcre:scan-to-strings "time for request: +([^ ]+) +([^ ]+) +([^ ]+) +([^ ]+) +([^ ]+)%" *res*)
    (if match
        (apply 'values (map 'list 'identity val))
        (values 0 0 0 0 0))))

;;;; Clips for perf
(defclip perf-report ()
  "Performance report as a string."
  ()
  (prin1-to-string (with-output-to-string (out)
                     (sb-sprof:report :stream out ))))

(defclip perf-rpw () ()
  (multiple-value-bind (match val)
      (cl-ppcre:scan-to-strings " *[0-9]+ +([0-9.]+) [ 0-9.]*foreign function poll" (perf-report))
    (when match (aref val 0))))


;;;; FDSET clips
(defclip encrypt-buffer-size (client) ()
  (let ((client (or (car tls-server/async/tls::*clients*) 'dummy)))
    (if (eql 'dummy client)
        -1
        (tls-server/async/tls::client-encrypt-buf-size client))))

(defclip write-buffer-size (client) ()
  (let ((client (or (car tls-server/async/tls::*clients*) 'dummy)))
    (if (eql 'dummy client)
        -1
        (reduce #'+ (tls-server/async/tls::client-write-buf client)
                :key #'length))))

(defclip encrypt-buffer-size-all () (:schedule (:period "1 s")
                                     :output-file "/tmp/queue.clasp"
                                     :components (encrypt-buffer-size write-buffer-size)
                                     :map-function (list 1)))


(defun schedule-in-threads (fn &rest args)
  (print args)
  (bt:make-thread (lambda ()
                    (loop (sleep 0.1)
                          (funcall fn)))
                  :name "Data collecting thread"))

(defun start-thread (tls implementation &optional (port 8443))
  (bt:make-thread
   (lambda ()
     (ignore-errors (tls-server:create-server port tls implementation)))
   :name (format nil "Server for ~a" implementation)))

(defun start-threads (tls implementations)
  (loop for implementation in implementations
        collect (start-thread tls implementation (implementation-port implementation tls))))

(defun stop-thread (thread)
  (ignore-errors
   ;; thread may already be finished - TODO: handle better
   (bt:interrupt-thread thread #'kill-server))
  (bt:join-thread thread))

(defun stop-threads (threads)
  (mapc #'stop-thread threads))

(define-experiment run-profiled  (duration tls implementation)
  :system-version (format nil "~a ~a running for ~a secs" tls implementation duration)
  :simulator load-system
  :variables ((clients '(1 5)) (multi-threads '(1 10 100)))

  :timestamp get-internal-real-time
  :schedule-function schedule-in-threads
  :deactivate-scheduled-function (lambda (thread) (bt:destroy-thread thread))

  :locals ((thread (start-thread tls implementation)))
  :instrumentation (h2load-parsed-requests h2load-parsed-reqtime h2load-parsed-fin
                                           encrypt-buffer-size-all)
  :after-trial (write-current-experiment-data)
  :after-experiment (stop-thread thread))
