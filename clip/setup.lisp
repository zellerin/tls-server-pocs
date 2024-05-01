(mgl-pax:define-package #:tls-server/measure
  (:use :tls-server :clip :cl :mgl-pax))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'sb-sprof))

(in-package #:tls-server/measure)

(defsection tls-server::clips
    (:title "Clips")
  (data-moves clip-user::clip)
  (peer-in-out clip-user::clip)
  (curl clip-user::simulator)
  (curl clip-user::experiment))

(defvar *last-real-time*)

(defclip relative-times ()
  "Both CPU and real time"
  (:components (relative-run-time relative-real-time)))

(defclip relative-real-time ()
  "Real time since enabling or reset of the clip"
  (:enable-function (setf *last-real-time* (get-internal-real-time))
   :reset-function  (setf *last-real-time* (get-internal-real-time))
   :disable-function t
   :parent relative-times)
  (- (get-internal-real-time) *last-real-time*))

(defvar *last-run-time*)
(defclip relative-run-time ()
  "Real time since enabling or reset of the clip"
  (:enable-function (setf *last-run-time* (get-internal-run-time))
   :reset-function  (setf *last-run-time* (get-internal-run-time))
   :disable-function t
   :parent relative-times)
  (- (get-internal-run-time) *last-run-time*))

(defclip data-moves (client from to requested moved)
  "Collect data about data moves. When enabled, it puts appropriate code to the *LOGGER*. Outputs data to moves.clasp file relative to the main experiment file."
  (:enable-function (setf tls-server/async/tls::*logger*
                          (lambda (&rest pars) (apply 'clip::collect 'data-moves pars)))
   :disable-function (setf tls-server/async/tls::*logger* (constantly nil))
   :components (reltime from to requested moved)
   :output-file "moves"
   :class clip::composite-time-series-instrumentation)
  (values (tls-server/async/tls::timestamp client)
          from to requested moved))

;;;; Curl experiment

(defvar *status* nil)

(defclip curl-res-status ()
  (:enable-function (setf *status* nil)
   :disable-function (setf *status* nil)
   :components (http-output curl-status))
  (values (prin1-to-string (car *status*)) (third *status*)))

(define-simulator curl
  :description "Start TLS server on random port (0) and call a curl request to it."
  :start-system
  (handler-case
      (create-server 0 tls model
                     :announce-url-callback (callback-on-server
                                             (lambda (url)
                                               (setf *status*
                                                     (multiple-value-call 'list (uiop:run-program
                                                                                 `("curl" ,(puri:render-uri url nil) "-k" "--http2-prior-knowledge")
                                                                                 :ignore-error-status t
                                                                                 :output :string
                                                                                 :error-output :string))))))
    (tls-server::unsupported-server-setup ()
      (restart-case
          (sleep 1)
        (kill-server (res) res))
      (setf *status* (list "N/A" "Unsupported" -1)))))

(define-experiment curl ()
  "Run server, run curl request to it."
  :simulator curl
  :ivs ((model '(:none :thread :async :async-custom))
        (tls '(:tls nil)))
  :timestamp-clip-name relative-times
  :instrumentation (relative-times curl-res-status)
  :after-trial (write-current-experiment-data))

(defvar *ssl-out* 0)

(defclip ssl-out-write (client vector size res)
  (
   :trigger-event (tls-server/async/tls::ssl-write :after)
   :components (size res)
   :output-file "ssl-out-w")
  (incf *ssl-out* res)
  (values size res))

(defclip bio-out-read (wbio vector size res)
  (
   :trigger-event (tls-server/async/tls::bio-read% :after)
   :components (in size2 res2)
   :output-file "ssl-out-b")
  (let ((out *ssl-out*))
    (setf *ssl-out* 0)
    (values out size res)))


(defclip peer-data (client vector size res)
  (
   :trigger-event (tls-server/async/tls::read-from-peer :after)
   :components (data-size)
   :output-file "ssl-out-read")
  (values res))

(defclip sent-data (client vector from to res)
  (
   :trigger-event (tls-server/async/tls::send-to-peer :after)
   :components (from to written)
   :output-file "ssl-out-write")
  (values from to res))

(defclip to-decrypt-data (client vector from to res)
  (
   :trigger-event (tls-server/async/tls::write-octets-to-decrypt :after)
   :components (from to written)
   :output-file "write-octets-to-decrypt")
  (values from to res))

(defclip actions (client res)
  (
   :trigger-event (tls-server/async/tls::select-next-action :after)
   :components (action state)
   :output-file "actions")
  (values res (set-difference (tls-server/async/tls::client-state client) 'tls-server/async/tls::(CAN-WRITE CAN-WRITE-SSL BIO-NEEDS-READ SSL-INIT-NEEDED))))

(define-experiment curl-flow ()
  :simulator curl
  :timestamp-clip-name relative-real-time
  :ivs ((nagle '(nil t)))
  :locals ((model :async-custom) (tls :tls))
  :instrumentation (actions)
  :before-trial (setf tls-server/async/tls::*nagle* nagle)
  :after-trial (write-current-experiment-data))

#+run
(handler-bind
    ((error (lambda (e) (invoke-restart 'clip::shutdown-trial))))
  (run-experiment 'curl :output-file "/tmp/foo.clasp" ))


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
             :output :string :ignore-error-status t))))

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
      (cl-ppcre:scan-to-strings "finished in ([0-9.]+)s, ([0-9.]+) req/s, (.*)" *res*)
    (if match
        (apply 'values (map 'list 'identity val))
        (values 0 0 0))))

(defclip h2load-parsed-reqtime () (:components (tmin tmax mean sd pm))
  (multiple-value-bind (match val)
      (cl-ppcre:scan-to-strings "time for request: +([^ ]+) +([^ ]+) +([^ ]+) +([^ ]+) +([^ ]+)%" *res*)
    (if match
        (apply 'values (map 'list 'identity val))
        (values 0 0 0 0 0))))

(defclip h2load-full () ()
  (prin1-to-string *res*))


;;;; Clips for perf
(defclip perf-report ()
  "Performance report as a string."
  (:enable-function (sb-sprof:start-profiling :mode :time :threads (list (bt:current-thread)))
   :disable-function (sb-sprof:stop-profiling)
   :reset-function (sb-sprof:reset)
   :components (pr))
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
     (handler-case (tls-server:create-server port tls implementation)
       (error (e) (print e))))
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
                                           encrypt-buffer-size-all perf-report)
  :after-trial (write-current-experiment-data)
  :after-experiment (stop-thread thread))
