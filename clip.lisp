(in-package clip-user)

(require 'sb-sprof)
(defvar *res*)

(define-simulator load-system
  :system-name "Load test of TLS server"
  :start-system
  (unless (< iterations multi-threads)
    (let ((thread (bt:make-thread (lambda () (mini-http2:create-server 8443 tls dispatch-method))
                                  :name "Async TLS server")))
      (with-output-to-string (*standard-output*)
        (when perf
          (sb-sprof:start-profiling :mode perf :threads (list thread)))
        (unwind-protect
             (setf *res* (uiop:run-program
                          `("h2load" "https://localhost:8443"
                                     "-n" ,(princ-to-string iterations)
                                     "-m" ,(princ-to-string multi-threads)
                                     "-c" ,(princ-to-string clients))
                          :output :string))
          (bt:interrupt-thread thread #'tls-server:kill-server)
          (bt:join-thread thread)
          (sb-sprof:stop-profiling))))))

(defclip h2load-req/s () ()
  (multiple-value-bind (match val)
      (cl-ppcre:scan-to-strings "req/s[ :]*([0-9\\.]+)[ ]*([0-9\\.]+)[ ]*([0-9\\.]+)" *res*)
    (declare (ignore match))
    (aref val 2)))

(defclip h2load-ttr () ()
  (multiple-value-bind (match val)
      (cl-ppcre:scan-to-strings "time for request: *([0-9\\.]+)[um]s[ ]*([0-9\\.]+)[um]s[ ]*([0-9\\.]+)[um]s" *res*)
    (declare (ignore match))
    (aref val 2)))

(defclip h2load-finished () ()
  (multiple-value-bind (match val)
      (cl-ppcre:scan-to-strings "finished in ([0-9.]+[um]?s), ([0-9\\.]+) req/s" *res*)
    (declare (ignore match))
    val))

(defclip h2load-suceeded () ()
  (multiple-value-bind (match val)
      (cl-ppcre:scan-to-strings "requests: .* ([0-9]+) succeeded," *res*)
    (declare (ignore match))
    (aref val 0)))

(defclip h2load-full () (:output-file full.txt)
  *res*)

(define-experiment load-simple (tls dispatch-method)
  :simulator load-system
  :system-version (format nil "Dispatch: ~a, TLS: ~s" dispatch-method tls)
  :instrumentation (h2load-finished h2load-req/s h2load-ttr h2load-suceeded)
  :variables ((iterations '(10000))
              (multi-threads '(1 2 10 20 50))
              (perf '(nil))
              (clients '(1 2 5 9)))
  :after-trial (write-current-experiment-data)
  :after-experiment (setf tls-server/async/tls::*unencrypted-sender*
                          'tls-server/async/tls::send-unencrypted-bytes-v1))

(defclip perf-report-full () ()
  (with-output-to-string (*standard-output*)
    (sb-sprof:report)))

(define-experiment perf-test ()
  :simulator load-system
  :instrumentation (perf-report-full h2load-req/s h2load-suceeded h2load-finished)
  :variables ((iterations '(300000))
              (multi-threads '(1 10))
              (perf '(:cpu))
              (clients '(10))
              (dispatch-method '(:async-custom))
              (tls '(:tls)))

  :after-trial
  (write-current-experiment-data :format :ascii))

(define-experiment raising-requests ()
  :simulator load-system
  :instrumentation (h2load-req/s h2load-suceeded h2load-finished)
  :variables ((iterations '(10 30 100 300 1000 3000 10000 30000 100000 300000))
              (multi-threads '(10))
              (perf '(nil))
              (clients '(1))
              (dispatch-method '(:async-custom))
              (tls '(:tls)))

  :after-trial
  (write-current-experiment-data))

(defclip number-of-cli (len) ()
  (length tls-server/async/tls::*clients*))

(defclip nr-of-clients () (:components (number-of-cli)
                           :trigger-event (tls-server/async/tls::process-client-sockets :BEFORE)
                           :map-function (list (length tls-server/async/tls::*clients*))))

(define-experiment clients-test ()
  :simulator load-system
  :instrumentation (nr-of-clients)
  :schedule-function
  (lambda (function time period name &rest options)
    (declare (ignore name options function time period)))
  :timestamp get-internal-run-time
  :seconds-per-time-unit (/ 1.0 internal-time-units-per-second)
  :variables ((iterations '(30000))
              (multi-threads '(9))
              (perf '(nil))
              (clients '(9))
              (dispatch-method '(:async-custom))
              (tls '(:tls)))

  :after-trial
  (write-current-experiment-data :format :ascii))

(define-experiment test-different-implementations ()
  :simulator load-system
  :instrumentation (h2load-req/s h2load-suceeded h2load-finished)
  :variables ((iterations '(3000 10000))
              (multi-threads '(1 2 5 10 20))
              (clients '(1 2 5 10))
              (perf '(:cpu))
              (tls '(:tls))
              (dispatch-method '(:none :thread :async-custom)))

  :after-trial (write-current-experiment-data))
