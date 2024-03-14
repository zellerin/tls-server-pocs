(in-package clip-user)

(require 'sb-sprof)
(defvar *res*)
(defvar *timed*)

(define-simulator load-system
  :system-name "Load test of TLS server"
  :start-system
  (unless (< iterations multi-threads)
    (let ((thread (bt:make-thread #'tls-test/async/tls::serve-tls :name "Async TLS server")))
      (with-output-to-string (*standard-output*)
        (when perf
          (sb-sprof:start-profiling :mode perf :threads (list thread)))
        (unwind-protect
             (setf *res* (uiop:run-program
                          `("h2load" "https://localhost:8443"
                                     "-n" ,(princ-to-string iterations)
                                     "-m" ,(princ-to-string multi-threads))
                          :output :string))
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

(defclip time-full () (:output-file full.txt)
  *timed*)

(define-experiment load-simple ()
  :simulator load-system
  :instrumentation (h2load-req/s h2load-ttr h2load-suceeded)
  :variables ((iterations '(10  100 1000 10000))
              (multi-threads '(1 2 10 20 50))
              (perf '(nil))
              (sender-fn '(tls-test/async/tls::send-unencrypted-bytes-v1
                           tls-test/async/tls::send-unencrypted-bytes-v2)))
  :after-trial (write-current-experiment-data :format :ascii)
  :before-trial (setf tls-test/async/tls::*unencrypted-sender* sender-fn)
  :before-experiment (delete-file "/tmp/res.txt")
  :after-experiment (setf tls-test/async/tls::*unencrypted-sender*
                          'tls-test/async/tls::send-unencrypted-bytes-v1))

(defclip perf-report-full () ()
  (with-output-to-string (*standard-output*)
    (sb-sprof:report)))

(define-experiment perf-test-cpu ()
  :simulator load-system
  :instrumentation (perf-report-full h2load-req/s h2load-suceeded h2load-finished)
  :variables ((iterations '(3000))
              (multi-threads '(1 10))
              (perf '(:cpu))
              (sender-fn '(tls-test/async/tls::send-unencrypted-bytes-v1
                           tls-test/async/tls::send-unencrypted-bytes-v2)))
  :before-trial (setf tls-test/async/tls::*unencrypted-sender* sender-fn)

  :after-trial
  (write-current-experiment-data :format :ascii)
  :before-experiment (delete-file "/tmp/res-perf.txt"))
