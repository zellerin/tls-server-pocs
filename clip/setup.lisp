(mgl-pax:define-package #:tls-server/measure
  (:use #:tls-server #:clip #:cl #:mgl-pax)
  (:documentation "Standard clips for TLS servers.")
  (:import-from #:clip/doc #:clip))

(in-package #:tls-server/measure)

;;;; Use profiling features of sbcl
#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'sb-sprof))

(defsection tls-server::clips
    (:title "Clips")
  (@time-clips section)
  (@perf-clips section))

(defsection @time-clips
    (:title "Time measuring clips")
  #+nil  (actions clip/doc::clip)
  "Time in experiment measure clips.

Sample usage:

```
(define-simulator dummy  :start-system (sleep 1))

(define-experiment dummy-exp ()
  :simulator dummy
  :instrumentation (relative-real-time relative-run-time)
  :after-trial (write-current-experiment-data))
```

or

```
(defclip random-collectable ()
  (:class clip::composite-time-series-instrumentation
   :output-file \"ticks\"
   :components (rnd))
  (values (random 10)))

(define-experiment dummy-exp-2 ()
  :simulator dummy
  :timestamp-clip-name relative-real-time
  :start-system (dotimes (i 10) (sleep 0.1) (clip::collect 'random-collectable))
  :instrumentation (random-collectable)
  :after-trial (write-current-experiment-data))
```

Note that in second example there is a bug that prevents having
relative-real-time as an explicit instrumentation (the
clip::lookup-number-of-samples gives funny results)."
  (relative-real-time clip)
  (relative-run-time clip))

(defvar *last-real-time*)
(defclip relative-times ()
  "Both CPU and real time since enabling."
  ())

(defclip relative-real-time ()
  "Real time in ms since enabling or reset of the clip"
  (:enable-function (setf *last-real-time* (get-internal-real-time))
   :reset-function  (setf *last-real-time* (get-internal-real-time))
   :disable-function t)
  (round (- (get-internal-real-time) *last-real-time*)
         (/ internal-time-units-per-second 1000)))

(defvar *last-run-time*)
(defclip relative-run-time ()
  "Real time since enabling or reset of the clip"
  (:enable-function (setf *last-run-time* (get-internal-run-time))
   :reset-function  (setf *last-run-time* (get-internal-run-time))
   :disable-function t)
  (/ (- (get-internal-run-time) *last-run-time*)
         (/ internal-time-units-per-second 1000.0)))

(defsection @perf-clips
    (:title "Performance measure clips")
  "For SBCL only, there are three sb-sprof based clips. They produce huge amount of
text to the output and are mutually exclusive."
  (sb-sprof-alloc clip)
  (sb-sprof-time clip)
  (sb-sprof-cpu clip))

(defclip sb-sprof-alloc ()
  "Performance report as a string."
  (:enable-function (sb-sprof:start-profiling :mode :alloc
                                              :threads (list (bt:current-thread)))
   :disable-function (sb-sprof:stop-profiling)
   :reset-function (sb-sprof:reset))
  (prin1-to-string (with-output-to-string (out)
                     (sb-sprof:report :stream out ))))

(defclip sb-sprof-time ()
  "Performance report as a string."
  (:enable-function (sb-sprof:start-profiling :mode :time
                                              :threads (list (bt:current-thread)))
   :disable-function (sb-sprof:stop-profiling)
   :reset-function (sb-sprof:reset))
  (prin1-to-string (with-output-to-string (out)
                     (sb-sprof:report :stream out ))))

(defclip sb-sprof-cpu ()
  "Performance report as a string."
  (:enable-function (sb-sprof:start-profiling :mode :cpu
                                              :threads (list (bt:current-thread)))
   :disable-function (sb-sprof:stop-profiling)
   :reset-function (sb-sprof:reset))
  (prin1-to-string (with-output-to-string (out)
                     (sb-sprof:report :stream out ))))


;;;; External process results
(defvar *external-process-result* nil)

(defun run-process-in-trial (program)
  (setf *external-process-result*
        (multiple-value-call 'list (uiop:run-program
                                    program
                                    :ignore-error-status t
                                    :output :string
                                    :error-output :string))))

(defclip process-result ()
  "Result and the return value of a process that was run during the trial using
RUN-PROCESS-IN-TRIAL."
  (:enable-function (setf *external-process-result* nil)
   :disable-function (setf *external-process-result* nil)
   :components (stdout stderr status))
  (values (prin1-to-string (car *external-process-result*))
          (prin1-to-string (second *external-process-result*))
          (third *external-process-result*)))

(defun server-with-external-program (tls model program)
  "Run HTTP server described by TLS and MODEL. When started, run PROGRAM on it with
last parameter being the URL of the server."
  (handler-case
      (create-server 0 tls model
                     :announce-url-callback (callback-on-server
                                             (lambda (url)
                                               (run-process-in-trial
                                                `(,@program  ,(puri:render-uri url nil))))))
    (tls-server::unsupported-server-setup ()
      (restart-case
          (sleep 1)
        (kill-server (res) res))
      (setf *external-process-result* (list "N/A" "Unsupported" -1)))))

(define-simulator program-to-web-server
  :description "Start TLS server on random port (0) and call a curl request to it."
  :start-system (server-with-external-program tls model program))

(define-experiment curl ()
  "Run server, run curl request to it."
  :simulator program-to-web-server
  :locals ((program '("curl" "-ksS" "--http2-prior-knowledge")))
  :ivs ((model '(:none :thread :async :async-custom))
        (tls '(:tls nil)))
  :timestamp relative-times
  :instrumentation (relative-real-time relative-run-time process-result sb-sprof-alloc)
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
  "Collect client state and action (data movement) selected for execution into a separate clasp file, actions.clasp."
  (
   :trigger-event (tls-server/async/tls::select-next-action :after)
   :components (reltime state action ssl-pending encrypt-buff-size write-buf-len write-buf-chunks)
   :output-file "actions")
  (values (relative-real-time)
          (tls-server/async/tls::states-to-string (tls-server/async/tls::client-state client))
          res
          (tls-server/async/tls::ssl-pending (tls-server/async/tls::client-ssl client))
          (tls-server/async/tls::client-encrypt-buf-size client)
          (reduce #'+ (tls-server/async/tls::client-write-buf client)
                  :key #'length)
          (length  (tls-server/async/tls::client-write-buf client))))

#+nil
(define-experiment curl-flow ()
  :simulator curl
  :timestamp-clip-name get-internal-run-time
  :ivs ((nagle '(nil t)))
  :locals ((model :async-custom) (tls :tls))
  :instrumentation (relative-times actions)
  :before-trial (setf tls-server/async/tls::*nagle* nagle)
  :after-trial (write-current-experiment-data))

#+nil
(define-experiment h2load-flow ()
  :simulator curl
  :start-system (server-with-external-program tls model
                                              '("h2load" "-n2"))
  :timestamp-clip-name get-internal-run-time
  :locals ((nagle t) (model :async-custom) (tls :tls))
  :instrumentation (relative-times actions curl-res-status)
  :before-trial (setf tls-server/async/tls::*nagle* nagle)
  :after-trial (write-current-experiment-data))

#+run
(handler-bind
    ((error (lambda (e) (invoke-restart 'clip::shutdown-trial))))
  (run-experiment 'curl :output-file "/tmp/foo.clasp" ))


;;; Following clips parse specific lines of standard output of h2load
(defclip h2load-parsed-requests ()
  (:components (total started done succeeded failed errored timeout))
  (multiple-value-bind (match val)
      (cl-ppcre:scan-to-strings "requests: ([0-9]+) total, ([0-9]+) started, ([0-9]+) done, ([0-9]+) succeeded, ([0-9]+) failed, ([0-9]+) errored, ([0-9]+) timeout" *external-process-result*)
    (if match
        (apply 'values (map 'list 'identity val))
        (values 0 0 0 0 0 0 0))))

(defclip h2load-parsed-fin () (:components (finished req/s MB/s))
  (multiple-value-bind (match val)
      (cl-ppcre:scan-to-strings "finished in ([0-9.]+)s, ([0-9.]+) req/s, (.*)" *external-process-result*)
    (if match
        (apply 'values (map 'list 'identity val))
        (values 0 0 0))))

(defclip h2load-parsed-reqtime () (:components (tmin tmax mean sd pm))
  (multiple-value-bind (match val)
      (cl-ppcre:scan-to-strings "time for request: +([^ ]+) +([^ ]+) +([^ ]+) +([^ ]+) +([^ ]+)%" *external-process-result*)
    (if match
        (apply 'values (map 'list 'identity val))
        (values 0 0 0 0 0))))


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

(defun stop-thread (thread)
  (ignore-errors
   ;; thread may already be finished - TODO: handle better
   (bt:interrupt-thread thread #'kill-server))
  (bt:join-thread thread))

(defun stop-threads (threads)
  (mapc #'stop-thread threads))

(define-experiment run-profiled (duration tls model program clients multi-threads)
  :system-version (format nil "~a ~a running for ~a secs" tls model duration)
  :simulator program-to-web-server
  :timestamp-clip-name relative-run-time
  :deactivate-scheduled-function (lambda (thread) (bt:destroy-thread thread))
  :instrumentation (process-result sb-sprof-cpu)
  :after-trial (write-current-experiment-data))
