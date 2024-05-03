(asdf:load-asd  (merge-pathnames "../tls-server.asd" *load-pathname*))
(ql:quickload 'tls-server/clip)

(in-package #:tls-server/measure)

(define-experiment vary-threads (duration clients tls)
  :simulator load-system
  :system-version (format nil "~d cliens~:p running for ~a secs (tls ~a)" clients duration tls)
  :instrumentation (h2load-parsed-requests h2load-parsed-reqtime h2load-parsed-fin)
  :variables ((multi-threads '(1 2 5 10 20 30 40 50 60 70 80 100 120))
              (implementation '(:async-custom :thread  :none)))
  :after-trial (write-current-experiment-data)
  :locals ((threads (start-threads tls '(:async-custom :thread :async :none))))
  :after-experiment (stop-threads threads))

(let ((clasp-file (merge-pathnames "./data.clasp" (or *load-pathname* "/tmp/"))))
  (when (probe-file clasp-file)
    (delete-file clasp-file))
  (handler-bind ((error (lambda (e) (print e) (clip:shutdown-and-run-next-trial))))
    (clip:run-experiment 'vary-threads :args '(1 1 :tls) :output-file clasp-file)
    (clip:run-experiment 'vary-threads :args '(1 5 :tls) :output-file clasp-file)
    (clip:run-experiment 'vary-threads :args '(1 1 nil) :output-file clasp-file)))
