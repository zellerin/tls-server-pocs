(asdf:load-asd  (merge-pathnames "../tls-server.asd" *load-pathname*))
(ql:quickload 'tls-server/clip)

(in-package #:tls-server/measure)

(when (probe-file "/tmp/perf.clasp") (delete-file "/tmp/perf.clasp"))
(clip:run-experiment 'tls-server/measure::run-profiled
                     :args '(1 :tls :async-custom)
                     :output-file "/tmp/perf.clasp")
