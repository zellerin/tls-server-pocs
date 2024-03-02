(in-package tls-server/tests)

(defvar *test-pars*
  '(("-D" "1")
    ("-D" "1" "-m" "150")
    ("-D" "1" "-m" "150" "--warm-up-time" "50ms"))
  "H2load parameters.")

(defun test-port (url)
  (dolist (args *test-pars*)
    (let ((res (uiop:run-program
                `("h2load" ,(puri:render-uri url nil) ,@args)
                :output :string)))
      (multiple-value-bind (match val)
          (cl-ppcre:scan-to-strings "req/s[ :]*([0-9\\.]+)[ ]*([0-9\\.]+)[ ]*([0-9\\.]+)" res)
        (declare (ignore match))
        (let ((val-as-num (read-from-string (aref val 2))))
          (format t "~&~10@a ~{~a~^ ~}~%" val-as-num args)
          val-as-num)))))

(deftest test-speed-single/core (tls method)
  (is
      (mini-http2:create-server
       0 tls method
       :announce-url-callback (callback-on-server #'test-port
                                                  :thread-name (princ-to-string (list method tls))))))

(deftest test-speed-single ()
  "This should test speed, but FIXME: does not work and is commented out. "
#+nil  (dolist (tls '(nil :tls))
    (dolist (method '(:none :none/http2 :thread :async))
      (format t "~&TLS: ~a, ~a~%" tls method)
      (test-speed-single/core tls method))))
