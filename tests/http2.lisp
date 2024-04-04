(in-package tls-server/mini-http2/tests)


(deftest url-from-socket/test ()
  "Test correct format of URL created from tls and plain socket port."
  (is (equal
       (usocket:with-socket-listener (socket "localhost" 31415)
         (princ-to-string (url-from-socket socket "localhost" t)))
       "https://localhost:31415/"))
  (is (equal
       (usocket:with-socket-listener (socket "localhost" 31415)
         (princ-to-string (url-from-socket socket "localhost" nil)))
       "http://localhost:31415/")))

(deftest get-frame-size/test ()
  "Test that frame size is correctly extracted from fixed frames."
  (is (zerop (get-frame-size *settings-frame*)))
  (is (= 11 (get-frame-size *header-frame*))))

(deftest get-frame-type/test ()
  "Test that frame type is correctly extracted from fixed frames."
  (is (= 4 (get-frame-type *settings-frame*)))
  (is (= 1 (get-frame-type *header-frame*)))
  (is (= 0 (get-frame-type *data-frame*))))

(deftest get-frame-flags/test ()
  (is (= 0 (get-frame-flags *settings-frame*)))
  (is (= 4 (get-frame-flags *header-frame*)))
  (is (= 1 (get-frame-flags *data-frame*)))
  (is (= 1 (get-frame-flags *ack-frame*))))

(deftest callback-on-server/test ()
  "Test that CALLBACK-ON-SERVER kills the server in parent thread."
  (is
      (equal
       (restart-case
           (let ()
             (funcall (callback-on-server #'identity) "foo")
             (sleep 10)
             (is nil))
         (kill-server (result) result))
       "foo")))

(deftest callback-on-server/test-no-kill-server ()
  "Test that CALLBACK-ON-SERVER fails nicely when there is no KILL-SERVER restart
   present."
  (is (equal "foo"
       (let ((thread (funcall (callback-on-server #'identity) "foo")))
         (bt:join-thread thread)))))

(deftest callback-on-server/callback-error ()
  "Test that CALLBACK-ON-SERVER behaves on its error."
  (is (null
       (restart-case
           (progn
             (funcall (callback-on-server
                       (lambda (a)
                         (declare (ignore a))
                         (handler-bind
                             ((error (lambda (condition)
                                       (declare (ignore condition))
                                       (invoke-restart 'kill-parent))))
                           (error "An error"))))
                      "http://foo/")
             (sleep 10)
             (is nil))
         (kill-server (result) result)))))
