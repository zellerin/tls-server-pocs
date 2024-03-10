(in-package tls-server/tests)

(defun query-port-using-http2 (url)
  "Query a port using a client build upon http2 package."
  (unless (eql (puri:uri-scheme url) :http)
    (error "Only HTTP supported."))
  (let (res)
    (with-simple-restart (kill-client "Kill client")
      (with-client-socket
          (socket stream "localhost"
                  (puri:uri-port url)
                  :element-type '(unsigned-byte 8))
        (http2/client::retrieve-url-using-network-stream
         stream url
         :end-headers-fn
         (lambda (raw-stream)
           (unwind-protect
                (with-open-stream (response-stream
                                   (http2:make-transport-stream raw-stream nil nil))
                  (setq res
                        (equalp (map 'vector 'char-code mini-http2::*result-text*)
                                (http2::read-stream-content-into-byte-vector response-stream))))
             (invoke-restart 'kill-client))))))
    res))

(defun query-port-using-curl (url)
  "Query a port using a client build upon http2 package."
  (is (null
       (mismatch mini-http2::*result-text*
                 (uiop:run-program
                  `("curl" ,(puri:render-uri url nil) "-k" "--http2-prior-knowledge")
                  :output :string)))))

(deftest http2-client-curl ()
  "Test the server using curl."
  (dolist (tls '(nil :tls))
    (dolist (model '(:none :thread #+nil :poll))
      (is (mini-http2:create-server 0 nil model
                                    :announce-url-callback (callback-on-server
                                                            #'query-port-using-curl))))))

(defun http2-client-curl-test (tls model)
  "Test the server using curl."
  (is (mini-http2:create-server 0 tls model
                                :announce-url-callback (callback-on-server
                                                        #'query-port-using-curl))))

(defun http2-client-native-test (tls model)
  "Test the server from http2-based client."
  (is (mini-http2:create-server 0 tls model
                                :announce-url-callback (callback-on-server
                                                        #'query-port-using-http2))))

(deftest none/native ()
  "Run tests on single-client server."
  (http2-client-native-test nil :none)
  (http2-client-curl-test nil :none)
  (http2-client-curl-test :tls :none))

(deftest thread/native ()
  "Run tests on a threading server."
  (http2-client-native-test nil :thread)
  (http2-client-curl-test :tls :none))
