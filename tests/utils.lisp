(in-package tls-server/tests)

(defun callback-on-server (fn)
  "Return a function that takes one parameter, socket, and call FN on socket port
number and additional ARGS as parameters in a separate thread. Then it kills the
server.

This is to be used as callback fn on an open server for testing it."
  (lambda (url)
    (let ((parent (bt:current-thread)))
      (bt:make-thread (lambda ()
                        (unwind-protect
                             (let ((res
                                     (handler-case
                                         (funcall fn url))))
                               (bt:interrupt-thread parent
                                                    'invoke-restart 'mini-http2:kill-server res))))
                      :name "Test client for a server"))))
