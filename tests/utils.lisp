(in-package tls-server/utils/tests)

(deftest fully-read-array/test ()
  (let ((vector (octetize #(1 2 3 4 5 6 7))))
    (is (typep vector '(simple-array (unsigned-byte 8) (7))))
    (is (aref vector 3) 4)))
