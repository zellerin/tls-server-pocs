(in-package tls-server/utils)

(mgl-pax:defsection @octets ()
    "Simplify work with octet vectors"
  (octet-vector type)
  (octetize function)
  (fully-read-array function))

(deftype octet-vector ()
  "Simple (i.e., not adjustable and w/o fill pointer) one-dimensional array of
octets"
  '(simple-array (unsigned-byte 8) (*)))

(defun fully-read-array (stream vector to-read)
  "Read TO-READ octets to the octet VECTOR.

Lisp standard says that read-sequence returns less than required bytes only on
EOF, and we rely on that; if this happens, invoke GO-AWAY restart."
  (declare ((integer 0 #.array-dimension-limit) to-read)
           (octet-vector vector))
  (let ((read (read-sequence vector stream :start 0
                                           :end to-read)))
    (unless (= read to-read)
      (invoke-restart 'go-away))
    vector))

(defun octetize (array)
  "Make a simple one-dimensional array of octets with same content as ARRAY."
  (make-array (length array) :element-type '(unsigned-byte 8)
                             :initial-contents array))
