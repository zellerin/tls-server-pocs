(in-package mini-http2)

(mgl-pax:defsection @octets ()
    "Simplify work with octet vectors"
  (octet-vector type)
  (octetize function)
  (fully-read-array function))

(deftype octet-vector ()
  "Simple (i.e., not adjustable and w/o fill pointer) onedimensional array of
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
  "Make a simple onedimensional array of same content as ARRAY."
  (make-array (length array) :element-type '(unsigned-byte 8)
                             :initial-contents array))

(mgl-pax:defsection @tracing ()
  "SBCL allows trace to call an arbitraty function. We use this to store traces to
an array that can be analysed later."
  (with-tracing macro))

(defvar *log-classes* '(:always))
(defvar *log-events* (make-array 10 :fill-pointer 0 :adjustable t)
  "Array of events")

(defstruct (trace-item (:print-function print-trace-item)) real-time cpu-time fn-name depth keyword values)

(defvar *real-time-0* nil)
(defvar *cpu-time-0* nil)
(defun print-trace-item (item stream level)
  (when (or (null *print-level*)
            (< level *print-level*))
    (format stream "~@[~f~] ~@[~f~] ~a ~a ~a~%"
            (when *real-time-0* (round (- (trace-item-real-time item) *real-time-0*) (/ internal-time-units-per-second 1000)))
            (when *cpu-time-0* (round (- (trace-item-cpu-time item) *cpu-time-0*) (/ internal-time-units-per-second 1000)))
            (trace-item-fn-name item)
            (trace-item-values item)
            (trace-item-keyword item))))

(defun do-log (depth fn-name keyword stack values)
  (declare (ignore stack))
  (vector-push-extend
   (make-trace-item :real-time (get-internal-real-time)
                    :cpu-time (get-internal-run-time)
                    :fn-name fn-name
                    :depth depth
                    :keyword keyword
                    :values values)
   *log-events*))

(defmacro trace-by-log (&rest fns)
  `(trace :report do-log ,@fns))

(defmacro with-tracing (fns &body body)
  `(let ((*log-events* (make-array 10 :fill-pointer 0 :adjustable t)))
    (trace :report do-log ,@fns)
     (unwind-protect
          (restart-case
              (progn ,@body)
            (dump-traces ()
              *log-events*))
       (untrace ,@fns))
     *log-events*))

(defun make-doc ()
  (mgl-pax:update-asdf-system-html-docs mini-http2::@http2-server-pocs "tls-server")
  (mgl-pax:update-asdf-system-readmes mini-http2::@http2-server-pocs "tls-server"))
