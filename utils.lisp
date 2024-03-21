(in-package tls-server/utils)

(mgl-pax:defsection @octets (:title "Work with octets")
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

(mgl-pax:defsection @mgl-extensions (:title "New locatives")
    "Define a  locative to document CFFI callbacks."
  (define-documented-callback mgl-pax:macro)
  (callback mgl-pax:locative))


(defclass callback ()
  ((name :accessor get-name :initarg :name)
   (args :accessor get-args :initarg :args)
   (docstring :accessor get-docstring :initarg :docstring))
  (:documentation "Helper class to keep information about CFFI callbacks."))

(defvar *callbacks* (make-hash-table :test 'eq))

(defun dref-to-callback (dref)
  (gethash (dref-ext:dref-name dref) *callbacks*))

(defmacro define-documented-callback (name res-type args docstring &body body)
  "Wrapper on CFFI:DEFCALLBACK that also tracks the args and docstrings."
  (check-type docstring string)
  `(progn
     (setf (gethash ',name *callbacks*)
           (make-instance 'callback :name ',name :args ',args :docstring ,docstring))
     (cffi:defcallback ,name ,res-type ,args ,docstring ,@body)))

(dref-ext:define-locative-type callback ()
  "Refers to a CFFI callback.")

(dref-ext:define-definition-class callback callback-dref)

(defmethod dref-ext:locate* ((object callback))
  (make-instance 'callback-dref :name (get-name object) :locative 'callback))

(defmethod dref-ext:dref* (symbol (locative-type (eql 'callback)) locative-args)
  (unless (and (symbolp symbol)
               (gethash symbol *callbacks*))
    (dref-ext:locate-error "~S does not name a callback (not in table)." symbol))
  (make-instance 'callback-dref :name symbol :locative 'callback))

(defmethod dref-ext:resolve* ((dref callback-dref))
  (dref-to-callback dref))

(defmethod dref-ext:docstring* ((callback callback-dref))
  (get-docstring (dref-to-callback callback)))

(defmethod dref-ext:arglist* ((callback callback-dref))
  (mapcar 'car (get-args  (dref-to-callback callback))))
