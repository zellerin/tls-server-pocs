
(ql:quickload 'adw-charting-vecto)
(ql:quickload 'lisp-stat)


(defun load-clasp-frame (name stream)
  (let* ((symbol (intern (string-upcase name)))
         (package (or (find-package (string-upcase name))
                      (make-package (string-upcase name))))
         (note (the string (read stream)))
         (columns (loop while (eql (peek-char t stream) #\")
                        collect (string-upcase (read stream))))
         (data (loop while (eql (peek-char t stream nil) #\()
                     collect (read stream)))
         (frame (data-frame:alist-df
                 (mapcar (lambda (column-key data-column)
                           (set (intern column-key package) data-column)
                           (cons (intern column-key package) data-column))
                         columns
                         (apply 'mapcar 'vector data)))))
    (set symbol frame)
    (setf (lisp-stat:name frame) symbol)
    (df:heuristicate-types frame)
    (values frame note)))

(defun load-measurements (file)
  (with-open-file (in file)
    (load-clasp-frame "one" in)
    (load-clasp-frame "five" in)))

(load-measurements (merge-pathnames  "data.clasp" *load-pathname*))

(defun filter (value filter-col data1 data2)
  (loop for f across filter-col
        for d1 across data1
        for d2 across data2
        when (eql value f)
          collect (list d1 d2)))

(with-line-chart (800 600 :background '(.7 .5 .7))
  (dolist (name '(sync async-custom none async))
    (add-series (princ-to-string name)
                (filter name oneclient::implementation oneclient::multi-threads oneclient::total)))
  (set-axis :y "Requests in 1 sec" :label-formatter "~,2F" :draw-zero-p t )
  (set-axis :x "Pipeline size" :draw-zero-p t
            :draw-gridlines-p nil
            :label-formatter #'(lambda (v)
                                 ;;could do something more interesting here
                                 (format nil "~a" (floor v))))
  (save-file "/tmp/customized-line-chart.png"))

(with-line-chart (800 600 :background '(.7 .5 .7))
  (dolist (name '(sync async-custom none async))
    (add-series (princ-to-string name)
                (filter name five::implementation five::multi-threads five::total)))
  (set-axis :y "Requests in 1 sec" :label-formatter "~,2F" )
  (set-axis :x "Pipeline size" :draw-zero-p t
            :draw-gridlines-p nil
            :label-formatter #'(lambda (v)
                                 ;;could do something more interesting here
                                 (format nil "~a" (floor v))))
  (save-file "/tmp/customized-line-chart2.png"))
