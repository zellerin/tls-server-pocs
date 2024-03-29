(load "~/quicklisp/setup")
(load "~/.sbclrc")
(asdf:load-asd  (merge-pathnames "../tls-server.asd" *load-pathname*))
(ql:quickload 'clip-1994/loader)
(ql:quickload 'adw-charting-vecto)

(defpackage tls-server/report
  (:use #:cl #:adw-charting #:clip/loader))

(in-package tls-server/report)

(load-measurements (merge-pathnames  "data.clasp" *load-pathname*)
                   "one" "five" "notls")

(defun filter (value filter-col data1 data2)
  (loop for f across filter-col
        for d1 across data1
        for d2 across data2
        when (eql value f)
          collect (list d1 d2)))

(adw-charting:with-line-chart (800 600 :background '(.7 .5 .7))
  (dolist (name '(thread async-custom none async))
    (add-series (princ-to-string name)
                (filter name one::implementation one::multi-threads one::total)))
  (set-axis :y "Requests in 1 sec" :label-formatter "~,2F" :draw-zero-p t )
  (set-axis :x "Pipeline size" :draw-zero-p t
            :draw-gridlines-p nil
            :label-formatter #'(lambda (v)
                                 ;;could do something more interesting here
                                 (format nil "~a" (floor v))))
  (save-file (merge-pathnames "../images/single-client.png" *load-pathname*)))

(with-line-chart (800 600 :background '(.7 .5 .7))
  (dolist (name '(thread async-custom none async))
    (add-series (princ-to-string name)
                (filter name five::implementation five::multi-threads five::total)))
  (set-axis :y "Requests in 1 sec" :label-formatter "~,2F" )
  (set-axis :x "Pipeline size" :draw-zero-p t
            :draw-gridlines-p nil
            :label-formatter #'(lambda (v)
                                 ;;could do something more interesting here
                                 (format nil "~a" (floor v))))
  (save-file (merge-pathnames "../images/five-clients.png" *load-pathname*)))

(with-line-chart (800 600 :background '(.7 .5 .7))
       (dolist (name '(thread async-custom none async))
         (add-series (princ-to-string name)
                     (filter name notls::implementation notls::multi-threads notls::total)))
       (set-axis :y "Requests in 1 sec" :label-formatter "~,2F" )
       (set-axis :x "Pipeline size" :draw-zero-p t
                 :draw-gridlines-p nil
                 :label-formatter #'(lambda (v) (format nil "~a" (floor v))))
       (save-file (merge-pathnames "../images/one-client-no-tls.png" *load-pathname*)))

(defun parse-time-symbol (datum)
  (let* ((s (symbol-name datum))
         (len (length s)))
    (cond ((null (mismatch "US" s :start2 (- len 2)))
           (read-from-string s t nil :end (- len 2)))
          ((null (mismatch "MS" s :start2 (- len 2)))
           (* 1000.0 (read-from-string s t nil :end (- len 2))))
          ((null (mismatch "S" s :start2 (- len 1)))
           (* 1000000.0 (read-from-string s t nil :end (- len 1)))))))

(with-line-chart (800 600 :background '(.7 .5 .7))
  (dolist (name '(thread async-custom none #+nil async))
    (add-series (princ-to-string name)
                (filter name one::implementation one::multi-threads (map 'vector 'parse-time-symbol one::mean))))
  (set-axis :x "Pipeline size (streams)" :label-formatter "~,2F" )
  (set-axis :y "Mean request time (us)" :draw-zero-p t
            :draw-gridlines-p nil
            :scalefn 'log
            :label-formatter #'(lambda (v) (format nil "~a" (floor v))))
  (save-file (merge-pathnames "../images/one-client-ttl.png" *load-pathname*)))
