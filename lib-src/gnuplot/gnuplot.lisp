(in-package :matlisp)
(defvar *current-gnuplot-stream* nil)
(defvar *gnuplot-binary* "/usr/bin/gnuplot")

(defun open-gnuplot-stream ()
  (#+:sbcl
   sb-ext:run-program
   *gnuplot-binary* nil :input :stream :wait nil :output t))

(defun plot2d (data &key (color (list "#FF0000")) (stream (#+:sbcl
						    sb-ext:process-input
						    *current-gnuplot-stream*)))
  (with-open-file (s "/tmp/matlisp-gnuplot.out" :direction :output :if-exists :supersede :if-does-not-exist :create)
    (loop :for i :from 0 :below (loop :for x :in data :minimizing (number-of-elements x))
       :do (loop :for x :in data :do (format s "~a " (tensor-ref x i)) :finally (format s "~%"))))
  (format stream "plot '/tmp/matlisp-gnuplot.out' with lines linecolor rgb ~s~%" color)
  (finish-output stream))

(defun gnuplot-send (str &key (stream (#+:sbcl
				       sb-ext:process-input
				       *current-gnuplot-stream*)))
  (format stream "~a~%" str)
  (finish-output stream))


;; (defclass gnuplot-plot-info ()
;;   ((title
;;     :initform "GNU PLOT"
;;     :accessor gnuplot-title)
;;    (x-label
;;     :initform "X"
;;     :accessor gnuplot-x-label)
;;    (y-label
;;     :initform "Y"
;;     :accessor gnuplot-y-label)
;;    (x-data
;;     :accessor gnuplot-x-data)
;;    (y-data
;;     :accessor gnuplot-y-data)
;;    (z-data
;;     :accessor gnuplot-z-data)))


;; (defun gnuplot-plot (info  &key (stream (#+:sbcl
;; 							       sb-ext:process-input
;; 							       *current-gnuplot-stream*)))
;;   (with-accessors ((title gnuplot-title)
;; 		   (x-label gnuplot-x-label)
;; 		   (y-label gnuplot-y-label)
;; 		   (x-data gnuplot-x-data)
;; 		   (y-data gnuplot-y-data)
;; 		   (z-data gnuplot-z-data))
;;       info
;;     (format stream "~&set title '~S'~%" title)
;;     (format stream "~&set xlabel '~S'~%" x-label)
;;     (format stream "~&set ylabel '~S'~%" y-label)
;;     (finish-output stream)
;;     (map nil #'(lambda (x y z)
;; 		 (with-open-file (s "/tmp/gnuplot.out" :direction :output
;; 				    :if-exists :overwrite)
;; 		   (map nil #'(lambda (xs ys zs)
;; 				(if zs
;; 				    (format s "~A ~A ~A~%" xs ys zs)
;; 				    (format s "~A ~A~%" xs ys)))
;; 			x y z)
;; 		   (format stream "~A '/tmp/gnuplot.out'~%"
;; 			   (if z "splot" "plot"))
;; 		   (finish-output stream)
;; 		   (sleep 5)))
;; 	 x-data y-data z-data)
;;     ))
