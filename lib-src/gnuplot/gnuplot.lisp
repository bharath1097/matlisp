(in-package :matlisp)
(defvar *current-gnuplot-process* nil)

(defun open-gnuplot (&key (gnuplot-binary (pathname
					   #+darwin "/opt/local/bin/gnuplot"
					   #+linux "/usr/bin/gnuplot"))
		       (terminal "wxt"))
  (or *current-gnuplot-process*
      (progn
	(setf *current-gnuplot-process* (#+:sbcl sb-ext:run-program
					 #+:ccl ccl:run-program
					 gnuplot-binary nil :input :stream :wait nil :output t))
	(gnuplot-send "~%set datafile fortran~%set term ~a~%" terminal)
	*current-gnuplot-process*)))

(defun close-gnuplot ()
  (when *current-gnuplot-process*
    (gnuplot-send "quit~%")
    (setf *current-gnuplot-process* nil)))

(defmacro with-gnuplot-stream ((stream) &rest body)
  `(let ((,stream (#+:sbcl sb-ext:process-input
		   #+:ccl ccl:external-process-input-stream
		   (open-gnuplot))))
     (unwind-protect (progn ,@body) (finish-output ,stream))))

(defun gnuplot-send (str &rest args)
  (with-gnuplot-stream (s)
    (apply #'format s str args)))
;;
(defun split-color (num)
  (multiple-value-bind (a b0) (floor num 256)
    (multiple-value-bind (b2 b1) (floor a 256)
      (list b2 b1 b0))))

(defmacro with-gnuplot-term ((stream num &key multiplot (terminal "wxt") output) &rest body)
  (using-gensyms (decl (num output terminal multiplot))
    `(let (,@decl)
       (with-gnuplot-stream (,stream)
	 (format ,stream "set term push~%set term ~a ~a~%" ,terminal ,num)
	 (when ,output (format ,stream "set output '~a'~%" (etypecase ,output (pathname (pathname-name ,output)) (string ,output))))
	 (when ,multiplot (format ,stream "set multiplot~%"))
	 (unwind-protect (progn ,@body)
	   (when ,multiplot (format ,stream "unset multiplot~%"))
	   (when ,output (format ,stream "set output~%"))
	   (format ,stream "set term pop~%"))))))

(defun plot (data &key (lines t) color)
  (declare (type (and real-numeric-tensor (or tensor-matrixp tensor-vectorp)) data))
  (let* ((color (if (listp color) color (let ((lst (list color))) (setf (cdr lst) lst))))
	 (data (suptensor~ data 2))
	 (n (dimensions data 1)))
    (labels ((pcom (x y s)
	       (let ((out (zeros (list (dimensions data 0) 2))))
		 #i(out[:, 0][] = x, out[:, 1][] = y)
		 (format s "")
		 ))))
    (with-gnuplot-stream (s)
      (if (= n 1)
	  (format s "plot ")
	  ))
    (with-gnuplot-stream (s)
      
      )
    (let ((cmd (apply #'string+ (cons "plot " (loop :for x :in (cdr data)
						 :for i := 2 :then (1+ i)
						 :for clist := col :then (cdr clist)
						 :collect (string+ "'" fname "' using 1:" (format nil "~a " i)
								   "with " (if lines "lines" "points") " "
								   (if (car clist)
								       (apply #'(lambda (r g b) (format nil "linecolor rgb(~a, ~a, ~a)" r g b))
									      (split-color (car clist)))
								       "")
								   (format nil "title \"~a\"" (1- i))
								   ", "))))))
      (setf (aref cmd (- (length cmd) 2)) #\;
	    (aref cmd (- (length cmd) 1)) #\Newline)
      (gnuplot-send cmd))))

(defun splot (data)
  (let ((fname "/tmp/matlisp-gnuplot.out"))
    (with-open-file (s fname :direction :output :if-exists :supersede :if-does-not-exist :create)
      (loop :for i :from 0 :below (loop :for x :in data :minimizing (size x))
	 :do (loop :for x :in data :do (format s "~a " (coerce (ref x i) 'single-float)) :finally (format s "~%"))))
    (gnuplot-send (string+ "splot \'" fname "\'
"))))

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
