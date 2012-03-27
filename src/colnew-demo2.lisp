;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: :matlisp; Base: 10 -*-

(in-package #:matlisp)

(defvar *gamma* 1.1d0)
(defvar *eps* .01d0)
(defvar *dmu* *eps*)
(defvar *eps4mu* (/ (expt *eps* 4) *dmu*))
(defvar *xt* (sqrt (/ (* 2 (- *gamma* 1)) *gamma*)))


(defun fsub (x z f)
  (setf (fv-ref f 0)
	(+ (/ (fv-ref z 0) x x)
	   (- (/ (fv-ref z 1) x))
	   (/ (- (fv-ref z 0)
		 (* (fv-ref z 2)
		    (- 1 (/ (fv-ref z 0)
			    x)))
		 (* *gamma* x (- 1
				 (* x x 0.5d0))))
	      *eps4mu*)))
  (setf (fv-ref f 1)
	(+ (/ (fv-ref z 2) x x)
	   (/ (- (fv-ref z 3)) x)
	   (* (fv-ref z 0)
	      (/ (- 1 (/ (fv-ref z 0) 2 x))
		 *dmu*)))))

(defun dfsub (x z df)
  (let ((nrows 2))
    (flet ((column-major-index (r c)
	     (+ (- r 1)
		(* (- c 1) nrows))))
      (setf (fv-ref df (column-major-index 1 1))
	    (+ (/ 1 x x)
	       (/ (+ 1
		     (/ (fv-ref z 2)
			x))
		  *eps4mu*)))
      (setf (fv-ref df (column-major-index 1 2))
	    (/ -1 x))
      (setf (fv-ref df (column-major-index 1 3))
	    (- (/ (- 1 (/ (fv-ref z 0)
			  x))
		  *eps4mu*)))
      (setf (fv-ref df (column-major-index 1 4))
	    0d0)
      (setf (fv-ref df (column-major-index 2 1))
	    (/ (- 1
		  (/ (fv-ref z 0)
		     x))
	       *dmu*))
      (setf (fv-ref df (column-major-index 2 2))
	    0d0)
      (setf (fv-ref df (column-major-index 2 3))
	    (/ 1 x x))
      (setf (fv-ref df (column-major-index 2 4))
	    (/ -1 x)))))

(defun gsub (i z g)
  (case i
    ((or 1 3)
     (setf (fv-ref g 0) (fv-ref z 0)))
    (2
     (setf (fv-ref g 0) (fv-ref z 2)))
    (4
     (setf (fv-ref g 0) (+ (fv-ref z 3)
			   (* -0.3d0 (fv-ref z 2))
			   0.7d0)))))

(defun dgsub (i z dg)
  (dotimes (k 4)
    (setf (fv-ref dg k) 0d0))
  (case i
    ((or 1 3)
     (setf (fv-ref dg 0) 1d0))
    (2
     (setf (fv-ref dg 2) 1d0))
    (4
     (setf (fv-ref dg 3) 1d0)
     (setf (fv-ref dg 2) -0.3d0))))

(defun guess (x z dmval)
  (let ((con (* *gamma* x (- 1 (* 0.5d0 x x))))
	(dcon (* *gamma* (- 1 (* 1.5d0 x x))))
	(d2con (* -3 *gamma* x)))
    (cond ((<= x *xt*)
	   (setf (fv-ref z 0) (* 2 x))
	   (setf (fv-ref z 1) 2d0)
	   (setf (fv-ref z 2) (+ (* -2 x) con))
	   (setf (fv-ref z 3) (+ dcon -2d0))
	   (setf (fv-ref dmval 1) d2con))
	  (t
	   (setf (fv-ref z 0) 0d0)
	   (setf (fv-ref z 1) 0d0)
	   (setf (fv-ref z 2) (- con))
	   (setf (fv-ref z 3) (- dcon))
	   (setf (fv-ref dmval 1) (- d2con))))
    (setf (fv-ref dmval 0) 0d0)))


(defun colnew-prob2 ()
  (let* (
	 (aleft 0d0)
	 (aright 1d0)
	 ;; Two differential equations
	 (ncomp 2)
	 ;; Orders of each equation
	 (m (make-array 2 :element-type '(signed-byte 32)
			  :initial-contents '(2 2)))
	 ;; Locations of side conditions
	 (zeta (make-array 4 :element-type 'double-float
			     :initial-contents '(0d0 0d0 1d0 1d0)))
	 (ipar (make-array 11 :element-type '(signed-byte 32)
			      :initial-element 0))
	 ;; Error tolerances on u and its second derivative
	 (ltol (make-array 4 :element-type '(signed-byte 32)
			     :initial-contents '(1 2 3 4)))
	 (tol (make-array 4 :element-type 'double-float
			    :initial-contents '(1d-5 1d-5 1d-5 1d-5)))
	 (fspace (make-array 40000 :element-type 'double-float))
	 (ispace (make-array 2500 :element-type '(signed-byte 32)))
	 (fixpnt (make-array 1 :element-type 'double-float)))
    ;; Set up parameters of the problem.
    (setf (aref ipar 0) 1)		; nonlinear problem
    (setf (aref ipar 1) 4)		; 4 collocation points per subinterval
    (setf (aref ipar 2) 10)		; Initial uniform mesh of 10 subintervals
    (setf (aref ipar 7) 0)
    (setf (aref ipar 4) 40000)		; Size of fspace
    (setf (aref ipar 5) 2500)		; Size of ispace
    (setf (aref ipar 6) -1)		; Full output
    (setf (aref ipar 8) 1)		; Initial approx provided
    (setf (aref ipar 9) 0)		; Regular problem
    (setf (aref ipar 10) 0)		; No fixed points in mesh
    (setf (aref ipar 3) 4)		; Tolerances on all components
    
    (colnew ncomp m aleft aright zeta ipar ltol tol fixpnt ispace fspace 0
	    #'fsub #'dfsub #'gsub #'dgsub #'guess)
    (let ((x 0d0)
	  (z (make-array 4 :element-type 'double-float)))
      (dotimes (j 21)
	(appsln x z fspace ispace)
	(format t "~5,2f ~{~15,5e~}~%" x (coerce z 'list))
	(incf x 0.05d0)))))
