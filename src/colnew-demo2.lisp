;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: :matlisp; Base: 10 -*-

(in-package #:matlisp)

(defvar *gamma* 1.1d0)
(defvar *eps* .01d0)
(defvar *dmu* *eps*)
(defvar *eps4mu* (/ (expt *eps* 4) *dmu*))
(defvar *xt* (sqrt (/ (* 2 (- *gamma* 1)) *gamma*)))


(defun fsub (x a-z a-f)
  (utilities::with-fortran-matrices ((z a-z (1 2))
				     (f a-f (1 4)))
    (setf (f 1)
	  (+ (/ (z 1) x x)
	     (- (/ (z 2) x))
	     (/ (- (z 1)
		   (* (z 3)
		      (- 1 (/ (z 1)
			      x)))
		   (* *gamma* x (- 1
				   (* x x 0.5d0))))
		*eps4mu*)))
    (setf (f 2)
	  (+ (/ (z 3) x x)
	     (/ (- (z 4)) x)
	     (* (z 1)
		(/ (- 1 (/ (z 1) 2 x))
		   *dmu*))))))

(defun dfsub (x a-z a-df)
  (utilities::with-fortran-matrices ((d a-df (1 2) (1 4))
				     (z a-z (1 4)))
    (setf (d 1 1)
	  (+ (/ 1 x x)
	     (/ (+ 1
		   (/ (z 3)
		      x))
		*eps4mu*)))
    (setf (d 1 2)
	  (/ -1 x))
    (setf (d 1 3)
	  (- (/ (- 1 (/ (z 1)
			x))
		*eps4mu*)))
    (setf (d 1 4)
	  0d0)
    (setf (d 2 1)
	  (/ (- 1
		(/ (z 1)
		   x))
	     *dmu*))
    (setf (d 2 2)
	  0d0)
    (setf (d 2 3)
	  (/ 1 x x))
    (setf (d 2 4)
	  (/ -1 x))))

(defun gsub (i a-z a-g)
  (utilities::with-fortran-matrices ((z a-z (1 4))
				     (g a-g (1 4)))
    (case i
      ((or 1 3)
       (setf (g 1) (z 1)))
      (2
       (setf (g 1) (z 3)))
      (4
       (setf (g 1) (+ (z 4)
		       (* -0.3d0 (z 3))
		       0.7d0))))))

(defun dgsub (i a-z a-dg)
  (utilities::with-fortran-matrices ((dg a-dg (1 4)))
    (loop for k from 1 upto 4 do
      (setf (dg k) 0d0))
    (case i
      ((or 1 3)
       (setf (dg 1) 1d0))
      (2
       (setf (dg 3) 1d0))
      (4
       (setf (dg 4) 1d0)
       (setf (dg 3) -0.3d0)))))

(defun guess (x z dmval)
  (let ((con (* *gamma* x (- 1 (* 0.5d0 x x))))
	(dcon (* *gamma* (- 1 (* 1.5d0 x x))))
	(d2con (* -3 *gamma* x)))
    (utilities::with-fortran-matrices ((z z (1 4))
				       (dmval dmval (1 2)))
      (cond ((<= x *xt*)
	     (setf (z 1) (* 2 x))
	     (setf (z 2) 2d0)
	     (setf (z 3) (+ (* -2 x) con))
	     (setf (z 4) (+ dcon -2d0))
	     (setf (dmval 2) d2con))
	    (t
	     (setf (z 1) 0d0)
	     (setf (z 2) 0d0)
	     (setf (z 3) (- con))
	     (setf (z 4) (- dcon))
	     (setf (dmval 2) (- d2con))))
      (setf (dmval 1) 0d0))))


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
