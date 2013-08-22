(in-package #:matlisp)

(declaim (inline draw-standard-exponential))
(defun draw-standard-exponential ()
  "Return a random variable from the Exponential(1) distribution, which has density exp(-x)."
  ;; need 1-random, because there is a small but nonzero chance of getting a 0.
  (- (log (- 1d0 (random 1d0)))))

(declaim (ftype (function () double-float) draw-standard-normal-leva draw-standard-normal-marsaglia))
(definline draw-standard-normal-leva ()
  "Draw a random number from N(0,1)."
  ;; Method from Leva (1992).  This is considered much better/faster than the Box-Muller method.
  ;; Adapted from cl-random, originally written by Tamas Papp
  ;; This tends to be just as fast as Marsaglia with storage.
  (very-quickly
    (loop
       :do (let* ((u (random 1d0))
		  (v (* 1.7156d0 (- (random 1d0) 0.5d0)))
		  (x (- u 0.449871d0))
		  (y (+ (abs v) 0.386595d0))
		  (q (+ (expt x 2) (* y (- (* 0.19600d0 y) (* 0.25472d0 x))))))
	     (declare (type double-float u v x y q))
	     (unless (and (> q 0.27597d0)
			  (or (> q 0.27846d0)
			      (plusp (+ (expt v 2) (* 4 (expt u 2) (log u))))))
	       (return (/ v u)))))))

;;Not thread safe, obviously
(let ((prev nil))
  (defun draw-standard-normal-marsaglia ()
    (if prev
	(prog1 prev
	  (setf prev nil))
	(very-quickly
	  (loop
	     :do (let* ((x (1- (random 2d0)))
			(y (1- (random 2d0)))
			(s (+ (* x x) (* y y))))
		   (declare (type double-float x y s))
		   (when (<= s 1d0)
		     (let ((mult (sqrt (/ (* -2 (log s)) s))))
		       (declare (type double-float mult))
		       (setf prev (* y mult))
		       (return (* x mult))))))))))

;;	      
(defun randn (dims)
  (let* ((ret (zeros dims 'real-tensor))
	 (sto (store ret)))
    (declare (type (simple-array double-float (*)) sto))
    (very-quickly
      (mod-dotimes (idx (dimensions ret))
	:with (linear-sums
	       (of-ret (strides ret)))
	:do (setf (aref sto of-ret) (the double-float (draw-standard-normal-leva)))))
    ret))

(defun rand (dims)
  (let* ((ret (zeros dims 'real-tensor))
	 (sto (store ret)))
    (declare (type (simple-array double-float (*)) sto))
    (very-quickly
      (mod-dotimes (idx (dimensions ret))
	:with (linear-sums
	       (of-ret (strides ret)))
	:do (setf (aref sto of-ret) (random 1d0))))
    ret))
