(in-package #:matlisp)

(declaim (ftype (function () double-float) draw-standard-normal draw-standard-normal-marsaglia draw-standard-exponential))

(definline draw-standard-exponential ()
  "Return a random variable from the Exponential(1) distribution, which has density exp(-x)."
  ;; Adapted from cl-random, originally written by Tamas Papp
  ;; need 1-random, because there is a small but nonzero chance of getting a 0.
  (- (log (- 1d0 (random 1d0)))))

(definline draw-standard-normal ()
  "Draw a random number from N(0,1)."
  ;; Method from Leva (1992).  This is considered much better/faster than the Box-Muller method.
  ;; Adapted from cl-random, originally written by Tamas Papp
  ;; This seems to be just as fast as Marsaglia with storage.
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
(defmacro fill-tensor (type (func tensor))
  (using-gensyms (decl (tensor))
    (with-gensyms (sto ofst)
      `(let* (,@decl
	      (,sto (store ,tensor)))
       (declare (type ,type ,tensor)
		(type ,(store-type type) ,sto))
       (very-quickly
	 (mod-dotimes (idx (dimensions ,tensor))
	   :with (linear-sums
		  (,ofst (strides ,tensor)))
	   :do (t/store-set ,type ,(etypecase func (symbol `(,func)) (cons func))  ,sto ,ofst)))
       ,tensor))))

(macrolet ((generate-rand (func clause)
	     (let ((clause (etypecase clause
			     (symbol `(,clause))
			     (cons clause))))
	       `(defun ,func (&optional dims)
		  (if dims
		      (fill-tensor real-tensor (,clause (zeros dims 'real-tensor)))
		      ,clause))))
	   (generate-rands ((&rest args))
	     `(progn
		,@(mapcar #'(lambda (x) `(generate-rand ,(car x) ,(cadr x))) args))))
  (generate-rands ((randn (draw-standard-normal))
		   (rand (random 1d0))
		   (rande (draw-standard-exponential)))))

