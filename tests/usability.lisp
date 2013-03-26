(defpackage #:mpc
  (:use (:cl :matlisp)))

(defun pcart-field (time y &optional (ydot (real-typed-zeros (dimensions y))))
  (declare (ignore time))
  (assert (= 4 (length (store y)) (length (store ydot))) nil "nooo!")
  (let*-typed ((sto-y (store y) :type real-store-vector)
	       (sto-yd (store ydot) :type real-store-vector)
	       (theta (aref sto-y 1) :type double-float)
	       (xdot (aref sto-y 2) :type double-float)
	       (thetadot (aref sto-y 3) :type double-float))
	      (very-quickly
		(setf (aref sto-yd 0) xdot
		      (aref sto-yd 1) thetadot
		      (aref sto-yd 2) (/ (+ (* (cos theta) (sin theta)) (* (sin theta) (expt thetadot 2))) (- 2 (expt (cos theta) 2)))
		      (aref sto-yd 3) (/ (+ (* 2 (sin theta)) (* (cos theta) (sin theta) (expt thetadot 2))) (- (expt (cos theta) 2) 2)))))
  ydot)

(defun rk4-stepper (field dim)
  (compile-and-eval
   `(let ((k1 (make-real-tensor ,dim))
	  (k2 (make-real-tensor ,dim))
	  (k3 (make-real-tensor ,dim))
	  (k4 (make-real-tensor ,dim))
	  (xtmp (make-real-tensor ,dim)))
      (lambda (dtm tm x0)
	(declare (type double-float dtm tm)
		 (type real-tensor x0))
	(scal! dtm (funcall ,field tm x0 k1))
	(scal! dtm (funcall ,field (+ tm (/ dtm 2d0)) (axpy! 0.5d0 k1 (copy! x0 xtmp)) k2))
	(scal! dtm (funcall ,field (+ tm (/ dtm 2d0)) (axpy! 0.5d0 k2 (copy! x0 xtmp)) k3))
	(scal! dtm (funcall ,field (+ tm dtm) (axpy! 1d0 k3 (copy! x0 xtmp)) k4))
	(axpy! (/ 1d0 6d0) k1 x0)
	(axpy! (/ 1d0 3d0) k2 x0)
	(axpy! (/ 1d0 3d0) k3 x0)
	(axpy! (/ 1d0 6d0) k4 x0)))))

;;This is far too verbose. Probably okay for performance intensive stuff, but annoying
;;for quick prototyping
