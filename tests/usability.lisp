(defpackage #:mpc
  (:use (:cl :matlisp)))

(defun pcart-field (time y &optional (ydot (zeros (dims y))))
  (declare (ignore time))
  (assert (= 4 (length (store y)) (length (store ydot))) nil "nooo!")
  (let* ((theta (ref y 1))
	 (xdot (ref y 2))
	 (thetadot (ref y 3)))
    (with-marking
	(setf (ref ydot 0) xdot
	      (ref ydot 1) thetadot
	      (ref ydot 2) (/ (+ (* (:memo (cos theta)) (:memo (sin theta))) (* (:memo (sin theta)) (expt thetadot 2))) (- 2 (expt (:memo (cos theta)) 2)))
	      (ref ydot 3) (/ (+ (* 2 (:memo (sin theta)) (* (:memo (cos theta)) (:memo (sin theta)) (expt thetadot 2))) (- (expt (:memo (cos theta)) 2) 2))))))
  ydot)

(defun rk4-stepper (field dim)
  (compile-and-eval
   `(let ((k1 (zeros ,dim))
	  (k2 (zeros ,dim))
	  (k3 (zeros ,dim))
	  (k4 (zeros ,dim))
	  (xtmp (zeros ,dim)))
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
