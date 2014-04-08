(in-package :matlisp)

(defun norm (vec &optional (n 2))
  (declare (type real-tensor vec))
  (cond
    ((typep n 'real)
     (let-typed ((sum 0d0 :type double-float))
       (dorefs (idx (dimensions vec))
	       ((ref vec :type real-tensor))
	 (incf sum (expt (abs ref) n)))
     (expt sum (/ 1 n))))
    ((eql n :sup)
     (tensor-foldl real-tensor max vec 0d0))))
