
(defun tdcopy (n)
  (let* ((t-a (make-real-tensor-dims n n n))
	 (st-a (store t-a))
	 (t-b (make-real-tensor-dims n n n))
	 (st-b (store t-b)))
     (with-optimization (:speed 3 :safety 0 :space 0)
       (mod-dotimes (idx (idxv n n))
	 with (linear-sums
	       (of (idxv (* n n) n)))
	 do (dcopy n st-a 1 st-b 1 of of)))))

(defun tcopy (n)
  (let* ((t-a (make-real-tensor-dims n n n))
	 (t-b (make-real-tensor-dims n n n)))
    (time (real-tensor-copy t-a t-b))))

(defun modidx (n dims)
  (declare (optimize (speed 3) (safety 0))
	   (type index-type n)
	   (type cons dims))
  (multiple-value-bind (div rem) (let ((div (car dims)))
				   (declare (type index-type div))
				   (floor n div))
    (declare (ignore div))
    (if (null (cdr dims)) t
	(modidx rem (cdr dims)))))

(defun test-axpy ()
  (let ((x (copy! pi (make-real-tensor 1000 1000)))
	(y (make-real-tensor 1000 1000)))
    (time (axpy! 1d0 x y))
    t))

(defun test-mm-lisp (n)
  (let ((t-a (make-real-tensor n n))
	(t-b (make-real-tensor n n))
	(t-c (make-real-tensor n n)))
    (declare (type real-tensor t-a t-b t-c))
    (let ((st-a (store t-a))
	  (st-b (store t-b))
	  (st-c (store t-c)))
      (declare (type (real-array *) st-a st-b st-c))
      (very-quickly
	(mod-dotimes (idx (dimensions t-a))
	  with (linear-sums
		(of-a (strides t-a))
		(of-b (strides t-b))
		(of-c (strides t-c)))
	  do (setf (aref st-a of-a) (random 1d0)
		   (aref st-b of-b) (random 1d0)
		   (aref st-c of-c) 0d0)))
      (time 
       (very-quickly
      	 (mod-dotimes (idx (idxv n n n))
      	   with (loop-order :row-major)
      	   with (linear-sums
      		 (of-a (idxv n 1 0))
      		 (of-b (idxv 0 n 1))
      		 (of-c (idxv n 0 1)))
      	   do (incf (aref st-c of-c) (* (aref st-a of-a) (aref st-b of-b)))))))))  

(defun test-mm-ddot (n)
  (let* ((t-a (make-real-tensor n n))
	 (t-b (make-real-tensor n n))
	 (t-c (make-real-tensor n n))
	 (st-a (store t-a))
	 (st-b (store t-b))
	 (st-c (store t-c)))
    (declare (type real-tensor t-a t-b t-c)
	     (type (real-array *) st-a st-b st-c))
    (mod-dotimes (idx (dimensions t-a))
      with (linear-sums
	    (of-a (strides t-a))
	    (of-b (strides t-b))
	    (of-c (strides t-c)))
      do (setf (aref st-a of-a) (random 1d0)
	       (aref st-b of-b) (random 1d0)
	       (aref st-c of-c) 0d0))
    (time     
     (very-quickly
       (mod-dotimes (idx (idxv n n))
	 with (loop-order :row-major)
	 with (linear-sums
	       (of-a (idxv n 0))
	       (of-b (idxv 0 1))
	       (of-c (idxv n 1)))
	 do (setf (aref st-c of-c)
		  (ddot n st-a 1 st-b n of-a of-b)))))))
