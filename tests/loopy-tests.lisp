
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

(definline idxv (&rest dims)
  (make-array (length dims) :element-type 'index-type :initial-contents dims))

(defun test-mm-lisp (n)
  (let ((t-a (make-real-tensor n n))
	(t-b (make-real-tensor n n))
	(t-c (make-real-tensor n n)))
    (declare (type real-tensor t-a t-b t-c))
    (let ((st-a (store t-a))
	  (st-b (store t-b))
	  (st-c (store t-c))
	  (rstrd-a (row-stride t-a))
	  (cstrd-a (col-stride t-a))
	  (rstrd-b (row-stride t-b))
	  (cstrd-b (col-stride t-b))
	  (rstrd-c (row-stride t-c))
	  (cstrd-c (col-stride t-c))
	  (nr-c (nrows t-c))
	  (nc-c (ncols t-c))
	  (nc-a (ncols t-a))
	  (hd-a (head t-a))
	  (hd-b (head t-b))
	  (hd-c (head t-c)))
      (declare (type real-store-vector st-a st-b st-c)
	       (type index-type rstrd-a cstrd-a rstrd-b cstrd-b rstrd-c cstrd-c nr-c
		     nc-c nc-a hd-a hd-b hd-c))
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
	 (loop repeat nr-c
	    for rof-a of-type index-type = hd-a then (+ rof-a rstrd-a)
	    for rof-c of-type index-type = hd-c then (+ rof-c rstrd-c)
	    do (loop repeat nc-c
		  for cof-b of-type index-type = hd-b then (+ cof-b cstrd-b)
		  for of-c of-type index-type = rof-c then (+ of-c cstrd-c)
		  do (loop repeat nc-a
			for of-a of-type index-type = rof-a then (+ of-a cstrd-a)
			for of-b of-type index-type = cof-b then (+ of-b rstrd-b)
			summing (* (aref st-a of-a) (aref st-b of-b)) into sum of-type real-type
			finally (setf (aref st-c of-c) sum))))
	 #+nil
      	 (mod-dotimes (idx (dimensions t-c))
      	   with (loop-order :row-major)
      	   with (linear-sums
      		 (rof-a (idxv rstrd-a 0) (head t-a))
      		 (cof-b (idxv 0 cstrd-b) (head t-b))
      		 (of-c (strides t-c) (head t-c)))
      	   do (loop repeat nc-a
		 for of-a of-type index-type = rof-a then (+ of-a cstrd-a)
		 for of-b of-type index-type = cof-b then (+ of-b rstrd-b)
		 summing (* (aref st-a of-a) (aref st-b of-b)) into sum of-type real-type
		 finally (setf (aref st-c of-c) sum)))
	 #+nil
	 (mod-dotimes (idx (idxv n n n))
	   with (loop-order :row-major)
	   with (linear-sums
		 (of-a (idxv n 0 1))
		 (of-b (idxv 0 1 n))
		 (of-c (idxv n 1 0)))
	   do (incf (aref st-c of-c) (* (aref st-a of-a) (aref st-b of-b))))))
      (values t-a t-b t-c))))


(defun test-mm-ddot (n)
  (let ((t-a (make-real-tensor n n))
	(t-b (make-real-tensor n n))
	(t-c (make-real-tensor n n)))
    (declare (type real-tensor t-a t-b t-c))
    (let ((st-a (store t-a))
	  (st-b (store t-b))
	  (st-c (store t-c))
	  (rstrd-a (row-stride t-a))
	  (cstrd-a (col-stride t-a))
	  (rstrd-b (row-stride t-b))
	  (cstrd-b (col-stride t-b))
	  (rstrd-c (row-stride t-c))
	  (cstrd-c (col-stride t-c))
	  (nr-c (nrows t-c))
	  (nc-c (ncols t-c))
	  (nc-a (ncols t-a))
	  (hd-a (head t-a))
	  (hd-b (head t-b))
	  (hd-c (head t-c)))
      (declare (type (real-array *) st-a st-b st-c)
	       (type index-type rstrd-a cstrd-a rstrd-b cstrd-b rstrd-c cstrd-c nr-c
		     nc-c nc-a hd-a hd-b hd-c))
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
	 (loop repeat nr-c
	    for rof-a of-type index-type = hd-a then (+ rof-a rstrd-a)
	    for rof-c of-type index-type = hd-c then (+ rof-c rstrd-c)
	    do (loop repeat nc-c
		  for cof-b of-type index-type = hd-b then (+ cof-b cstrd-b)
		  for of-c of-type index-type = rof-c then (+ of-c cstrd-c)
		  do (let ((dotp (ddot nc-a
				       st-a cstrd-a
				       st-b rstrd-b
				       rof-a cof-b)))
		       (declare (type real-type dotp))
		       (setf (aref st-c of-c) dotp))))

      	 #+nil(mod-dotimes (idx (dimensions t-c))
      	   with (loop-order :row-major)
      	   with (linear-sums
      		 (of-a (idxv (row-stride t-a) 0) (head t-a))
      		 (of-b (idxv 0 (col-stride t-b)) (head t-b))
      		 (of-c (strides t-c) (head t-c)))
      	   do (incf (aref st-c of-c) (* (aref st-a of-a) (aref st-b of-b)))))))))


(defun test-mm-daxpy (n)
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
	       (of-a (idxv 1 0))
	       (of-b (idxv n 1))
	       (of-c (idxv 1 0)))
	 do (daxpy n (aref st-b of-b) st-a n st-c n
		   of-a of-c))))))


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
