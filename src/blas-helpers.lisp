(in-package :matlisp)

(definline fortran-op (op)
  (ecase op (:n "N") (:t "T")))

(definline fortran-nop (op)
  (ecase op (:t "N") (:n "T")))

(defun fortran-snop (sop)
  (cond
    ((string= sop "N") "T")
    ((string= sop "T") "N")
    (t (error "Unrecognised fortran-op."))))

(defun blas-copyable-p (matrix)
  (declare (type (or real-matrix complex-matrix) matrix))
  (mlet* ((nr (nrows matrix) :type fixnum)
	  (nc (ncols matrix) :type fixnum)
	  (rs (row-stride matrix) :type fixnum)
	  (cs (col-stride matrix) :type fixnum)
	  (ne (number-of-elements matrix) :type fixnum))
	 (very-quickly
	   (cond
	     ((or (= nc 1) (= cs (* nr rs))) (values t rs ne))
	     ((or (= nr 1) (= rs (* nc cs))) (values t cs ne))
	     (t (values  nil -1 -1))))))

(defun blas-matrix-compatible-p (matrix &optional (op :n))
  (declare (optimize (safety 0) (speed 3))
	   (type (or real-matrix complex-matrix) matrix))
  (mlet* (((rs cs) (slot-values matrix '(row-stride col-stride))
	   :type (fixnum fixnum)))
	 (cond
	   ((= cs 1) (values :row-major rs (fortran-nop op)))
	   ((= rs 1) (values :col-major cs (fortran-op op)))
	   ;;Lets not confound lisp's type declaration.
	   (t (values nil -1 "?")))))


(defun col-major-p (strides dims)
  (declare (type (index-array *) strides dims))
  (very-quickly
    (loop
       for off across strides
       and dim across dims
       and accumulated-off = 1 then (* accumulated-off dim)
       unless (= off accumulated-off) do (return nil)
       finally (return t))))

(defun row-major-p (strides dims)
  (declare (type (index-array *) strides dims))
  (very-quickly
    (loop
       for idx of-type index-type from (1- (length dims)) downto 0
       for dim of-type index-type = (aref dims idx)
       for off of-type index-type = (aref strides idx)
       and accumulated-off of-type index-type = 1 then (* accumulated-off dim)
       unless (= off accumulated-off) do (return nil)
       finally (return t))))

(defun same-dimension-p (a b)
  (declare (type (index-array *) a b))
  (let ((l-a (length a)))
    (when (= l-a (length b))
      (very-quickly
	(loop
	   for i from 0 below l-a
	   unless (= (aref a i) (aref b i))
	   do (return nil)
	   finally (return t))))))

(defun consecutive-store-p (strides dims)
  (declare (type (index-array *) strides dims))
  (let* ((stride-dims (very-quickly
			(sort (apply #'vector
				     (loop
					for std across strides
					and dim across dims
					collect `(,std ,dim)))
			      #'< :key #'car)))
	 (stride-min (first (aref stride-dims 0))))
    (declare (type index-type stride-min)
	     (type (simple-vector *) stride-dims))
    (very-quickly
      (loop
	 for st-di across stride-dims
	 and accumulated-off = stride-min then (* accumulated-off (second st-di))
	 unless (= (first st-di) accumulated-off) do (return nil)
	 finally (return stride-min)))))
