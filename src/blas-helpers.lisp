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
