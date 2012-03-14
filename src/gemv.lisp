(in-package :matlisp)

(defun real-double-gemv!-typed (alpha A x beta y job)
  ;;Be very careful when you use this function!
  ;;Indexes can be tricky and this has no safety net
  ;;(you don't see a matrix-ref do you ?)
  ;;Use only after checking the arguments for compatibility.
  (declare (type double-float alpha beta)
	   (type real-matrix A x y)
	   (type symbol job))
  (mlet* ((fort-op (ecase job (:n "N") (:t "T")) :type ((string 1)))
	  ((st-a hd-a nr-a nc-a rs-a cs-a) (slot-values A '(store head number-of-rows number-of-cols row-stride col-stride))
	   :type ((real-matrix-store-type *) fixnum fixnum fixnum fixnum fixnum))
	  ((st-x hd-x rs-x) (slot-values x '(store head row-stride))
	   :type ((real-matrix-store-type *) fixnum fixnum))
	  ((st-y hd-y rs-y) (slot-values y '(store head row-stride))
	   :type ((real-matrix-store-type *) fixnum fixnum))
	  ((sym lda tf-op) (blas-matrix-compatible-p A fort-op) :type (symbol fixnum (string 1))))
	 (if (not (string= tf-op "?"))
	     (progn
	       (when (eq sym :row-major)
		 (rotatef nr-a nc-a)
		 (rotatef rs-a cs-a))
	       (blas:dgemv tf-op nr-a nc-a alpha st-a lda st-x rs-x beta st-y rs-y :head-a hd-a :head-x hd-x :head-y hd-y))
	     (progn
	       (when (string= fort-op "T")
		 (rotatef nr-a nc-a)
		 (rotatef rs-a cs-a))
	       (dotimes (i nr-a)
		 (setf (matrix-ref-2d y i 0) (+ (* alpha (blas:ddot nc-a st-a cs-a st-x rs-x :head-x (+ hd-a (* i rs-a)) :head-y hd-x))
						(* beta (matrix-ref-2d y i 0))))))))
	 y)