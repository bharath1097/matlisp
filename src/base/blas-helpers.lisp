(in-package #:matlisp)
   
(defun consecutive-storep (tensor)
  (declare (type standard-tensor tensor))
  (memoizing (tensor consecutive-storep)
    (mlet* (((sort-std std-perm) (very-quickly (sort-permute-base (copy-seq (the index-store-vector (strides tensor))) #'<))
	     :type (index-store-vector pindex-store-vector))
	    (perm-dims (very-quickly (apply-action! (copy-seq (the index-store-vector (dimensions tensor))) std-perm)) :type index-store-vector))
	   (very-quickly
	     (loop
		:for so-st :across sort-std
		:for so-di :across perm-dims
		:and accumulated-off := (aref sort-std 0) :then (the index-type (* accumulated-off so-di))
		:unless (= so-st accumulated-off) :do (return (values nil perm-dims sort-std std-perm))
		:finally (return (values (aref sort-std 0) perm-dims sort-std std-perm)))))))

(defun blas-copyablep (ten-a ten-b)
  (declare (type standard-tensor ten-a ten-b))
  (when (= (rank ten-a) (rank ten-b))
    (mlet*
     (((csto-a? pdims-a tmp perm-a) (consecutive-storep ten-a) :type (t index-store-vector nil pindex-store-vector))
      ((csto-b? pdims-b tmp perm-b) (consecutive-storep ten-b) :type (t index-store-vector nil pindex-store-vector)))
     (when (and csto-a? csto-b? (very-quickly (lvec-eq perm-a perm-b)) (very-quickly (lvec-eq pdims-a pdims-b)))
       (list csto-a? csto-b?)))))

(definline fortran-nop (op)
  (ecase op (#\t #\n) (#\n #\t)))

(defun split-job (job)
  (declare (type symbol job))
  (let-typed ((name (symbol-name job) :type string))
    (loop :for x :across name :collect (char-downcase x))))

(definline flip-major (job)
  (declare (type symbol job))
  (case job
    (:row-major :col-major)
    (:col-major :row-major)))

(definline blas-matrix-compatiblep (matrix op)
  (declare (type standard-tensor matrix)
	   (type character op))
  (assert (tensor-matrixp matrix) nil 'tensor-not-matrix)
  (let*-typed ((stds (strides matrix) :type index-store-vector)
	       (rs (aref stds 0) :type index-type)
	       (cs (aref stds 1) :type index-type))
    ;;Note that it is not required that (rs = nc * cs) or (cs = nr * rs)
    (cond
      ((= cs 1) (values :row-major rs (fortran-nop op)))
      ((= rs 1) (values :col-major cs op)))))

;;Stride makers.
(definline make-stride-rmj (dims)
  (declare (type index-store-vector dims))
  (let-typed ((stds (allocate-index-store (length dims)) :type index-store-vector))
    (very-quickly
      (loop
	 :for i  :of-type index-type :downfrom (1- (length dims)) :to 0
	 :and st :of-type index-type := 1 :then (the index-type (* st (aref dims i)))	 
	 :do (progn
	       (assert (> st 0) nil 'tensor-invalid-dimension-value :argument i :dimension (aref dims i))
	       (setf (aref stds i) st))
	 :finally (return (values stds st))))))

(definline make-stride-cmj (dims)
  (declare (type index-store-vector dims))
  (let-typed ((stds (allocate-index-store (length dims)) :type index-store-vector))
    (very-quickly
      (loop
	 :for i :of-type index-type :from 0 :below (length dims)
	 :and st :of-type index-type := 1 :then (the index-type (* st (aref dims i)))
	 :do (progn
	       (assert (> st 0) nil 'tensor-invalid-dimension-value :argument i :dimension (aref dims i))
	       (setf (aref stds i) st))
	 :finally (return (values stds st))))))

(defun make-stride (dims)
  (ecase *default-stride-ordering* (:row-major (make-stride-rmj dims)) (:col-major (make-stride-cmj dims))))

(definline call-fortran? (x lb)
  (declare (type standard-tensor x))
  (> (lvec-max (the index-store-vector (dimensions x))) lb))
