(in-package #:matlisp)

(definline fortran-nop (op)
  (ecase op (#\T #\N) (#\N #\T)))

(defun split-job (job)
  (declare (type symbol job))
  (let-typed ((name (symbol-name job) :type string))
    (loop :for x :across name :collect x)))

(definline flip-major (job)
  (declare (type symbol job))
  (case job
    (:row-major :col-major)
    (:col-major :row-major)))

(defun blas-copyable-p (ten-a ten-b)
  (declare (type standard-tensor ten-a ten-b))
  (when (= (rank ten-a) (rank ten-b))
    (mlet*
     (((sort-std-a std-a-perm) (very-quickly (sort-permute-base (copy-seq (the index-store-vector (strides ten-a))) #'<)) :type (index-store-vector pindex-store-vector))
      (perm-a-dims (very-quickly (apply-action! (copy-seq (the index-store-vector (dimensions ten-a))) std-a-perm)) :type index-store-vector)
      ;;If blas-copyable then the strides must have the same sorting permutation.
      (sort-std-b (very-quickly (apply-action! (copy-seq (the index-store-vector (strides ten-b))) std-a-perm)) :type index-store-vector)
      (perm-b-dims (very-quickly (apply-action! (copy-seq (the index-store-vector (dimensions ten-b))) std-a-perm)) :type index-store-vector))
     (very-quickly
       (loop
	  :for i :of-type index-type :from 0 :below (rank ten-a)
	  :for sost-a :across sort-std-a
	  :for a-aoff :of-type index-type := (aref sort-std-a 0) :then (the index-type (* a-aoff (aref perm-a-dims (1- i))))
	  ;;
	  :for sost-b :across sort-std-b
	  :for b-aoff :of-type index-type := (aref sort-std-b 0) :then (the index-type (* b-aoff (aref perm-b-dims (1- i))))
	  ;;
	  :do (unless (and (= sost-a a-aoff)
			   (= sost-b b-aoff)
			   (= (aref perm-a-dims i) (aref perm-b-dims i)))
		(return nil))
	  :finally (return (list (aref sort-std-a 0) (aref sort-std-b 0))))))))

(defmemo consecutive-store-p (tensor)
  (declare (type standard-tensor tensor))
  (mlet* (((sort-std std-perm) (very-quickly (sort-permute-base (copy-seq (the index-store-vector (strides tensor))) #'<))
	   :type (index-store-vector pindex-store-vector))
	  (perm-dims (very-quickly (apply-action! (copy-seq (the index-store-vector (dimensions tensor))) std-perm)) :type index-store-vector))
	 (very-quickly
	   (loop
	      :for so-st :across sort-std
	      :for so-di :across perm-dims
	      :and accumulated-off := (aref sort-std 0) :then (the index-type (* accumulated-off so-di))
	      :unless (= so-st accumulated-off) :do (return nil)

	      :finally (return (values t (aref sort-std 0)))))))

(definline blas-matrix-compatible-p (matrix op)
  (declare (type standard-matrix matrix)
	   (type character op))
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
