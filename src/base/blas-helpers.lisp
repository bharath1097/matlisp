(in-package #:matlisp)

;;Check dimensions of the tensors before passing the argument here!
(defun blas-copyable-p (ten-a ten-b)
  (declare (type standard-tensor ten-a ten-b))
  (mlet*
   (((sort-std-a std-a-perm) (let-typed ((std-a (strides ten-a) :type index-store-vector))
			       (very-quickly (sort-permute (copy-seq std-a) #'<)))
     :type (index-store-vector permutation-action))
    (perm-a-dims (permute (dimensions ten-a) std-a-perm) :type index-store-vector)
    ;;If blas-copyable then the strides must have the same sorting permutation.
    (sort-std-b (permute (strides ten-b) std-a-perm) :type index-store-vector)
    (perm-b-dims (permute (dimensions ten-b) std-a-perm) :type index-store-vector))
   (very-quickly
     (loop
	:for i :of-type index-type :from 0 :below (rank ten-a)
	:for sost-a :across sort-std-a
	:for a-aoff :of-type index-type := (aref sort-std-a 0) :then (the index-type (* a-aoff (aref perm-a-dims (1- i))))
	;;
	:for sost-b :across sort-std-b
	:for b-aoff :of-type index-type := (aref sort-std-b 0) :then (the index-type (* b-aoff (aref perm-b-dims (1- i))))
	;;
	:do (progn
	      (unless (and (= sost-a a-aoff)
			   (= sost-b b-aoff))
		(return nil)))
	:finally (return (list (aref sort-std-a 0) (aref sort-std-b 0)))))))

(defun consecutive-store-p (tensor)
  (declare (type standard-tensor tensor))
  (mlet* (((sort-std std-perm) (let-typed ((stds (strides tensor) :type index-store-vector))
				 (very-quickly (sort-permute (copy-seq stds) #'<)))
	   :type (index-store-vector permutation))
	  (perm-dims (permute (dimensions tensor) std-perm) :type index-store-vector))
      (very-quickly
	(loop
	   :for so-st :across sort-std
	   :for so-di :across perm-dims
	   :and accumulated-off := (aref sort-std 0) :then (the index-type (* accumulated-off so-di))
	   :unless (= so-st accumulated-off) :do (return nil)
	   :finally (return (aref sort-std 0))))))

(defun blas-matrix-compatible-p (matrix op)
  (declare (type standard-matrix matrix))
  (let*-typed ((stds (strides matrix) :type index-store-vector)
	       (rs (aref stds 0) :type index-type)
	       (cs (aref stds 1) :type index-type))
    ;;Note that it is not required that (rs = nc * cs) or (cs = nr * rs)
    (cond
      ((= cs 1) (values :row-major rs (fortran-nop op)))
      ((= rs 1) (values :col-major cs (fortran-op op)))
      (t (values nil 0 "?")))))

(definline fortran-op (op)
  (ecase op (:n "N") (:t "T")))

(definline fortran-nop (op)
  (ecase op (:t "N") (:n "T")))

(defun fortran-snop (sop)
  (cond
    ((string= sop "N") "T")
    ((string= sop "T") "N")
    (t (error "Unrecognised fortran-op."))))

(defun split-job (job)
  (values-list
   (map 'list #'(lambda (x) (intern (string x) "KEYWORD")) (symbol-name job))))

(defun combine-jobs (&rest jobs)
  (let ((job (intern (apply #'concatenate 'string (mapcar #'symbol-name jobs)) "KEYWORD")))
    job))

(definline flip-major (job)
  (declare (type symbol job))
  (case job
    (:row-major :col-major)
    (:col-major :row-major)))

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
