(in-package #:matlisp)

(defmacro make-tensor-maker (func-name (tensor-class))
  (let ((opt (get-tensor-class-optimization-hashtable tensor-class)))
    (assert opt nil 'tensor-cannot-find-optimization :tensor-class tensor-class)
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
	 (let ((opt (get-tensor-class-optimization-hashtable ',tensor-class)))
	   (assert opt nil 'tensor-cannot-find-optimization :tensor-class ',tensor-class)
	   (setf (getf opt :maker) ',func-name
		 (get-tensor-class-optimization ',tensor-class) opt)))
       (defun ,func-name (&rest args)
	 (labels ((make-dims (dims)
		    (declare (type cons dims))
		    (let*-typed ((vdim (make-index-store dims) :type index-store-vector)
				 (ss (very-quickly (lvec-foldl #'(lambda (x y) (the index-type (* x y))) vdim)))
				 (store (,(getf opt :store-allocator) ss))
				 (rnk (length vdim))
				 (ret (let ((*check-after-initializing?* nil))
					(make-instance (case rnk (2 ',(getf opt :matrix)) (1 ',(getf opt :vector)) (t ',tensor-class))
						       :strides (make-stride vdim)
						       :store store :store-size ss :dimensions vdim))))
				(setf (number-of-elements ret) ss)
				ret))
		  (make-from-array (arr)
		    (declare (type (array * *) arr))
		    (let* ((ret (make-dims (array-dimensions arr)))
			   (st-r (store ret))
			   (lst (make-list (rank ret))))
		      (declare (type ,tensor-class ret)
			       (type ,(linear-array-type (getf opt :store-type)) st-r))
		      (mod-dotimes (idx (dimensions ret))
			with (linear-sums
			      (of-r (strides ret) (head ret)))
			do (,(getf opt :value-writer) (,(getf opt :coercer) (apply #'aref arr (lvec->list! idx lst))) st-r of-r))
		      ret))
		  (make-from-list (lst)
		    (let* ((ret (make-dims (list-dimensions lst)))
			   (st-r (store ret)))
		      (declare (type ,tensor-class ret)
			       (type ,(linear-array-type (getf opt :store-type)) st-r))
		      (list-loop (idx ele lst)
				 with (linear-sums
				       (of-r (strides ret) (head ret)))
				 do (,(getf opt :value-writer) (,(getf opt :coercer) ele) st-r of-r))
		      ret)))
	   (let ((largs (length args)))
	     (if (= largs 1)
		 (etypecase (first args)
		   (array
		    (make-from-array (first args)))
		   (cons
		    (make-from-list (first args)))
		   (integer
		    (make-dims (list (first args)))))
		 (make-dims args))))))))

(make-tensor-maker make-real-tensor (real-tensor))
(make-tensor-maker make-complex-tensor (complex-tensor))

#+maxima
(make-tensor-maker make-symbolic-tensor (symbolic-tensor))

;;Had to move it here in the wait for copy!
(definline sub-tensor (tensor subscripts &optional (preserve-rank nil))
  (copy (sub-tensor~ tensor subscripts preserve-rank)))

(defmacro make-zeros-dims (func-name (tensor-class))
  (let ((opt (get-tensor-class-optimization-hashtable tensor-class)))
    (assert opt nil 'tensor-cannot-find-optimization :tensor-class tensor-class)
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
	 (let ((opt (get-tensor-class-optimization-hashtable ',tensor-class)))
	   (assert opt nil 'tensor-cannot-find-optimization :tensor-class ',tensor-class)
	   (setf (getf opt :zero-maker) ',func-name
		 (get-tensor-class-optimization ',tensor-class) opt)))
       (defun ,func-name (dims)
	 (declare (type (or cons index-store-vector) dims))
	 (let*-typed ((dims (if (consp dims) (make-index-store dims) (copy-seq dims)) :type index-store-vector)
		      (rnk (length dims) :type index-type)
		      (size (very-quickly (lvec-foldl #'(lambda (a b) (declare (type index-type a b)) (the index-type (* a b))) dims))))
		     (make-instance (case rnk (2 ',(getf opt :matrix)) (1 ',(getf opt :vector)) (t ',tensor-class))
				   :dimensions dims :store (,(getf opt :store-allocator) size) :store-size size))))))

(make-zeros-dims real-typed-zeros (real-tensor))
(make-zeros-dims complex-typed-zeros (complex-tensor))

#+maxima
(make-zeros-dims symbolc-typed-tensor (symbolic-tensor))
