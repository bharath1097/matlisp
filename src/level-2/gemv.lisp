(in-package #:matlisp)

(deft/generic (t/blas-gemv-func #'subtypep) sym ())
(deft/method t/blas-gemv-func (sym real-tensor) ()
  'dgemv)
(deft/method t/blas-gemv-func (sym complex-tensor) ()
  'zgemv)
;;
(deft/generic (t/blas-gemv! #'subtypep) sym (alpha A lda x st-x beta y st-y transp))

(deft/method t/blas-gemv! (sym blas-numeric-tensor) (alpha A lda x st-x beta y st-y transp)
  (using-gensyms (decl (alpha A lda x st-x beta y st-y transp))
    (with-gensyms (m n)
      `(let* (,@decl
	      (,m (aref (the index-store-vector (dimensions ,A)) 0))
	      (,n (aref (the index-store-vector (dimensions ,A)) 1)))
	 (declare (type ,sym ,A ,x ,y)
		  (type ,(field-type sym) ,alpha ,beta)
		  (type index-type ,st-x ,st-y ,lda ,m ,n))
	 (,(macroexpand-1 `(t/blas-gemv-func ,sym))
	   ,transp ,m ,n
	   ,alpha
	   (the ,(store-type sym) (store ,A)) ,lda
	   (the ,(store-type sym) (store ,x)) ,st-x
	   ,beta
	   (the ,(store-type sym) (store ,y)) ,st-y
	   (the index-type (head ,A)) (the index-type (head ,x)) (the index-type (head ,y)))
	 ,y))))

;;
(deft/generic (t/gemv! #'subtypep) sym (alpha A x beta y transp))

;;Witness the power of macros, muggles! :)
(deft/method t/gemv! (sym standard-tensor) (alpha A x beta y transp)
  (using-gensyms (decl (alpha A x beta y transp))
   `(let (,@decl)
      (declare (type ,sym ,A ,x ,y)
	       (type ,(field-type sym) ,alpha ,beta)
	       (type character ,transp))
      (unless (t/f= ,(field-type sym) ,beta (t/fid* ,(field-type sym)))
	(t/scdi! ,sym ,beta ,y :scal? t :numx? t))
      ;;These loops are optimized for column major matrices
      (ecase (char-upcase ,transp)
       (#\N (einstein-sum ,sym (j i) (ref ,y i) (* ,alpha (ref ,A i j) (ref ,x j)) nil))
       (#\T (einstein-sum ,sym (i j) (ref ,y i) (* ,alpha (ref ,A j i) (ref ,x j)) nil))
       (#\C (einstein-sum ,sym (j i) (ref ,y i) (* ,alpha (t/fc ,(field-type sym) (ref ,A i j)) (ref ,x j)) nil))
       (#\H (einstein-sum ,sym (i j) (ref ,y i) (* ,alpha (t/fc ,(field-type sym) (ref ,A j i)) (ref ,x j)) nil)))
      ,y)))
;;---------------------------------------------------------------;;

(defgeneric gemv! (alpha A x beta y &optional job)
  (:documentation
   "
  Syntax
  ======
  (GEMV! alpha A x beta y [job])

  Purpose
  =======
  Performs the GEneral Matrix Vector operation given by
	       --      -      -

	    Y <- alpha * op(A) * x + beta * y

  and returns y.

  alpha,beta are scalars,
  A is a matrix, and x,y are vectors.

  op(A) means either A or A'.

     JOB                    Operation
  ---------------------------------------------------
     :N (default)      alpha * A * x + beta * y
     :T                alpha * transpose(A)* x + beta * y
     :C                alpha * conjugate(A) * x + beta * y
     :H                alpha * transpose o conjugate(A) + beta * y
")
  (:method :before (alpha (A standard-tensor) (x standard-tensor)
		    beta (y standard-tensor)
		    &optional (job :n))
    (assert (member job '(:n :t :c :h)) nil 'invalid-value
	    :given job :expected `(member job '(:n :t :c :h))
	    :message "GEMV!: Given an unknown job.")
    (assert (not (eq x y)) nil 'invalid-arguments
	    :message "GEMV!: x and y cannot be the same vector")
    (assert (and
	     (tensor-vectorp x) (tensor-vectorp y) (tensor-matrixp A)
	     (= (aref (the index-store-vector (dimensions x)) 0)
		(aref (the index-store-vector (dimensions A)) (if (member job '(:t :h)) 0 1)))
	     (= (aref (the index-store-vector (dimensions y)) 0)
		(aref (the index-store-vector (dimensions A)) (if (member job '(:t :h)) 1 0))))
	    nil 'tensor-dimension-mismatch)))

(defun ieql (&rest args)
  (loop :for ele :in (cdr args)
     :do (unless (eql (car args) ele)
	   (return nil))
     :finally (return t)))

(defun >class (cls)
  (let ((clist (reverse (sort cls #'coerceable?))))
    (loop :for ele :in (cdr clist)
       :do (unless (coerceable? ele (car clist))
	     (return nil))
       :finally (return (car clist)))))

(defun tensor-coerce (ten cls &optional (duplicate? t))
  (let ((clname (if (typep cls 'standard-class) (class-name cls) cls)))
    (if (and (not duplicate?) (typep ten clname)) ten
	(copy! ten (zeros (dimensions ten) clname)))))

(defmethod gemv! (alpha (A standard-tensor) (x standard-tensor) beta (y standard-tensor) &optional (job :n))
  (let ((clx (class-name (class-of x)))
	(cly (class-name (class-of y)))
	(cla (class-name (class-of y))))
    (assert (and (member cla *tensor-type-leaves*)
		 (member clx *tensor-type-leaves*)
		 (member cly *tensor-type-leaves*))
	    nil 'tensor-abstract-class :tensor-class (list cla clx cly))
    (cond
      ((ieql clx cly cla)
       (compile-and-eval
	`(defmethod gemv! (alpha (A ,cla) (x ,clx) beta (y ,cly) &optional (job :n))
	   (let ((alpha (t/coerce ,(field-type clx) alpha))
		 (beta (t/coerce ,(field-type clx) beta))
		 (cjob (aref (symbol-name job) 0)))
	     (declare (type ,(field-type clx) alpha beta)
		      (type character trans-op))
	     ,(recursive-append
	       (when (subtypep clx 'blas-numeric-tensor)
		 `(if (call-fortran? A (t/l2-lb ,cla))
		      (let ((A-copy (if (blas-matrix-compatiblep A cjob) A
					(let ((*default-stride-ordering* :col-major))
					  (t/copy! (,cla ,cla) A (t/zeros ,clx (dimensions A)))))))
			(multiple-value-bind (lda op maj) (blas-matrix-compatiblep A-copy cjob)
			  (declare (ignore maj))
			  (t/blas-gemv! ,cla alpha A-copy lda
					x (aref (the index-store-vector (strides x)) 0)
					beta
					y (aref (the index-store-vector (strides y)) 0)
					op)))))
	       `(t/gemv! ,cla alpha A x beta y cjob)))
	   y))
       (gemv! alpha A x beta y job))
      (t
       (error "Don't know how to apply axpy! to classes ~a, ~a." clx cly)))))
;;---------------------------------------------------------------;;
(defgeneric gemv (alpha A x beta y &optional job)
  (:documentation
   "
  Syntax
  ======
  (GEMV alpha A x beta y [job])

  Purpose
  =======
  Returns the GEneral Matrix Vector operation given by

	    alpha * op(A) * x + beta * y

  alpha,beta are scalars,
  A is a matrix, and x,y are vectors.

  op(A) means either A or A'.

     JOB                    Operation
  ---------------------------------------------------
     :N (default)      alpha * A * x + beta * y
     :T                alpha * A'* x + beta * y
     :C                alpha * conjugate(A) * x + beta * y
     :H                alpha * transpose o conjugate(A) + beta * y
"))

(defmethod gemv (alpha (A standard-tensor) (x standard-tensor)
		 beta (y standard-tensor) &optional (job :n))
  (gemv! alpha A x beta (copy y) job))
