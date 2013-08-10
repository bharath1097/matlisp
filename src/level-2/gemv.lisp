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
	 (when (cl:char= (char-upcase ,transp) #\T)
	   (rotatef ,m ,n))
	 (,(macroexpand-1 `(t/blas-gemv-func ,sym))
	   ,transp ,m ,n
	   ,alpha
	   (the ,(store-type sym) (store ,A)) ,lda
	   (the ,(store-type sym) (store ,x)) ,st-x
	   ,beta
	   (the ,(store-type sym) (store ,y)) ,st-y
	   (the index-type (head ,A)) (the index-type (head ,x)) (the index-type (head ,y)))
	 y))))

;;
(deft/generic (t/gemv! #'subtypep) sym (alpha A x beta y transp))

;;Witness the power of macros, muggles! :)
(deft/method t/gemv! (sym standard-tensor) (alpha A x beta y transp)
  (using-gensyms (decl (alpha A x beta y transp))
   `(let (,@decl)
      (declare (type ,sym ,A ,x ,y)
	       (type ,(field-type sym) ,alpha ,beta))
      (unless (t/f= ,(field-type sym) ,beta (t/fid* ,(field-type sym)))
	(t/scdi! ,sym ,beta ,y :scal? t :numx? t))
      ;;These loops are optimized for column major matrices
      (if ,transp
	  (einstein-sum ,sym (i j) (ref ,y i) (* ,alpha (ref ,A j i) (ref ,x j)) nil)
	  (einstein-sum ,sym (j i) (ref ,y i) (* ,alpha (ref ,A i j) (ref ,x j)) nil))
      ,y)))

;;Symbolic
#+maxima
(generate-typed-gemv! symbolic-base-typed-gemv!
    (symbolic-tensor nil 0))
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
  (:method :before ((alpha number) (A standard-matrix) (x standard-vector)
		    (beta number) (y standard-vector)
		    &optional (job :n))
    (assert (member job '(:n :t :c :h)) nil 'invalid-value
	    :given job :expected `(member job '(:n :t :c :h))
	    :message "Inside gemv!")
    (assert (not (eq x y)) nil 'invalid-arguments
	    :message "GEMV!: x and y cannot be the same vector")
    (assert (and
	     (= (aref (dimensions x) 0)
		(aref (dimensions A) (if (eq job :t) 0 1)))
	     (= (aref (dimensions y) 0)
		(aref (dimensions A) (if (eq job :t) 1 0))))
	    nil 'tensor-dimension-mismatch)))

(defmethod gemv! ((alpha number) (A real-matrix) (x real-vector)
		  (beta number) (y real-vector) &optional (job :n))
  (real-typed-gemv! (coerce-real alpha) A x
		    (coerce-real beta) y job))

(defmethod gemv! ((alpha number) (A complex-matrix) (x complex-vector)
		  (beta number) (y complex-vector) &optional (job :n))
  (complex-typed-gemv! (coerce-complex alpha) A x
		       (coerce-complex beta) y job))

(defmethod gemv! ((alpha number) (A real-matrix) (x real-vector)
		  (beta number) (y complex-vector) &optional (job :n))
  (unless (= beta 1)
    (complex-typed-scal! (coerce-complex beta) y))
  (unless (= alpha 0)
    (if (not (zerop (imagpart alpha)))
	(let ((A.x (make-real-tensor (aref (dimensions y) 0)))
	      (vw-y (tensor-realpart~ y)))
	  (real-typed-gemv! (coerce-real 1) A x (coerce-real 0) A.x job)
	  ;;
	  (real-typed-axpy! (coerce-real (realpart alpha)) A.x vw-y)
	  ;;Move view to the imaginary part
	  (incf (head vw-y))
	  (real-typed-axpy! (coerce-real (imagpart alpha)) A.x vw-y))
	(real-typed-gemv! (coerce-real alpha) A x
			  (coerce-real 1) (tensor-realpart~ y) job)))
  y)

(defmethod gemv! ((alpha number) (A real-matrix) (x complex-vector)
		  (beta number) (y complex-matrix) &optional (job :n))
  (unless (= beta 1)
    (complex-typed-scal! (coerce-complex beta) y))
  (unless (= alpha 0)
    (let ((A.x (make-complex-tensor (aref (dimensions y) 0))))
      (let ((vw-x (tensor-realpart~ x))
	    (vw-A.x (tensor-realpart~ x)))
	;;Re
	(real-typed-gemv! (coerce-real 1) A vw-x (coerce-real 0) vw-A.x job)
	;;Im
	(incf (head vw-x))
	(incf (head vw-A.x))
	(real-typed-gemv! (coerce-real 1) A vw-x (coerce-real 0) vw-A.x job))
      (complex-typed-axpy! (coerce-complex alpha) A.x y)))
  y)

(defmethod gemv! ((alpha number) (A complex-matrix) (x real-vector)
		  (beta number) (y complex-vector) &optional (job :n))
  (let ((cplx-x (make-complex-tensor (aref (dimensions x) 0))))
    (real-typed-copy! x (tensor-realpart~ cplx-x))
    (complex-typed-gemv! (coerce-complex alpha) A cplx-x
			 (coerce-complex beta) y job))
  y)

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

(defmethod gemv ((alpha number) (A standard-matrix) (x standard-vector)
		 (beta number) (y complex-vector) &optional (job :n))
  (let ((result (copy y)))
    (gemv! alpha A x 1d0 result job)))

(defmethod gemv ((alpha number) (A standard-matrix) (x standard-vector)
		 (beta number) (y real-vector) &optional (job :n))
  (let ((result (if (or (complexp alpha) (complexp beta)
			(typep A 'complex-matrix) (typep x 'complex-vector))
		    (make-complex-tensor (aref (dimensions y) 0))
		    (make-real-tensor (aref (dimensions y) 0)))))
    (scal! y result)
    (gemv! alpha A x beta result job)))

(defmethod gemv ((alpha number) (A standard-matrix) (x standard-vector)
		 (beta (eql nil)) (y (eql nil)) &optional (job :n))
  (let ((result (apply
		 (if (or (complexp alpha) (complexp beta)
			 (typep A 'complex-matrix) (typep x 'complex-vector))
		     #'make-complex-tensor
		     #'make-real-tensor)
		 (list (ecase job ((:n :c) (nrows A)) ((:t :h) (ncols A)))))))
    (gemv! alpha A x 0 result job)))
