(in-package #:matlisp)

(deft/generic (t/blas-gemm-func #'subtypep) sym ())
(deft/method t/blas-gemm-func (sym real-tensor) ()
  'dgemm)
(deft/method t/blas-gemm-func (sym complex-tensor) ()
  'zgemm)
;;
(deft/generic (t/blas-gemm! #'subtypep) sym (alpha A lda B ldb beta C ldc transa transb))

(deft/method t/blas-gemm! (sym blas-numeric-tensor) (alpha A lda B ldb beta C ldc transa transb)
  (using-gensyms (decl (alpha A lda B ldb beta C ldc transa transb))
    (with-gensyms (m n k)
      `(let* (,@decl
	      (,m (aref (the index-store-vector (dimensions ,C)) 0))
	      (,n (aref (the index-store-vector (dimensions ,C)) 1))
	      (,k (aref (the index-store-vector (dimensions ,A)) (ecase (char-upcase ,transa) ((#\N #\C) 1) ((#\T #\H) 0)))))
	 (declare (type ,sym ,A ,B ,C)
		  (type ,(field-type sym) ,alpha ,beta)
		  (type index-type ,lda ,ldb ,ldc ,m ,n ,k)
		  (type character ,transa ,transb))
	 (,(macroexpand-1 `(t/blas-gemm-func ,sym))
	   ,transa ,transb
	   ,m ,n ,k
	   ,alpha
	   (the ,(store-type sym) (store ,A)) ,lda
	   (the ,(store-type sym) (store ,B)) ,ldb
	   ,beta
	   (the ,(store-type sym) (store ,C)) ,ldc
	   (the index-type (head ,A)) (the index-type (head ,B)) (the index-type (head ,C)))
	 ,C))))

;;
(deft/generic (t/gemm! #'subtypep) sym (alpha A B beta C transa transb))

;;Witness the power of macros, muggles! :)
(deft/method t/gemm! (sym standard-tensor) (alpha A B beta C transa transb)
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


;;Real
(generate-typed-gemm! real-base-typed-gemm!
    (real-tensor dgemm *real-l3-fcall-lb*))

(definline real-typed-gemm! (alpha A B beta C job)
  (real-base-typed-gemm! alpha A B beta C
			 (apply #'combine-jobs
				(mapcar #'(lambda (x)
					    (ecase x ((:n :t) x) (:h :t) (:c :n)))
					(multiple-value-list (split-job job))))))

;;Complex
(generate-typed-gemm! complex-base-typed-gemm!
    (complex-tensor zgemm *complex-l3-fcall-lb*))

(definline complex-typed-gemm! (alpha A B beta C job)
  (declare (type complex-matrix A B C)
	   (type complex-type alpha beta)
	   (type symbol job))
  (multiple-value-bind (job-A job-B) (split-job job)
    (if (and (member job-A '(:n :t))
	     (member job-B '(:n :t)))
	(complex-base-typed-gemm! alpha A B beta C job)
	(let ((A (ecase job-A
		   ((:n :t) A)
		   ((:h :c) (let ((ret (complex-typed-copy! A (complex-typed-zeros (dimensions A)))))
			      (real-typed-num-scal! -1d0 (tensor-imagpart~ ret))
			      ret))))
	      (B (ecase job-B
		   ((:n :t) B)
		   ((:h :c) (let ((ret (complex-typed-copy! A (complex-typed-zeros (dimensions A)))))
			      (real-typed-num-scal! -1d0 (tensor-imagpart~ ret))
			      ret))))
	      (tjob (combine-jobs (ecase job-A ((:n :t) job-A) (:h :t) (:c :n))
				  (ecase job-B ((:n :t) job-B) (:h :t) (:c :n)))))
	  (complex-base-typed-gemm! alpha A B
				    beta C tjob)))))

;;Symbolic
#+maxima
(generate-typed-gemm! symbolic-base-typed-gemm!
    (symbolic-tensor nil 0))

;;---------------------------------------------------------------;;

(defgeneric gemm! (alpha A B beta C &optional job)
  (:documentation
   "
  Syntax
  ======
  (GEMM! alpha a b beta c [job])

  Purpose
  =======
  Performs the GEneral Matrix Multiplication given by
	       --      -      -

	    C <- alpha * op(A) * op(B) + beta * C

  and returns C.

  alpha,beta are scalars and A,B,C are matrices.
  op(A) means either A or A'.

  JOB must be a keyword with two of these alphabets
     N                 Identity
     T                 Transpose
     C                 Complex conjugate
     H                 Hermitian transpose {conjugate transpose}

  so that (there are 4x4 operations in total).

     JOB                    Operation
  ---------------------------------------------------
     :NN (default)      alpha * A * B + beta * C
     :TN                alpha * transpose(A) * B + beta * C
     :NH                alpha * A * transpose o conjugate(B) + beta * C
     :HC                alpha * transpose o conjugate(A) * conjugate(B) + beta * C
")
  (:method :before ((alpha number) (A standard-matrix) (B standard-matrix)
		    (beta number) (C standard-matrix)
		    &optional (job :nn))
    (let ((nr-a (nrows A))
	  (nc-a (ncols A))
	  (nr-b (nrows B))
	  (nc-b (ncols B))
	  (nr-c (nrows C))
	  (nc-c (ncols C)))
      (declare (type index-type nr-a nc-a nr-b nc-b nr-c nc-c))
      (let ((sjobs (multiple-value-list (split-job job))))
	(assert (= (length sjobs) 2) nil 'invalid-arguments :message "Ill formed job")
	(ecase (first sjobs) ((:n :c) t) ((:t :h) (rotatef nr-a nc-a)))
	(ecase (second sjobs) ((:n :c) t) ((:t :h) (rotatef nr-b nc-b))))
      (assert (not (or (eq A C) (eq B C))) nil 'invalid-arguments
	      :message "GEMM!: C = {A or B} is not allowed.")
      (assert (and (= nr-c nr-a)
		   (= nc-a nr-b)
		   (= nc-b nc-c)) nil 'tensor-dimension-mismatch))))

(defmethod gemm! ((alpha number) (a real-matrix) (b real-matrix)
		  (beta number) (c real-matrix)
		  &optional (job :nn))
  (real-typed-gemm! (coerce-real alpha) a b
		    (coerce-real beta) c job))

(defmethod gemm! ((alpha number) (a complex-matrix) (b complex-matrix)
		  (beta number) (c complex-matrix)
		  &optional (job :nn))
  (complex-typed-gemm! (coerce-complex alpha) a b
		       (coerce-complex beta) c job))

(defmethod gemm! ((alpha number) (a real-matrix) (b real-matrix)
		  (beta number) (c complex-matrix)
		  &optional (job :nn))
  (unless (= beta 1)
    (scal! beta c))
  (unless (= alpha 0)
    (if (complexp alpha)
	(let ((A.x (make-real-tensor (nrows c) (ncols c)))
	      (vw.c (tensor-realpart~ c)))
	  (real-typed-gemm! (coerce-real 1) A B (coerce-real 0) A.x job)
	  ;;Re
	  (axpy! (realpart alpha) A.x vw.c)
	  ;;Im
	  (incf (head vw.c))
	  (axpy! (imagpart alpha) A.x vw.c))
	(let ((vw.c (tensor-realpart~ c)))
	  (real-typed-gemm! (coerce-real alpha) A B
			    (coerce-real 1) vw.c job))))
  C)

(defmethod gemm! ((alpha number) (a real-matrix) (b complex-matrix)
		  (beta number) (c complex-matrix)
		  &optional (job :nn))
  (let ((A.cplx (copy! A (make-complex-tensor (nrows a) (ncols a)))))
    (complex-typed-gemm! (coerce-complex alpha) A.cplx B
			 (coerce-complex beta) C job))
  C)

(defmethod gemm! ((alpha number) (a complex-matrix) (b real-matrix)
		  (beta number) (c complex-matrix)
		  &optional (job :nn))
  (let ((B.cplx (copy! B (make-complex-tensor (nrows B) (ncols B)))))
    (complex-typed-gemm! (coerce-complex alpha) A B.cplx
			 (coerce-complex beta) C job))
  C)

;;---------------------------------------------------------------;;
(defgeneric gemm (alpha a b beta c &optional job)
  (:documentation
   "
  Syntax
  ======
  (GEMM alpha a b beta c [job])

  Purpose
  =======
  Performs the GEneral Matrix Multiplication given by
	       --      -      -

	     alpha * op(A) * op(B) + beta * C

  and returns the result in a new matrix.

  alpha,beta are scalars and A,B,C are matrices.
  op(A) means either A or A'.

  JOB must be a keyword with two of these alphabets
     N                 Identity
     T                 Transpose
     C                 Complex conjugate
     H                 Hermitian transpose {conjugate transpose}

  so that (there are 4x4 operations in total).

     JOB                    Operation
  ---------------------------------------------------
     :NN (default)      alpha * A * B + beta * C
     :TN                alpha * transpose(A) * B + beta * C
     :NH                alpha * A * transpose o conjugate(B) + beta * C
     :HC                alpha * transpose o conjugate(A) * conjugate(B) + beta * C
"))

(defmethod gemm ((alpha number) (a standard-matrix) (b standard-matrix)
		 (beta number) (c complex-matrix)
		 &optional (job :nn))
  (let ((result (copy C)))
    (gemm! alpha A B beta result job)))

;; if all args are not real then at least one of them
;; is complex, so we need to call GEMM! with a complex C
(defmethod gemm ((alpha number) (a standard-matrix) (b standard-matrix)
		 (beta number) (c real-matrix)
		 &optional (job :nn))
  (let ((result (funcall (if (or (complexp alpha) (complexp beta)
				 (typep a 'complex-matrix) (typep b 'complex-matrix))
			     #'complex-typed-zeros
			     #'real-typed-zeros)
			 (dimensions C))))
    (copy! C result)
    (gemm! alpha A B beta result job)))

(defmethod gemm ((alpha number) (a standard-matrix) (b standard-matrix)
		 (beta (eql nil)) (c (eql nil))
		 &optional (job :nn))
  (multiple-value-bind (job-A job-B) (split-job job)
    (let ((result (funcall (if (or (complexp alpha) (complexp beta)
				   (typep a 'complex-matrix) (typep b 'complex-matrix))
			       #'complex-typed-zeros
			       #'real-typed-zeros)
			   (make-index-store (list (if (member job-A '(:n :c)) (nrows A) (ncols A))
						   (if (member job-B '(:n :c)) (ncols B) (nrows B)))))))
      (gemm! alpha A B 0 result job))))
