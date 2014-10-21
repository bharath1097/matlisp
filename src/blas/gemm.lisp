(in-package #:matlisp)

(deft/generic (t/blas-gemm-func #'subfieldp) sym ())
(deft/method t/blas-gemm-func (sym real-tensor) ()
  'dgemm)
(deft/method t/blas-gemm-func (sym complex-tensor) ()
  'zgemm)
;;
(deft/generic (t/blas-gemm! #'subtypep) sym (alpha A lda B ldb beta C ldc transa opa opb))

(deft/method t/blas-gemm! (sym blas-numeric-tensor) (alpha A lda B ldb beta C ldc transa opa opb)
  (let ((ftype (field-type sym)))
    (using-gensyms (decl (alpha A lda B ldb beta C ldc transa opa opb) (m n k))   
      `(let* (,@decl
	      (,m (dimensions ,C 0)) (,n (dimensions ,C 1))
	      (,k (dimensions ,A (ecase ,transa (#\N 1) ((#\T #\C) 0)))))
	 (declare (type ,sym ,A ,B ,C)
		  (type ,(field-type sym) ,alpha ,beta)
		  (type index-type ,lda ,ldb ,ldc ,m ,n ,k)
		  (type character ,transa ,opa ,opb))
	 (ffuncall ,(blas-func "gemm" ftype)
		   (:& :character) ,opa (:& :character) ,opb
		   (:& :integer) ,m (:& :integer) ,n (:& :integer) ,k
		   (:& ,(lisp->ffc ftype t)) ,alpha
		   (:* ,(lisp->ffc ftype) :+ (head ,A)) (the ,(store-type sym) (store ,A)) (:& :integer) ,lda
		   (:* ,(lisp->ffc ftype) :+ (head ,B)) (the ,(store-type sym) (store ,B)) (:& :integer) ,ldb
		   (:& ,(lisp->ffc ftype t)) ,beta
		   (:* ,(lisp->ffc ftype) :+ (head ,C)) (the ,(store-type sym) (store ,C)) (:& :integer) ,ldc)
	 ,C))))

;;
(deft/generic (t/gemm! #'subtypep) sym (alpha A B beta C transa transb))
(deft/method t/gemm! (sym standard-tensor) (alpha A B beta C transa transb)
  (using-gensyms (decl (alpha A B beta C transa transb))
   `(let (,@decl)
      (declare (type ,sym ,A ,B ,C)
	       (type ,(field-type sym) ,alpha ,beta)
	       (type character ,transa ,transb))
      (unless (t/f= ,(field-type sym) ,beta (t/fid* ,(field-type sym)))
	(t/scdi! ,sym ,beta ,C :scal? t :numx? t))
      ,@(when (field-realp (field-type sym))
	      `((when (char= ,transa #\C) (setq ,transa #\T))
		(when (char= ,transb #\C) (setq ,transb #\T))))
      ;;These loops are optimized for column major matrices
      ,(labels ((transpose-ref (mat)
		  `(ref ,(cadr mat) ,@(reverse (cddr mat))))
		(conjugate-ref (mat)
		  `(t/fc ,(field-type sym) ,mat))
		(generate-mm-code (transa transb)
		  (destructuring-bind (A-ref B-ref) (mapcar #'(lambda (mat trans) (ecase trans
										    ((#\N #\T) mat)
										    ((#\C) (conjugate-ref mat))))
							    (mapcar #'(lambda (mat trans) (ecase trans
											    ((#\N) mat)
											    ((#\T #\C) (transpose-ref mat))))
								    (list `(ref ,A i k) `(ref ,B k j)) (list transa transb))
							    (list transa transb))
		    (let ((loopo (let ((ta (member transa '(#\T #\C)))
				       (tb (member transb '(#\T #\C))))
				      (cond
					((and (not ta) (not tb)) `(j k i))
					((and (not ta) tb) `(k j i))
					(t`(i j k))))))
		      `(einstein-sum ,sym ,loopo (ref ,C i j) (* ,alpha ,A-ref ,B-ref) nil)))))
	       `(ecase ,transa
		  ,@(loop :for ta :across (if (field-realp (field-type sym)) "NT" "NTC")
		       :collect `(,ta (ecase ,transb
					,@(loop :for tb :across (if (field-realp (field-type sym)) "NT" "NTC")
					     :collect `(,tb ,(generate-mm-code ta tb))))))))
      ,C)))
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
     C                 Hermitian transpose {conjugate transpose}
")
  (:method :before (alpha (A standard-tensor) (B standard-tensor)
		    beta (C standard-tensor)
		    &optional (job :nn))
    (let ((nr-a (nrows A))
	  (nc-a (ncols A))
	  (nr-b (nrows B))
	  (nc-b (ncols B))
	  (nr-c (nrows C))
	  (nc-c (ncols C)))
      (declare (type index-type nr-a nc-a nr-b nc-b nr-c nc-c))
      (let ((sjobs (split-job job)))
	(assert (= (length sjobs) 2) nil 'invalid-arguments :message "Ill formed job")
	(ecase (first sjobs) (#\N t) ((#\T #\C) (rotatef nr-a nc-a)))
	(ecase (second sjobs) ((#\N) t) ((#\T #\C) (rotatef nr-b nc-b))))
      (assert (not (or (eq A C) (eq B C))) nil 'invalid-arguments
	      :message "GEMM!: C = {A or B} is not allowed.")
      (assert (and (tensor-matrixp A) (tensor-matrixp B) (tensor-matrixp C)
		   (= nr-c nr-a)
		   (= nc-a nr-b)
		   (= nc-b nc-c)) nil 'tensor-dimension-mismatch))))

(define-tensor-method gemm! (alpha (A standard-tensor :input) (B standard-tensor :input) beta (C standard-tensor :output) &optional (job :nn))
  `(let ((alpha (t/coerce ,(field-type (cl a)) alpha))
	 (beta (t/coerce ,(field-type (cl a)) beta)))
     (declare (type ,(field-type (cl a)) alpha beta))
     (destructuring-bind (joba jobb) (split-job job)
       (declare (type character joba jobb))
       ,(recursive-append
	 (when (subtypep (cl c) 'blas-numeric-tensor)
	   `(if (call-fortran? C (t/l3-lb ,(cl c)))
		(with-columnification (((a joba) (b jobb)) (c))
		  (multiple-value-bind (lda opa) (blas-matrix-compatiblep a joba)
		    (multiple-value-bind (ldb opb) (blas-matrix-compatiblep b jobb)
		      (t/blas-gemm! ,(cl a) alpha A lda B ldb beta C (or (blas-matrix-compatiblep c #\N) 0) joba opa opb))))))
	 `(t/gemm! ,(cl a) alpha A B beta C joba jobb))))
  'C)
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
     C                 Hermitian conjugate
"))

(defmethod gemm (alpha (A standard-tensor) (B standard-tensor)
		 beta (C standard-tensor) &optional (job :nn))
  (gemm! alpha A B beta (copy C) job))

(defmethod gemm (alpha (A standard-tensor) (B standard-tensor)
		 (beta (eql nil)) (C (eql nil)) &optional (job :nn))
  (let ((ret (destructuring-bind (job-a job-b) (split-job job)
	       (zeros (list (ecase job-a (#\N (nrows A)) ((#\C #\T) (ncols A)))
			    (ecase job-b (#\N (ncols B)) ((#\C #\T) (nrows B))))
		      (class-of A)))))
    (gemm! alpha A B 1 ret job)))
