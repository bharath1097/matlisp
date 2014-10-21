(in-package #:matlisp)

;;
(deft/generic (t/blas-ger! #'subtypep) sym (alpha x st-x y st-y A lda &optional conjp))
(deft/method t/blas-ger! (sym blas-numeric-tensor) (alpha x st-x y st-y A lda &optional (conjp t))
  (let ((ftype (field-type sym)))
    (using-gensyms (decl (alpha x st-x y st-y A lda) (m n))
      `(let* (,@decl
	      (,m (dimensions ,A 0)) (,n (dimensions ,A 1)))
	 (declare (type ,sym ,A ,x ,y)
		  (type ,(field-type sym) ,alpha)
		  (type index-type ,st-x ,st-y ,lda ,m ,n))
	 (ffuncall ,(blas-func (string+ "ger" (when (subtypep ftype 'complex) (if conjp "c" "u"))) ftype)
		   (:& :integer) ,m (:& :integer) ,n
		   (:& ,(lisp->ffc ftype t)) ,alpha
		   (:* ,(lisp->ffc ftype) :+ (head ,x)) (the ,(store-type sym) (store ,x)) (:& :integer) (the index-type ,st-x)
		   (:* ,(lisp->ffc ftype) :+ (head ,y)) (the ,(store-type sym) (store ,y)) (:& :integer) (the index-type ,st-y)
		   (:* ,(lisp->ffc ftype) :+ (head ,A)) (the ,(store-type sym) (store ,A)) (:& :integer) ,lda)
	 ,A))))

;;
(deft/generic (t/ger! #'subtypep) sym (alpha x y A &optional conjp))
(deft/method t/ger! (sym standard-tensor) (alpha x y A &optional (conjp t))
  (using-gensyms (decl (alpha A x y))
   `(let (,@decl)
      (declare (type ,sym ,A ,x ,y)
	       (type ,(field-type sym) ,alpha))
      ;;These loops are optimized for column major matrices
      (unless (t/f= ,(field-type sym) ,alpha (t/fid+ ,(field-type sym)))
	(einstein-sum ,sym (j i) (ref ,A i j) (* ,alpha (ref ,x i)
						 ,(recursive-append
						   (when conjp `(t/fc ,(field-type sym)))
						   `(ref ,y j)))
		      nil))
      ,A)))
;;---------------------------------------------------------------;;
(defgeneric ger! (alpha x y A &optional conjugate-p)
  (:documentation
   "
  Syntax
  ======
  (GER! alpha x y A [job])

  Purpose
  =======
  Performs the GEneral matrix Rank-1 update given by
	       --             -

	    A <- alpha * x * op(y) + A

  and returns A.

  alpha is a scalars,
  x,y are vectors.
  A is a matrix.

  If conjugate-p is nil, then op(y) = y^T, else op(y) = y^H.
")
  (:method :before (alpha (x standard-tensor) (y standard-tensor) (A standard-tensor) &optional conjugate-p)
    (declare (ignore conjugate-p))
    (assert (and
	     (tensor-vectorp x) (tensor-vectorp y) (tensor-matrixp A)
	     (= (dimensions x 0) (dimensions A 0))
	     (= (dimensions y 0) (dimensions A 1)))
	    nil 'tensor-dimension-mismatch)))

(define-tensor-method ger! (alpha (x standard-tensor :input) (y standard-tensor :input) (A standard-tensor :output) &optional (conjugate-p t))
  `(let ((alpha (t/coerce ,(field-type (cl x)) alpha)))
     (declare (type ,(field-type (cl x)) alpha))
     ,(recursive-append
       (when (subtypep (cl x) 'blas-numeric-tensor)
	 `(if (call-fortran? A (t/l2-lb ,(cl a)))
	      (with-columnification (() (A))		  
		(if conjugate-p 
		    (t/blas-ger! ,(cl a)
				 alpha
				 x (strides x 0)
				 y (strides y 0)
				 A (or (blas-matrix-compatiblep A #\N) 0)
				 t)
		    (t/blas-ger! ,(cl a)
				 alpha
				 x (strides x 0)
				 y (strides y 0)
				 A (or (blas-matrix-compatiblep A #\N) 0)
				 nil)))))
       `(if conjugate-p
	    (t/ger! ,(cl a) alpha x y A t)
	    (t/ger! ,(cl a) alpha x y A nil))))
  'A)
;;---------------------------------------------------------------;;
(defgeneric ger (alpha x y A &optional conjugate-p)
  (:documentation
   "
  Syntax
  ======
  (GER alpha x y A [job])

  Purpose
  =======
  Performs the GEneral matrix Rank-1 update given by
	       --             -

	     alpha * x * op(y) + A

  and returns A.

  alpha is a scalars,
  x,y are vectors.
  A is a matrix.

  If conjugate-p is nil, then op(y) = y^T, else op(y) = y^H.
"))

(defmethod ger (alpha (x standard-tensor) (y standard-tensor)
		(A standard-tensor) &optional conjugate-p)
  (ger! alpha x y (copy A) conjugate-p))

(defmethod ger (alpha (x standard-tensor) (y standard-tensor)
		(A (eql nil)) &optional conjugate-p)
  (ger! alpha x y (zeros (append (dims x) (dims y))) conjugate-p))
