(in-package #:matlisp)

(definline tb+ (a &optional b)
  "
  Syntax
  ======
  (t+ a b)

  Purpose
  =======
  Create a new matrix which is the sum of A and B.
  A or B (but not both) may be a scalar, in which
  case the addition is element-wise.
  "
  (if b
      (cart-etypecase (a b)
	((number number) (cl:+ a b))
	((number base-tensor) (axpy a nil b))
	((base-tensor number) (axpy b nil a))
	((base-tensor base-tensor) (axpy 1 a b)))
      a))

(definline t+ (&rest objs)
  (reduce #'tb+ objs))
(definline m+ (&rest objs)
  (apply #'t+ objs))
(definline m.+(&rest objs)
  (apply #'t+ objs))
;;
(definline tb- (a &optional b)
  "
  Syntax
  ======
  (t- a b)

  Purpose
  =======
  Create a new matrix which is the sum of A and B.
  A or B (but not both) may be a scalar, in which
  case the addition is element-wise.
"
  (if b
      (cart-etypecase (a b)
	((number number) (cl:- a b))
	((number base-tensor) (axpy! a nil (scal -1 b)))
	((base-tensor number) (axpy (cl:- b) nil a))
	((base-tensor base-tensor) (axpy -1 b a)))
      (etypecase a
	(number (cl:- a))
	(base-tensor (scal -1 a)))))

(definline t- (&rest objs)
  (if (cdr objs)
      (reduce #'tb- objs)
      (tb- (car objs))))
(definline m- (&rest objs)
  (apply #'t- objs))
(definline m.- (&rest objs)
  (apply #'t- objs))
;;
(definline tb* (a &optional b)
  (if b
      (cart-etypecase (a b)
	((number number) (cl:* a b))
	;;Scaling
	((number base-tensor) (scal a b))
	((base-tensor number) (scal b a))
	;;Matrix, vector/matrix product
	((base-matrix base-matrix) (gemm 1 a b nil nil))
	((base-matrix base-vector) (gemv 1 a b nil nil :n))
	((base-vector base-matrix) (gemv 1 b a nil nil :t))
	;;Permutation action. Left action permutes axis-0, right action permutes the last axis (-1).
	((permutation base-tensor) (permute b a 0))
	((base-tensor permutation) (permute a b -1))
	;;The correctness of this depends on the left-right order in reduce (foldl).
	((permutation permutation) (compose a b)))
      a))

(defmacro tb*-opt (a b)
  (labels ((op (code)
	     (when (consp code)
	       (case (car code)
		 (htranspose #\C)
		 (transpose #\T)))))
    (if (or (op a) (op b))
	(with-gensyms (ma mb)
	  `(let ((,ma ,(if (op a) (second a) a))
		 (,mb ,(if (op b) (second b) b)))
	     (cart-etypecase (,ma ,mb)
	       ((base-matrix base-matrix)
		(gemm 1 ,ma ,mb nil nil ,(intern (coerce (list (or (op a) #\N) (or (op b) #\N)) 'string) :keyword)))
	       ((base-matrix base-vector) ;;The other case involves a complex conjugate.
		(gemv 1 ,ma ,mb nil nil ,(intern (coerce (list (or (op a) #\N)) 'string) :keyword)))
	       ((t t)
		(tb* ,(if (op a) `(,(car a) ,ma) ma) ,(if (op b) `(,(car b) ,mb) mb))))))
	`(tb* ,a ,b))))

(definline t* (&rest objs)
  (reduce #'tb* objs))
(definline m* (&rest objs)
  (apply #'t* objs))
;;
(definline tb.* (a &optional b)
  (if b
      (cart-etypecase (a b)
	((number number) (cl:* a b))
	;;Scaling
	(((or number base-tensor) (or number base-tensor)) (scal a b)))
      a))

(definline t.* (&rest objs)
  (reduce #'tb.* objs))
(definline m.* (&rest objs)
  (apply #'t.* objs))
;;
(definline tb./ (a &optional b)
  (if b
      (cart-etypecase (a b)
	((number number) (cl:/ a b))
	;;Scaling
	(((or number base-tensor) (or number base-tensor)) (div b a)))
      (etypecase a
	(number (cl:/ a))
	(base-tensor (div a 1)))))

(definline t./ (&rest objs)
  (if (cdr objs)
      (reduce #'tb./ objs)
      (tb./ (car objs))))

(definline m./ (&rest objs)
  (apply #'t./ objs))
;;
(defparameter *tensor-contraction-functable* (make-hash-table :test 'equal))
(defgeneric gett! (alpha a b beta c)
  (:method :before (alpha (a base-tensor) (b base-tensor) beta (c base-tensor))
	   (assert (and (= (dimensions a -1) (dimensions b 0))
			(=  (+ (order a) (order b) -2) (order c))
			(dotimes (i (1- (order a)) t) (unless (= (dimensions a i) (dimensions c i)) (return nil)))
			(dotimes (i (1- (order b)) t) (unless (= (dimensions b (1+ i)) (dimensions c (+ (order a) i -1))) (return nil))))
		   nil 'tensor-dimension-mismatch)))

(define-tensor-method gett! (alpha (a standard-tensor :input) (b standard-tensor :input) beta (c standard-tensor :output))
  `(let ((func (or (gethash (list (order a) (order b) ',(cl a)) *tensor-contraction-functable*)
		   (let ((asyms (iter (for i from 0 below (1- (order a))) (collect (gensym (format nil "a_~a" i)))))
			 (bsyms (iter (for i from 1 below (order b)) (collect (gensym (format nil "b_~a" i)))))
			 (sumsym (gensym "idx")))
		     (format t "Generating contraction for orders : (~a, ~a)." (order a) (order b))
		     (setf (gethash (list (order a) (order b) ',(cl a)) *tensor-contraction-functable*)
			   (compile-and-eval `(lambda-einstein (alpha a b c) (,',(cl a)  (ref c ,@asyms ,@bsyms) (* alpha (ref a ,@asyms ,sumsym) (ref b ,sumsym ,@bsyms)) nil)))))))
	 (alpha (t/coerce ,(field-type (cl a)) alpha))
	 (beta (t/coerce ,(field-type (cl a)) beta)))
     (unless (t/f= ,(field-type (cl a)) beta (t/fid* ,(field-type (cl a)))) (scal! beta c))
     (funcall func alpha a b c)
     c))
;;
(definline tb@ (a b)
  (cart-etypecase (a b)
    ((number number) (cl:* a b))
    ;;Scaling
    ((number base-tensor) (scal a b))
    ((base-tensor number) (scal b a))
    ;;Matrix, vector/matrix product
    ((base-vector base-vector) (dot a b nil))
    ((base-matrix base-matrix) (gemm 1 a b nil nil))
    ((base-matrix base-vector) (gemv 1 a b nil nil :n))
    ((base-vector base-matrix) (gemv 1 b a nil nil :t))
    ((base-tensor base-tensor) (gett! 1 a b 1 (zeros (append (butlast (dims a)) (cdr (dims b))) (class-of a))))
    ;;Permutation action on arguments. Left action unpermutes arguments, right action permutes them.
    ;;See tb* for comparison.
    ((permutation base-tensor) (transpose b (inv a)))
    ((base-tensor permutation) (transpose b a))
    ;;The correctness of this depends on the left-right order in reduce (foldl).
    ((permutation permutation) (compose a b))))

(definline t@ (&rest objs)
  (reduce #'tb@ objs))
;;
(definline tb/ (b a)
  "Solve x a = b"
  (cart-etypecase (b a)
    ((number number) (cl:/ b a))
    ((base-tensor number) (scal (cl:/ a) b))
    (((eql nil) (and base-square-matrix blas-numeric-tensor))
     (inv a))
    (((and base-matrix blas-numeric-tensor) (and base-square-matrix blas-numeric-tensor))
     (transpose (with-colm (getrs! (getrf! (copy a)) (transpose b) :t))))
    (((and base-vector blas-numeric-tensor) (and base-square-matrix blas-numeric-tensor))
     (let ((ret (copy b)))
       (with-colm (getrs! (getrf! (copy a)) (suptensor~ ret 2) :t))
       ret))
    ((standard-tensor permutation)
     (permute b (inv a) -1))
    ;;The correctness of this depends on the left-right order in reduce (foldl).
    ((permutation permutation)
     (compose b (inv a)))))

(definline tb\\ (b a)
  "Solve a x = b"
  (cart-etypecase (b a)
    ((number number) (cl:/ b a))
    ((base-tensor number) (scal (cl:/ a) b))
    (((eql nil) (and base-square-matrix blas-numeric-tensor))
     (inv a))
    (((and base-matrix blas-numeric-tensor) (and base-square-matrix blas-numeric-tensor))
     (getrs! (getrf! (with-colm (copy a))) (copy b)))
    (((and base-vector blas-numeric-tensor) (and base-square-matrix blas-numeric-tensor))
     (let ((ret (copy b)))
       (getrs! (getrf! (with-colm (copy a))) (suptensor~ ret 2))
       ret))
    ((standard-tensor permutation)
     (permute b (inv a) 0))
    ;;The correctness of this depends on the left-right order in reduce (foldl).
    ((permutation permutation)
     (compose (inv a) b))))
;;
(defgeneric tb^ (a b)
  (:documentation "Returns the tensor outer product of a and b."))

(defmethod tb^ ((a base-tensor) b) ;;col-vector
  (orphanize (suptensor~ (scal b a) (1+ (order a)))))
(defmethod tb^ (a (b base-tensor)) ;;row-vector
  (orphanize (suptensor~ (scal a b) (1+ (order b)) 1)))
(define-tensor-method tb^ ((a standard-tensor :input) (b standard-tensor :input))
  `(cart-etypecase (a b)
     ((base-vector base-vector)
      (ger 1 a b nil nil))
     ((standard-tensor standard-tensor)
      (let* ((ret (zeros (append (dims a) (dims b)) ',(cl a)))
	     (ret-a (subtensor~ ret (loop :for i :from 0 :below (order ret)
				       :collect (if (< i (order a)) '(nil nil) 0))))
	     (rbstr (subseq (strides ret) (order a)))
	     (sto-b (store b)))
	(mod-dotimes (idx (dimensions b))
	  :with (linear-sums
		 (of-b (strides b) (head b))
		 (of-r rbstr (head ret)))
	  :do (progn
		(setf (slot-value ret-a 'head) of-r)
		(axpy! (t/store-ref ,(cl b) sto-b of-b) a ret-a)))
	ret))))

(definline t^ (&rest objs)
  (reduce #'tb^ objs))
;;
(defgeneric ge== (a b)
  (:method ((a base-tensor) (b base-tensor))
    (assert (lvec-eq (dimensions a) (dimensions b)) nil 'tensor-dimension-mismatch)))
(define-tensor-method ge== (a (b standard-tensor :input))
  `(let ((a (t/coerce ,(field-type (cl b)) a))
	 (ret (zeros (dimensions b) 'boolean-tensor)))
     (dorefs (idx (dimensions b))
	     ((ref.b b :type ,(cl b))
	      (ref.r ret :type boolean-tensor))
	     (when (t/f= ,(field-type (cl b)) a ref.b) (setf ref.r 1)))
     ret))
(define-tensor-method ge== ((a standard-tensor :input) (b standard-tensor :input))
  `(let ((ret (zeros (dimensions a) 'boolean-tensor)))
     (dorefs (idx (dimensions a))
	     ((ref.a a :type ,(cl a))
	      (ref.b b :type ,(cl b))
	      (ref.r ret :type boolean-tensor))
	     (when (t/f= ,(field-type (cl a)) ref.a ref.b) (setf ref.r 1)))
     ret))

(definline tb== (a &optional b)
  (if b
      (cart-etypecase (a b)
	((number number) (if (cl:= a b) 1 0))
	(((or number base-tensor) (or number base-tensor)) (etypecase b (base-tensor (ge== a b)) (number (ge== b a)))))
	 1))
