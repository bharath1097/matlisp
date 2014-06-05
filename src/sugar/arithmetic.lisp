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
	((base-vector base-matrix) (gemv 1 a b nil nil :t))
	;;Permutation action. Left action permutes axis-0, right action permutes the last axis (-1).
	((permutation base-tensor) (permute b a 0))
	((base-tensor permutation) (permute a b -1))
	;;The correctness of this depends on the left-right order in reduce (foldl).
	((permutation permutation) (compose a b)))
      a))

;; Is this really necessary ?
;; (defun t*ord (&rest mats)
;;   (let* ((n (length mats))
;; 	 (cost (make-array (list n n) :element-type 'fixnum :initial-element 0)))
;;     (iter (for mat in mats)
;; 	  (setf (aref cost i i) (cond ((tensor-matrixp mat) (aref (dimensions mat) 1)) ((tensor-vectorp mat) 1))
;;     (iter (for l from 2 to n)
;; 	  (iter (for i from 0 to (- n l))
;; 		(setf (aref cost i (+ i l -1))
;; 		      (iter (for k from i to (+ i l -2)
;; 				 (minimizing (+ (cost i k) (cost (1+ k) (+ i l -1)) (the fixnum (* (aref seq i) (the fixnum (* (aref seq (1+ k)) (aref seq (+ i l)))))))))))
		
;;        :finally (return (cost 0 (1- n)))))))

(defmacro t* (&rest objs)
  (labels ((op (code)
	     (when (consp code)
	       (case (car code)
		 (htranspose #\C)
		 (transpose #\T))))
	   (optimizer (a b)
	     (let ((op.a (op a))
		   (op.b (op b)))
	       (if (not (or op.a op.b))
		   `(tb* ,a ,b)
		   (with-gensyms (ma mb)
		     `(let ((,ma ,(if op.a (cadr a) a))
			    (,mb ,(if op.b (cadr b) b)))
			;;This will not throw errors that one would expect, sometimes.
			(if (and (tensor-matrixp ,ma) (tensor-matrixp ,mb))
			    (gemm 1 ,ma ,mb nil nil ,(intern (coerce (list (or op.a #\N) (or op.b #\N)) 'string) :keyword))
			    (tb* ,(if op.a `(,(car a) ,ma) ma) ,(if op.b `(,(car b) ,mb) mb))))))))
	   (ropt (lst)
	     (if (not (cdr lst)) (car lst)
		 (ropt (cons (optimizer (first lst) (second lst)) (cddr lst))))))
    (ropt objs)))

(defmacro m* (&rest objs)
  `(t* ,@objs))
;; (definline t* (&rest objs)
;;   (reduce #'tb* objs))
;; (definline m* (&rest objs)
;;   (apply #'t* objs))
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
(defgeneric tensor-contraction (a b))
(defparameter *tensor-contraction-functable* (make-hash-table :test 'equal))
(define-tensor-method tensor-contraction ((a standard-tensor :input) (b standard-tensor :input))
  `(let ((func (or (gethash (list (order a) (order b) ',(cl a)) *tensor-contraction-functable*)
		   (let ((asyms (iter (for i from 0 below (1- (order a))) (collect (gensym (format nil "a_~a" i)))))
			 (bsyms (iter (for i from 1 below (order b)) (collect (gensym (format nil "b_~a" i)))))
			 (sumsym (gensym "idx")))
		     (format t "Generating contraction for orders : (~a, ~a)." (order a) (order b))
		     (setf (gethash (list (order a) (order b) ',(cl a)) *tensor-contraction-functable*)
			   (compile-and-eval
			    `(lambda (a b c)
			       (einstein-sum ,',(cl a) (,@(reverse bsyms) ,sumsym ,@(reverse asyms)) (ref c ,@asyms ,@bsyms) (* (ref a ,@asyms ,sumsym) (ref b ,sumsym ,@bsyms))))))))))
     (funcall func a b (zeros (append (butlast (dims a)) (cdr (dims b))) ',(cl a)))))

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
    ((base-vector base-matrix) (gemv 1 a b nil nil :t))
    ((base-tensor base-tensor) (tensor-contraction a b))
    ;;Permutation action. Left action permutes axis-0, right action permutes the last axis (-1).
    ((permutation base-tensor) (transpose b (inv a)))
    ((base-tensor permutation) (transpose b a))
    ;;The correctness of this depends on the left-right order in reduce (foldl).
    ((permutation permutation) (compose a b))))

(definline t@ (&rest objs)
  (reduce #'tb@ objs))
;;
#+nil
(definline t/ (b &optional a)
  "Solve x a = b"
  (if a
      (cart-etypecase (b a)
	((number number) (cl:/ b a))
	((base-tensor number) (scal (cl:/ a) b))
	(((and base-square-matrix blas-numeric-tensor) (and base-matrix blas-numeric-tensor))
	 (transpose (with-colm (getrs! (getrf! (copy a)) (transpose b) :t))))
	(((and base-square-matrix blas-numeric-tensor) (and base-vector blas-numeric-tensor))
	 (let ((ret (copy b)))
	   (with-colm (getrs! (getrf! (copy a)) (suptensor~ ret 2) :t))
	   ret))
	
	)))

(defgeneric t/ (b a) ;;rdiv
  (:documentation "Solve x a = b")
  (:method ((b number) (a number))
    (cl:/ b a))
  ;;Scaling
  (:method ((b standard-tensor) (a number))
    (scal (cl:/ a) b))
  ;;Matrix, vector/matrix product
  (:method ((a standard-tensor) (b (eql nil)))
    (cond
      ((and (tensor-matrixp a) (tensor-squarep a))
       (inv a))
      (t (error "Don't know how to solve the given equation."))))
  (:method ((b standard-tensor) (a standard-tensor))
    (cond
      ((and (tensor-matrixp a) (tensor-squarep a) (tensor-matrixp b))
       (transpose (getrs! (getrf! (copy a)) (transpose b) :t)))
      ((and (tensor-matrixp a) (tensor-squarep a) (tensor-vectorp b))
       (let ((tmp (zeros (list (aref (dimensions b) 0) 1) (class-of b))))
	 (copy! b (slice~ tmp 1))
	 (getrs! (getrf! (copy a)) tmp :t)
	 (let ((ret (slice~ tmp 1)))
	   (setf (slot-value ret 'parent-tensor) nil)
	   ret)))
      (t (error "Don't know how to solve the given equation."))))
  ;;Permutation action. Left action permutes axis-0, right action permutes axis-1.
  (:method ((b standard-tensor) (a permutation))
    (permute b (inv a) 1))
  ;;The correctness of this depends on the left-right order in reduce (foldl).
  (:method ((a permutation) (b permutation))
    (compose b (inv a))))

(defgeneric t\\ (b a) ;ldiv
  (:documentation "Solve a x = b")
  (:method ((b t) (a t))
    (t/ b a))
  (:method ((b standard-tensor) (a standard-tensor))
    (cond
      ((and (tensor-matrixp a) (tensor-squarep a) (tensor-matrixp b))
       (getrs! (getrf! (copy a)) (copy b)))
      ((and (tensor-matrixp a) (tensor-squarep a) (tensor-vectorp b))
       (let ((tmp (zeros (list (aref (dimensions b) 0) 1) (class-of b))))
	 (copy! b (slice~ tmp 1))
	 (getrs! (getrf! (copy a)) tmp)
	 (let ((ret (slice~ tmp 1)))
	   (setf (slot-value ret 'parent-tensor) nil)
	   ret)))
      (t (error "Don't know how to solve the given equation."))))
  ;;Permutation action. Left action permutes axis-0, right action permutes axis-1.
  (:method ((b standard-tensor) (a permutation))
    (permute b (inv a) 0))
  ;;The correctness of this depends on the left-right order in reduce (foldl).
  (:method ((a permutation) (b permutation))
    (compose (inv a) b)))
;;
(defgeneric tb^ (a b))

(define-tensor-method tb^ ((a standard-tensor :input) (b standard-tensor :input)) 
  `(if (and (tensor-vectorp a) (tensor-vectorp b))
       (ger 1 a b nil nil)
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
	 ret)))

(definline t^ (&rest objs)
  (reduce #'tb^ objs))
