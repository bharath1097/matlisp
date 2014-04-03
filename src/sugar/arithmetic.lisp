(in-package #:matlisp)

(defgeneric tb+ (a b)
  (:documentation
   "
  Syntax
  ======
  (t+ a b)

  Purpose
  =======
  Create a new matrix which is the sum of A and B.
  A or B (but not both) may be a scalar, in which
  case the addition is element-wise.
")
  (:method ((a number) (b number))
    (+ a b))
  (:method ((a standard-tensor) (b standard-tensor))
    (axpy 1 a b))
  (:method ((a number) (b standard-tensor))
    (axpy a nil b))
  (:method ((a standard-tensor) (b number))
    (axpy b nil a)))

(definline t+ (&rest objs)
  (reduce #'tb+ objs))
(definline m+ (&rest objs)
  (apply #'t+ objs))
(definline m.+(&rest objs)
  (apply #'t+ objs))
;;
(defgeneric tb- (a b)
  (:documentation
   "
  Syntax
  ======
  (t- a b)

  Purpose
  =======
  Create a new matrix which is the sum of A and B.
  A or B (but not both) may be a scalar, in which
  case the addition is element-wise.
")
  (:method ((a number) (b number))
    (- a b))
  (:method ((a standard-tensor) (b standard-tensor))
    (axpy -1 b a))
  (:method ((a number) (b standard-tensor))
    (axpy a nil (scal -1 b)))
  (:method ((a standard-tensor) (b number))
    (axpy (- b) nil a)))

(definline t- (&rest objs)
  (if (cdr objs)
      (reduce #'tb- objs)
      (scal -1 (car objs))))
(definline m- (&rest objs)
  (apply #'t- objs))
(definline m.- (&rest objs)
  (apply #'t- objs))
;;
(defgeneric tb^ (a b))

(define-tensor-method tb^ ((a standard-tensor :input) (b standard-tensor :input)) 
  `(if (and (tensor-vectorp a) (tensor-vectorp b))
       (ger 1 a b nil nil)
       (let* ((ret (zeros (append (dims a) (dims b)) ',(cl a)))
	      (ret-a (subtensor~ ret
				 (print (loop :for i :from 0 :below (order ret)
					   :collect (if (< i (order a)) '(nil nil) '(0 1))))
				 nil))
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
;;
(defgeneric tb* (a b)
  (:method ((a number) (b number))
    (cl:* a b))
  ;;Scaling
  (:method ((a number) (b standard-tensor))
    (scal a b))
  (:method ((a standard-tensor) (b number))
    (scal b a))
  ;;Matrix, vector/matrix product
  (:method ((a standard-tensor) (b standard-tensor))
    (cond
      ((and (tensor-matrixp a) (tensor-vectorp b))
       (gemv 1 a b nil nil :n))
      ((and (tensor-vectorp a) (tensor-matrixp b))
       (gemv 1 b a nil nil :t))
      ((and (tensor-matrixp a) (tensor-matrixp b))
       (gemm 1 a b nil nil))
      (t (error "Don't know how to multiply tensors."))))
  ;;Permutation action. Left action permutes axis-0, right action permutes axis-1.
  (:method ((a permutation) (b standard-tensor))
    (permute b a 0))
  (:method ((a standard-tensor) (b permutation))
    (permute a b 1))
  ;;The correctness of this depends on the left-right order in reduce (foldl).
  (:method ((a permutation) (b permutation))
    (compose a b)))

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

(definline t* (&rest objs)
  (reduce #'tb* objs))
(definline m* (&rest objs)
  (apply #'t* objs))
;;
(definline t.* (&rest objs)
  (reduce #'scal objs))
(definline m.* (&rest objs)
  (apply #'t.* objs))
;;
(definline t./ (&rest objs)
  (reduce #'div (reverse objs) :from-end t))
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

(defgeneric tb@ (a b)
  (:method ((a number) (b number))
    (cl:* a b))
  ;;Scaling
  (:method ((a number) (b standard-tensor))
    (scal a b))
  (:method ((a standard-tensor) (b number))
    (scal b a))
  ;;Matrix, vector/matrix product
  (:method ((a standard-tensor) (b standard-tensor))
    (cond
      ((and (tensor-vectorp a) (tensor-vectorp b))
       (dot a b nil))
      ((and (tensor-matrixp a) (tensor-vectorp b))
       (gemv 1 a b nil nil :n))
      ((and (tensor-vectorp a) (tensor-matrixp b))
       (gemv 1 b a nil nil :t))
      ((and (tensor-matrixp a) (tensor-matrixp b))
       (gemm 1 a b nil nil))
      (t (tensor-contraction a b))))
  ;;Permutation action. Left action permutes axis-0, right action permutes axis-1.
  (:method ((a permutation) (b standard-tensor))
    (transpose b (inv a)))
  (:method ((a standard-tensor) (b permutation))
    (transpose b a))
  ;;The correctness of this depends on the left-right order in reduce (foldl).
  (:method ((a permutation) (b permutation))
    (compose a b)))

(definline t@ (&rest objs)
  (reduce #'tb@ objs))
;;
(defgeneric tbsolve (a b)
  (:documentation "Solve a x = b")
  (:method ((a number) (b number))
    (cl:/ b a))  
  ;;Scaling
  (:method ((a number) (b standard-tensor))
    (scal (cl:/ a) b))
  ;;Matrix, vector/matrix product
  (:method ((a standard-tensor) (b (eql nil)))
    (cond
      ((and (tensor-matrixp a) (tensor-squarep a))
       (inv a))
      (t (error "Don't know how to solve the given equation."))))
  (:method ((a standard-tensor) (b standard-tensor))
    (cond
      ((and (tensor-matrixp a) (tensor-squarep a) (tensor-matrixp b))
       (getrs! (getrf! (copy a)) (copy b)))
      ((and (tensor-matrixp a) (tensor-squarep a) (tensor-vectorp b))
       (let ((tmp (zeros (list (aref (dimensions b) 0) 1) (class-of b))))
	 (copy! b (slice~ tmp 1))
	 (getrs! (getrf! (copy a)) b)
	 (let ((ret (slice~ tmp 1)))
	   (setf (slot-value tmp 'parent-tensor) nil)
	   ret)))
      (t (error "Don't know how to solve the given equation."))))
  ;;Permutation action. Left action permutes axis-0, right action permutes axis-1.
  (:method ((a permutation) (b standard-tensor))
    (permute b (inv a) 0))
  ;;The correctness of this depends on the left-right order in reduce (foldl).
  (:method ((a permutation) (b permutation))
    (compose (inv a) b)))
