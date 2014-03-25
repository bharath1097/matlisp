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
  (reduce #'tb- objs))
(definline m- (&rest objs)
  (apply #'t- objs))
(definline m.- (&rest objs)
  (apply #'t- objs))
;;
(defgeneric tb^ (a b))

(define-tensor-method tb^ ((a standard-tensor :input) (b standard-tensor :input))
  `(let* ((ret (zeros (append (dims a) (dims b)) ',(cl a)))
	  (ret-a (let ((sli (make-list (order ret) :initial-element '(* * *))))
		   (do ((lst (nthcdr (order a) sli) (cdr lst)))
		       ((null lst))
		     (setf (car lst) '(0 * 1)))
		   (subtensor~ ret sli)))
	  (rbstr (subseq (strides ret) (order a)))
	  (sto-b (store b)))
     (mod-dotimes (idx (dimensions b))
       :with (linear-sums
	      (of-b (strides b) (head b))
	      (of-r rbstr (head ret)))
       :do (progn
	     (setf (slot-value ret-a 'head) of-r)
	     (axpy! (t/store-ref ,(cl b) sto-b of-b) a ret-a)))
     ret))

(definline t^ (&rest objs)
  (reduce #'tb^ objs))
;;
(defgeneric tb* (a b)
  (:method ((a standard-tensor) (b standard-tensor))
    (cond
      ((and (tensor-matrixp a) (tensor-vectorp b))
       (gemv 1 a b nil nil :n))
      ((and (tensor-vectorp a) (tensor-matrixp b))
       (gemv 1 b a nil nil :t))
      ((and (tensor-matrixp a) (tensor-matrixp b))
       (gemm 1 a b nil nil))
      (t (error "Don't know how to multiply tensors."))))
  (:method ((a number) (b standard-tensor))
    (scal a b))
  (:method ((a standard-tensor) (b number))
    (scal b a))
  (:method ((a number) (b number))
    (cl:* a b)))

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
