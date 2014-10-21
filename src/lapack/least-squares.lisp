(in-package :matlisp)
;;
(deft/generic (t/lapack-gelsy! #'subtypep) sym (A lda B ldb rcond))
(deft/method t/lapack-gelsy! (sym blas-numeric-tensor) (A lda B ldb rcond)
  (let* ((ftype (field-type sym)) (complex? (subtypep ftype 'cl:complex))
	 (rtype (field-type (realified-type sym))))
    (using-gensyms (decl (A lda B ldb rcond) (lwork xxx xxr jpvt))
      `(let* (,@decl
	      (,jpvt (make-array (ncols ,A) :element-type ',(matlisp-ffi::%ffc->lisp :integer) :initial-element 0)))
	 (declare (type ,sym ,A ,B)
		  (type index-type ,lda ,ldb)
		  (type ,(field-type (realified-type sym)) ,rcond)
		  (type (simple-array ,(matlisp-ffi::%ffc->lisp :integer) (*)) ,jpvt))
	 (with-field-elements ,sym (,@(when complex? `((,xxr (t/fid+ ,ftype) (dimensions ,A 1)))))
	   (with-lapack-query ,sym (,xxx ,lwork)
	     (ffuncall ,(blas-func "gelsy" ftype)
	       (:& :integer) (dimensions ,A 0) (:& :integer) (dimensions ,A 1) (:& :integer) (dimensions ,B 1)
	       (:* ,(lisp->ffc ftype) :+ (head ,A)) (the ,(store-type sym) (store ,A)) (:& :integer) ,lda
	       (:* ,(lisp->ffc ftype) :+ (head ,B)) (the ,(store-type sym) (store ,B)) (:& :integer) ,ldb
	       (:* :integer) (the (simple-array ,(matlisp-ffi::%ffc->lisp :integer) (*)) ,jpvt) (:& ,(lisp->ffc rtype t)) ,rcond (:& :integer :output) 0
	       (:* ,(lisp->ffc ftype)) ,xxx (:& :integer) ,lwork
	       ,@(when complex? `((:* ,(lisp->ffc ftype)) ,xxr))
	       (:& :integer :output) 0)))))))
;;
(defgeneric gelsy (A B &optional rcond)
  (:documentation "
   Syntax
   =======

   (GELSY A B &optional TOL)

   INPUT
   -----
   A       A Matlisp matrix of size M x N
   B       A Matlisp matrix of size M x P
   RCOND   A condition number

   OUTPUT
   ------
   X       A Matlisp matrix of size N x NRHS
   RANK    An integer

   Purpose
   =======

   Compute the minimum-norm solution to a real linear least
   squares problem:
       minimize || A * X - B ||
   using a complete orthogonal factorization of A.  A is an M-by-N
   matrix which may be rank-deficient.

   Several right hand side vectors b and solution vectors x can be
   handled in a single call; they are stored as the columns of the
   M-by-NRHS right hand side matrix B and the N-by-NRHS solution
   matrix X.

   The routine first computes a QR factorization with column pivoting:
       A * P = Q * [ R11 R12 ]
		   [  0  R22 ]
   with R11 defined as the largest leading submatrix whose estimated
   condition number is less than 1/RCOND.  The order of R11, RANK,
   is the effective rank of A.

   Then, R22 is considered to be negligible, and R12 is annihilated
   by orthogonal transformations from the right, arriving at the
   complete orthogonal factorization:
      A * P = Q * [ T11 0 ] * Z
		  [  0  0 ]
   The minimum-norm solution is then
      X = P * Z' [ inv(T11)*Q1'*B ]
		 [        0       ]
   where Q1 consists of the first RANK columns of Q.

   This routine is basically identical to the original xGELSX except
   three differences:
     o The call to the subroutine xGEQPF has been substituted by the
       the call to the subroutine xGEQP3. This subroutine is a Blas-3
       version of the QR factorization with column pivoting.
     o Matrix B (the right hand side) is updated with Blas-3.
     o The permutation of matrix B (the right hand side) is faster and
       more simple.

   Further Details
   ===============

   Based on contributions by
     A. Petitet, Computer Science Dept., Univ. of Tenn., Knoxville, USA
     E. Quintana-Orti, Depto. de Informatica, Universidad Jaime I, Spain
     G. Quintana-Orti, Depto. de Informatica, Universidad Jaime I, Spain

   =====================================================================
")
  (:method :before ((A standard-tensor) (B standard-tensor) &optional rcond)
	   (assert (and (tensor-matrixp A) (tensor-matrixp B) (= (nrows A) (nrows B))) nil 'tensor-dimension-mismatch)
	   (assert (or (null rcond) (> rcond 0)) nil 'invalid-value :expected '(> rcond 0) :given rcond :message "Invalid rcond.")))

(define-tensor-method gelsy ((A standard-tensor :input) (B standard-tensor :input) &optional (rcond *default-rcond*))
  `(let* ((A (with-colm (copy A ',(cl b)))))
     (declare (type ,(cl b) A))
     (let* ((mn (max (nrows A) (ncols A)))
	    (X (with-colm (zeros (list mn (ncols B)) ',(cl b)))))
       (copy! B (subtensor~ X `((0 ,(nrows B)) (nil nil))))
       (letv* ((rank info (t/lapack-gelsy! ,(cl b) A (or (blas-matrix-compatiblep A #\N) 0) X (or (blas-matrix-compatiblep X #\N) 0) rcond)))
	 (unless (= info 0)
	   (error "gelsy returned ~a." info))
	 (values (copy (subtensor~ X `((0 ,(ncols A)) (nil nil)))) rank)))))

(definline lstsq (A B &optional (rcond nil rcond-p))
  (let ((B (matrixify~ B)))
    (if rcond-p
	(gelsy A B rcond)
	(gelsy A B))))

;; (let* ((a (randn '(10 5)))
;;        (x (randn '(5 5)))
;;        (b (t* a x)))
;;   (norm (t- x (lstsq a b))))
