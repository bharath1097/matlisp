(in-package :matlisp)

(deft/generic (t/lapack-gelsy-func #'subfieldp) sym ())
(deft/method t/lapack-gelsy-func (sym real-tensor) ()
  'matlisp-lapack:dgelsy)

(definline mzgelsy (m n nrhs a lda b ldb jpvt rcond rank work lwork info &optional (head-a 0) (head-b 0))
  (matlisp-lapack:zgelsy m n nrhs a lda b ldb jpvt rcond rank work lwork (t/store-allocator complex-tensor n) info head-a head-b))
(deft/method t/lapack-gelsy-func (sym complex-tensor) ()
  'mzgelsy)
;;
(deft/generic (t/lapack-gelsy! #'subtypep) sym (A lda B ldb rcond))
(deft/method t/lapack-gelsy! (sym blas-numeric-tensor) (A lda B ldb rcond)
  (using-gensyms (decl (A lda B ldb rcond) (lwork xxx))
    (with-gensyms (jpvt)
    `(let* (,@decl
	    (,jpvt (make-array (ncols ,A) :element-type '(unsigned-byte 32) :initial-element 0))
	    (,lwork -1))
       (declare (type ,sym ,A ,B)
		(type index-type ,lda ,ldb ,lwork)
		;;BEWARE: This will throw an error, if you use (simple-array (complex double-float) (*)) for store.
		(type ,(store-element-type sym) ,rcond)
		(type (simple-array (unsigned-byte 32) (*)) ,jpvt))
       (let-typed ((,xxx (t/store-allocator ,sym 1) :type ,(store-type sym)))
	 (,(macroexpand-1 `(t/lapack-gelsy-func ,sym))
	   (nrows ,A) (ncols ,A) (ncols ,B)
	   ,xxx ,lda
	   ,xxx ,ldb
	   ,jpvt ,rcond 0
	   ,xxx -1
	   0)
	 (setq ,lwork (ceiling (t/frealpart ,(field-type sym) (t/store-ref ,sym ,xxx 0)))))
       (,(macroexpand-1 `(t/lapack-gelsy-func ,sym))
	 (nrows ,A) (ncols ,A) (ncols ,B)
	 (the ,(store-type sym) (store ,A)) ,lda
	 (the ,(store-type sym) (store ,B)) ,ldb
	 ,jpvt ,rcond 0
	 (t/store-allocator ,sym ,lwork) ,lwork
	 0
	 (the index-type (head ,A)) (the index-type (head ,B)))))))
;;
(defgeneric gelsy! (A B &optional rcond)
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
       (copy! B (subtensor~ X `((0 ,(nrows B)) (nil nil)) t))
       (multiple-value-bind (sto-a sto-b jpvt rank work-out info) (t/lapack-gelsy! ,(cl b) A (or (blas-matrix-compatiblep A #\N) 0) X (or (blas-matrix-compatiblep X #\N) 0) rcond)
	 (declare (ignore sto-a sto-b work-out jpvt))
	 (unless (= info 0)
	   (error "gelsy returned ~a." info))
	 (values (copy (subtensor~ X `((0 ,(ncols A)) (nil nil)) t)) rank)))))

(definline lstsq (A B &optional (rcond nil rcond-p))
  (if rcond-p
      (gelsy A B rcond)
      (gelsy A B)))
