;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: :matlisp; Base: 10 -*-

(in-package "MATLISP")

(defgeneric gelsy! (a b rcond)
  (:documentation "Destructive version of GELSY.  See GELSY."))

(defgeneric gelsy (a b rcond)
  (:documentation
   "
   Syntax
   =======

   (GELSY A B &key TOL)

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
"))

(defun check-info (info function-name)
  (unless (= info 0)
    (error "~a: error in argument ~d" function-name (- info))))

(defun dgelsy-workspace-inquiry (m n nrhs a lda b ldb jpvt rcond rank)
  (let ((work (allocate-real-store 1)))
    (multiple-value-bind
	  (store-a store-b store-jpvt rank store-work info)
	(lapack::dgelsy m n nrhs (store a) lda (store b) ldb jpvt rcond rank
			work -1 0)
      (declare (ignore store-a store-b store-jpvt rank store-work))
      (check-info info "dgelsy"))
    (values (ceiling (realpart (aref work 0))))))

(defmethod gelsy! ((a real-matrix) (b real-matrix) rcond)
  (let* ((m (nrows a))
	 (n (ncols a))
	 (nrhs (ncols b))
	 (jpvt (allocate-integer4-store n 0))
	 (lda m)
	 (ldb (max n m))
	 (b-arg b))
    (when (and (< m n))
      ;; In this case we need to extend the matrix which stores B
      ;; since it will be used to store the computation result
      (setq b-arg (make-real-matrix n nrhs))
      (dotimes (i m)
	(dotimes (j nrhs)
	  (setf (matrix-ref b-arg i j)
		(matrix-ref b i j)))))
    (let* ((lwork (dgelsy-workspace-inquiry m n nrhs a lda b-arg ldb jpvt
					    rcond 0))
	   (work (allocate-real-store lwork)))
      (assert (= m (nrows b)))
      (multiple-value-bind
	    (store-a store-b store-jpvt rank store-work info)
	  (lapack::dgelsy m n nrhs (store a) lda (store b-arg) ldb jpvt rcond 0
			  work lwork 0)
	(declare (ignore store-a store-jpvt store-work))
	(check-info info "dgelsy")
	(let ((x (make-real-matrix n nrhs)))
	  ;; extract the matrix X from B
	  (dotimes (i n)
	    (dotimes (j nrhs)
	      (setf (matrix-ref x i j)
		    (aref store-b (fortran-matrix-indexing i j n)))))
	  (values x rank))))))

(defmethod gelsy ((a real-matrix) (b real-matrix) rcond)  
  (gelsy! (copy a) (copy b) rcond))

;; Example

#|

(let* ((m 100)
       (n 100)
       (a (rand m n))
       (x (rand n 1))
       (b (m* a x))
       (eps (coerce (expt 2 -52) 'double-float))
       (rcond (* eps (max m n))))
  (multiple-value-bind (r1 rank)
      (matlisp::gelsy a b rcond)
    (list rank
	  (norm (m- x r1)))))

|#
