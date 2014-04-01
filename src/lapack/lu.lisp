;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: :matlisp; Base: 10 -*-
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copyright (c) 2000 The Regents of the University of California.
;;; All rights reserved.
;;;
;;; Permission is hereby granted, without written agreement and without
;;; license or royalty fees, to use, copy, modify, and distribute this
;;; software and its documentation for any purpose, provided that the
;;; above copyright notice and the following two paragraphs appear in all
;;; copies of this software.
;;;
;;; IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY
;;; FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
;;; ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
;;; THE UNIVERSITY OF CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.
;;;
;;; THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
;;; INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE SOFTWARE
;;; PROVIDED HEREUNDER IS ON AN "AS IS" BASIS, AND THE UNIVERSITY OF
;;; CALIFORNIA HAS NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES,
;;; ENHANCEMENTS, OR MODIFICATIONS.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package #:matlisp)

(deft/generic (t/lapack-getrf-func #'subfieldp) sym ())
(deft/method t/lapack-getrf-func (sym real-tensor) ()
  'dgetrf)
(deft/method t/lapack-getrf-func (sym complex-tensor) ()
  'zgetrf)
;;
(deft/generic (t/lapack-getrf! #'subtypep) sym (A lda ipiv))

(deft/method t/lapack-getrf! (sym blas-numeric-tensor) (A lda ipiv)
  (using-gensyms (decl (A lda ipiv))
    (with-gensyms (m n)
      `(let* (,@decl
	      (,m (nrows A))
	      (,n (ncols A)))
	 (declare (type ,sym ,A)
		  (type (simple-array (unsigned-byte 32) (*)) ,ipiv)
		  (type index-type ,lda))
	 (,(macroexpand-1 `(t/lapack-getrf-func ,sym))
	   ,m ,n
	   (the ,(store-type sym) (store ,A)) ,lda
	   ,ipiv 0
	   (the index-type (head ,A)))))))

;;
(defgeneric getrf! (A)
  (:documentation
"
  Syntax
  ======
  (GETRF! a)

  Purpose
  =======
  Given an NxM matrix A, compute its LU factorization using
  partial pivoting, row or column interchanges:

		A = P * L * U  (if A is row-major ordered)
		A = L * U * P' (if A is col-major ordered)

  where:

	 P: permutation matrix
	 L: lower triangular with unit diagonal elements
	    (lower trapezoidal when N>M)
	 U: upper triangular
	    (upper trapezoidal when N<M)

  Return Values
  =============
  [1] The factors L and U from the factorization A = P*L*U  where the
      unit diagonal elements of L are not stored. (overwriting A)
  [2] IPIV
  [3] INFO = T: successful
	     i:  U(i,i) is exactly zero.
")
  (:method :before ((A standard-tensor))
	   (assert (tensor-matrixp A)
		   nil 'tensor-dimension-mismatch)))

(define-tensor-method getrf! ((A blas-numeric-tensor :output))
  `(let ((upiv (make-array (lvec-min (the index-store-vector (dimensions A))) :element-type '(unsigned-byte 32))))
     (declare (type (simple-array (unsigned-byte 32) (*)) upiv))
     (with-columnification (() (A))
       (multiple-value-bind (lda opa) (blas-matrix-compatiblep A #\N)
	 (declare (ignore opa))
	 (multiple-value-bind (sto piv info) (t/lapack-getrf! ,(cl a) A lda upiv)
	   (declare (ignore sto piv))
	   (unless (= info 0)
	     (if (< info 0)
		 (error "GETRF: the ~a'th argument had an illegal value." (- info))
		 (warn 'singular-matrix :message "GETRF: U(~a, ~:*~a) is exactly zero. The factorization has been completed, but the factor U is exactly singular, and division by zero will occur if it is used to solve a system of equations." :position info))))))
     (let ((perm (let ((ret (let ((*check-after-initializing?* nil)) (make-instance 'permutation-pivot-flip :store (pflip.f->l upiv)))))
		   (setf (slot-value ret 'permutation-size) (length upiv))
		   ret)))
       (setf (gethash 'getrf (attributes A)) upiv)
       (values A perm))))

#+nil
(let ((a (copy! #2a((1 2) (3 4)) (zeros '(2 2)))))
  (multiple-value-bind (mat perm) (time (getrf! a))
    (time (permute! mat perm))))

;;
(defgeneric lu (a &optional split-lu?)
  (:documentation
  "
  Syntax
  ======
  (LU a split-lu?)

  Purpose
  =======
  Computes the LU decomposition of A.

  This functions is an interface to GETRF!

  If SPLIT-LU? is T, then return (L, U, P), otherwise
  returns (LU, P).
"))

(define-tensor-method lu ((a blas-numeric-tensor :input) &optional (split-lu? t))
  `(multiple-value-bind (lu perm) (getrf! (copy a))
     (if (not split-lu?) (values lu perm)
	 (let* ((min.d (lvec-min (dimensions lu)))
		(l (tricopy! 1 (tricopy! lu (zeros (list (aref (dimensions lu) 0) min.d) ',(cl a)) :l) :d))
		(u (tricopy! lu (zeros (list min.d (aref (dimensions lu) 1)) ',(cl a)) :u)))
	   (values l u perm)))))
;;
(deft/generic (t/lapack-getrs-func #'subfieldp) sym ())
(deft/method t/lapack-getrs-func (sym real-tensor) ()
  'dgetrs)
(deft/method t/lapack-getrs-func (sym complex-tensor) ()
  'zgetrs)
;;
(deft/generic (t/lapack-getrs! #'subtypep) sym (A lda B ldb ipiv transp))

(deft/method t/lapack-getrs! (sym blas-numeric-tensor) (A lda B ldb ipiv transp)
  (using-gensyms (decl (A lda B ldb ipiv transp))
   `(let* (,@decl)
      (declare (type ,sym ,A ,B)
	       (type (simple-array (unsigned-byte 32) (*)) ,ipiv)
	       (type index-type ,lda ,ldb)
	       (type character ,transp))
      (,(macroexpand-1 `(t/lapack-getrs-func ,sym))
	,transp
	(nrows ,A) (ncols ,B)
	(the ,(store-type sym) (store ,A)) ,lda ,ipiv
	(the ,(store-type sym) (store ,B)) ,ldb
	0
	(the index-type (head ,A)) (the index-type (head ,B))))))

#+nil(let ((a (copy! #2a((1 2) (3 1)) (zeros '(2 2))))
      (b (copy! #2a((3 3) (2 1)) (zeros '(2 2)))))
  (getrf! a)
  (getrs! a b))

(defgeneric getrs! (A B &optional job ipiv)
  (:documentation
   "
  Syntax
  ======
  (GETRS! a b [:trans :N])

  Purpose
  =======
  Solves a system of linear equations
      A * X = B  or  A' * X = B
  with a general N-by-N matrix A using the LU factorization computed
  by GETRF.  A and IPIV are the results from GETRF, TRANS specifies
  the form of the system of equations: 
           = 'N':  A * X = B  (No transpose)
           = 'T':  A'* X = B  (Transpose)
           = 'C':  A'* X = B  (Conjugate transpose)

  Return Values
  =============
  [1] The NxM matrix X. (overwriting B)
  [4] INFO = T: successful
             i:  U(i,i) is exactly zero.  The LU factorization
                 used in the computation has been completed, 
                 but the factor U is exactly singular.
                 Solution could not be computed.
")
  (:method :before ((A standard-tensor) (B standard-tensor) &optional (job :n) ipiv)
	   (assert (and (tensor-matrixp A) (tensor-matrixp B)
			(= (nrows A) (ncols A) (nrows B))
			(or (not ipiv) (>= (permutation-size ipiv) (nrows A))))
		   nil 'tensor-dimension-mismatch)
	   (assert (member job '(:n :t :c)) nil 'invalid-value
		   :given job :expected `(member job '(:n :t :c)))))

(define-tensor-method getrs! ((A blas-numeric-tensor :input) (B blas-numeric-tensor :output) &optional (job :n) ipiv)
  `(let ((upiv (if ipiv
		   (pflip.l->f (store (etypecase ipiv
					(permutation-action (action->pivot-flip ipiv))
					(permutation-cycle (action->pivot-flip (cycle->action ipiv)))
					(permutation-pivot-flip ipiv))))
		   (or (gethash 'getrf (attributes A)) (error "Cannot find permutation for the PLU factorisation of A."))))
	 (cjob (aref (symbol-name job) 0)))
     (declare (type (simple-array (unsigned-byte 32) (*)) upiv))
     (with-columnification (((A #\C)) (B))
       (mlet* (((lda opa) (blas-matrix-compatiblep A cjob))
	       (ldb (blas-matrix-compatiblep B #\N)))
	      (multiple-value-bind (sto info) (t/lapack-getrs! ,(cl a) A lda B ldb upiv opa)
		(declare (ignore sto))
		(unless (= info 0)
		  (error "getrs returned ~a. the ~a'th argument had an illegal value." (- info))))))
     B))
