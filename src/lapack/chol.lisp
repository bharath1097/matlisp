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

(deft/generic (t/lapack-potrf-func #'subfieldp) sym ())
(deft/method t/lapack-potrf-func (sym real-tensor) ()
  'dpotrf)
(deft/method t/lapack-potrf-func (sym complex-tensor) ()
  'zpotrf)
;;
(deft/generic (t/lapack-potrf! #'subtypep) sym (A lda uplo))

(deft/method t/lapack-potrf! (sym blas-numeric-tensor) (A lda uplo)
  (using-gensyms (decl (A lda uplo))
    `(let* (,@decl)
       (declare (type ,sym ,A)
		(type index-type ,lda)
		(type character ,uplo))
       (,(macroexpand-1 `(t/lapack-potrf-func ,sym))
	 ,uplo
	 (nrows ,A)
	 (the ,(store-type sym) (store ,A)) ,lda
	 0
	 (the index-type (head ,A))))))

;;
(defgeneric potrf! (a &optional uplo)
  (:documentation "
  Syntax
  ======
  (POTRF! a)

  Purpose
  =======
  POTRF computes the Cholesky factorization of a real symmetric
  positive definite matrix A.

  This is the block version of the algorithm, calling Level 3 BLAS.

  Return Values
  =============
  [1] The factor U or L from the Cholesky
	  factorization A = U**T*U or A = L*L**T.
  [2] INFO = T: successful
	     i:  U(i,i) is exactly zero.
")
  (:method :before ((a standard-tensor) &optional (uplo :l))
	   (assert (tensor-square-matrixp a) nil 'tensor-dimension-mismatch
		   :message "Expected square matrix.")
	   (assert (member uplo '(:l :u)) nil 'invalid-arguments
		   :given uplo :expected `(member uplo '(:l :u)))))

(define-tensor-method potrf! ((a blas-numeric-tensor :output) &optional (uplo *default-uplo*))
  `(with-columnification (() (A))
     (multiple-value-bind (sto info) (t/lapack-potrf! ,(cl a) A (or (blas-matrix-compatiblep A #\N) 0) (char-upcase (aref (symbol-name uplo) 0)))
       (declare (ignore sto))
       (unless (= info 0)
	 (if (< info 0)
	     (error "POTRF: the ~a'th argument had an illegal value." (- info))
	     (restart-case (error 'matrix-not-pd :message "POTRF: the leading minor of order ~a is not p.d; the factorization could not be completed." :position info)
	       (increment-diagonal-and-retry (value)
		 (let-typed ((ss.a (lvec-foldr #'(lambda (x y) (declare (type index-type x y)) (the index-type (+ x y))) (strides a)) :type index-type)
			     (sto.a (store a) :type ,(store-type (cl a)))
			     (value (t/coerce ,(field-type (cl a)) value) :type ,(field-type (cl a))))
			    (loop :repeat (the index-type (lvec-min (dimensions a)))
			       :for of.a :of-type index-type := (head a) :then (the index-type (+ of.a ss.a))
			       :do (incf (aref sto.a of.a) value))
			    (potrf! a uplo))))))))
  'A)
;;
(deft/generic (t/lapack-potrs-func #'subfieldp) sym ())
(deft/method t/lapack-potrs-func (sym real-tensor) ()
  'dpotrs)
(deft/method t/lapack-potrs-func (sym complex-tensor) ()
  'zpotrs)
;;
(deft/generic (t/lapack-potrs! #'subtypep) sym (A lda B ldb uplo))

(deft/method t/lapack-potrs! (sym blas-numeric-tensor) (A lda B ldb uplo)
  (using-gensyms (decl (A lda B ldb uplo))
    `(let* (,@decl)
       (declare (type ,sym ,A ,B)
		(type index-type ,lda ,ldb)
		(type character ,uplo))
       (,(macroexpand-1 `(t/lapack-potrs-func ,sym))
	 ,uplo
	 (nrows ,A) (ncols ,B)
	 (the ,(store-type sym) (store ,A)) ,lda
	 (the ,(store-type sym) (store ,B)) ,ldb
	 0
	 (the index-type (head ,A)) (the index-type (head ,B))))))

;;
(defgeneric potrs! (A B &optional uplo)
  (:documentation "
  Syntax
  ======
  (POTRS! a b [:U :L])

  Purpose
  =======
  Solves a system of linear equations
      A * X = B  or  A' * X = B
  with a general N-by-N matrix A using the Cholesky LU factorization computed
  by POTRF.  A and are the results from POTRF, UPLO specifies
  the form of the system of equations:
	   = 'U':   A = U**T*U
	   = 'L':   A = L*L**T

  Return Values
  =============
  [1] The NxM matrix X. (overwriting B)
  [4] INFO = T: successful
	     i:  U(i,i) is exactly zero.  The LU factorization
		 used in the computation has been completed,
		 but the factor U is exactly singular.
		 Solution could not be computed.
")
  (:method :before ((A standard-tensor) (B standard-tensor) &optional (uplo :l))
	   (assert (and (tensor-matrixp A) (tensor-matrixp B)
			(= (nrows A) (ncols A) (nrows B)))
		   nil 'tensor-dimension-mismatch)
	   (assert (member uplo '(:l :u)) nil 'invalid-value
		   :given uplo :expected `(member uplo '(:u :l)))))

(define-tensor-method potrs! ((A blas-numeric-tensor :input) (B blas-numeric-tensor :output) &optional (uplo *default-uplo*))
  `(with-columnification ((A #\C) (B))
     (multiple-value-bind (sto info) (t/lapack-potrs! ,(cl a)
						      A (or (blas-matrix-compatiblep A #\N) 0)
						      B (or (blas-matrix-compatiblep B #\N) 0)
						      (aref (symbol-name uplo) 0))
       (declare (ignore sto))
       (unless (= info 0)
	 (error "POTRS returned ~a. the ~a'th argument had an illegal value." (- info)))))
  'B)
;;
(defgeneric chol (a &optional uplo)
  (:documentation
   "
  Syntax
  ======
  (CHOL a &optional uplo)

  Purpose
  =======
  Computes the Cholesky decomposition of A.

  This functions is an interface to POTRF!
"))

(defmethod chol ((a blas-numeric-tensor) &optional (uplo *default-uplo*))
  (let* ((l (potrf! (copy a) uplo))
	 (diag (tricopy! l (zeros (aref (dimensions a) 0) (class-of a)) :d)))
    (tricopy! 0d0 l (ecase uplo (:u :l) (:l :u)))
    (tricopy! diag l :d)
    l))
