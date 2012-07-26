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

(defmacro generate-typed-getrf! (func-name (matrix-class lapack-func))
  (let* ((opt (get-tensor-class-optimization matrix-class)))
    (assert opt nil 'tensor-cannot-find-optimization :tensor-class matrix-class)
    `(defun ,func-name (A ipiv)
       (declare (type ,matrix-class A)
		(type permutation-pivot-flip ipiv))
       (mlet* (((maj-A ld-A fop-A) (blas-matrix-compatible-p A :n) :type (symbol index-type nil)))
	   (assert maj-A nil 'tensor-store-not-consecutive)
	   (multiple-value-bind (new-A new-ipiv info)
	    (,lapack-func
	     (nrows A) (ncols A) (store A)
	     ld-A (repr ipiv) 0)
	  (declare (ignore new-A new-ipiv))
	  ;;Convert from 1-based indexing to 0-based indexing, and fix
	  ;;other Fortran-ic quirks
	  (assert (= info 0) nil 'invalid-arguments :argnum (1- (- info)) :message (format-to-string "GETRF returned INFO: ~a." info))
	  (let-typed ((pidv (repr ipiv) :type perrepr-vector))
		     (very-quickly
		       (loop for i from 0 below (length pidv)
			  do (decf (aref pidv i)))))
	   (if (eq maj-A :row-major)
	       ;;Crout's decomposition
	       (values A (list :decomposition-type :|U_ii=1| :column-permutation ipiv))
	       ;;Dolittle's decomposition
	       (values A (list :decomposition-type :|L_ii=1| :row-permutation ipiv))))))))

(generate-typed-getrf! real-typed-getrf! (real-matrix dgetrf))
(generate-typed-getrf! complex-typed-getrf! (complex-matrix zgetrf))

#+nil
(let ((A (make-real-tensor '((1 2)
			     (3 4))))
      (idiv (make-pidx (perv 0 1))))
  (real-typed-getrf! A idiv))


(defgeneric getrf! (A ipiv)
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

  If the optional argument IPIV is provided it must
  be a (SIMPLE-ARRAY (UNSIGNED-BYTE 32) (*)) of dimension >= (MIN N M)

  IPIV is filled with the pivot indices that define the permutation
  matrix P:
           
        row i of the matrix was interchanged with row IPIV(i).
 
  If IPIV is not provided, it is allocated by GESV.

  Return Values
  =============
  [1] The factors L and U from the factorization A = P*L*U  where the 
      unit diagonal elements of L are not stored. (overwriting A)
  [2] IPIV
  [3] INFO = T: successful
             i:  U(i,i) is exactly zero. 
")
  (:method :before ((A standard-matrix) (ipiv permutation-pivot-flip))
	   (assert (>= (group-rank ipiv) (idx-min (dimensions A))) nil 'invalid-value
		   :given (group-rank ipiv) :expected '(>= (group-rank ipiv) (idx-min (dimensions A))))))

(defmethod getrf! ((A real-matrix) (ipiv permutation-pivot-flip))
  (let* ((copy? (not (consecutive-store-p A)))
	 (cp-A (if copy? (copy A) A))
	 (ret (multiple-value-list (real-typed-getrf! cp-A ipiv))))
    (when copy?
      (copy! (first ret) A)
      (rplaca ret A))
    (values-list ret)))

(defmethod getrf! ((A complex-matrix) (ipiv permutation-pivot-flip))
  (let* ((copy? (not (consecutive-store-p A)))
	 (cp-A (if copy? (copy A) A))
	 (ret (multiple-value-list (complex-typed-getrf! cp-A ipiv))))
    (when copy?
      (copy! (first ret) A)
      (rplaca ret A))
    (values-list ret)))

(defun permute-idx (A arg perm)
  (declare (type standard-matrix A)
	   (type permutation-pivot-flip perm))
  (let* ((idiv (repr perm)))
    (multiple-value-bind (tone ttwo) (let ((slst (make-list (rank A) :initial-element '\:)))
				       (rplaca (nthcdr arg slst) 0)
				       (values (sub-tensor~ A slst nil) (sub-tensor~ A slst nil)))
      (let ((argstd (aref (strides A) arg)))
	(loop for i from 0 below (length idiv)
	   do (progn
		(unless (= i (aref idiv i))
		  (setf (head ttwo) (* (aref idiv i) argstd))
		  (swap! tone ttwo))
		(incf (head tone) argstd))))))
  A)

(defun split-lu (A op-info)
  (declare (type standard-matrix A))
  (destructuring-bind (&key decomposition-type row-permutation col-permutation) op-info
    (assert (member decomposition-type '(:|U_ii=1| :|L_ii=1|)) nil 'invalid-arguments :message "Bad decomposition-type")
    
;;
(defgeneric lu (a &key with-l with-u with-p)
  (:documentation 
  "
  Syntax
  ======
  (LU a [:WITH-P with-p] [:WITH-L with-l] [:WITH-U with-u])
 
  Purpose
  =======
  Computes the LU decomposition of A. 

  This functions is an interface to GETRF!

  Return Values
  =============
  [1]      the factors L,U from the factorization in a single matrix,
           where the unit diagonal elements of L are not stored
  [2]-[4]  If WITH-X then X, in the order L,U,P

  By default WITH-L,WITH-U,WITH-P.
"))


(defmethod lu ((a standard-matrix) &key  (with-l t) (with-u t) (with-p t))
  (multiple-value-bind (lu ipiv info)
      (getrf! (copy a))
    (declare (ignore info))

    (let* ((result (list lu))
	   (n (nrows a))
	   (m (ncols a))
	   (p (min n m)))

      (declare (type fixnum n m p))

      ;; Extract the lower triangular part, if requested
      (when with-l
	(let ((lmat (typecase lu
		      (real-matrix (make-real-matrix-dim n p))
		      (complex-matrix (make-complex-matrix-dim n p)))))
	  (dotimes (i p)
	    (setf (matrix-ref lmat i i) 1.0d0))
	  (dotimes (i n)
	    (dotimes (j (min i p))
	      (setf (matrix-ref lmat i j) (matrix-ref lu i j))))

	  (push lmat result)))


      ;; Extract the upper triangular part, if requested
      (when with-u
	(let ((umat (typecase lu
		      (real-matrix (make-real-matrix-dim p m))
		      (complex-matrix (make-complex-matrix-dim p m)))))
	  (dotimes (i p)
	    (loop for j from i to (1- m)
		  do (setf (matrix-ref umat i j) (matrix-ref lu i j))))

	  (push umat result)))

      ;; Extract the permutation matrix, if requested
      (when with-p
	(let* ((npiv (length ipiv))
	       (pmat (make-real-matrix-dim n n))
	       (pidx (make-array n :element-type '(unsigned-byte 32))))
	  ;; Compute the P matrix from the pivot vector
	  (dotimes (k n)
	    (setf (aref pidx k) k))
	  (dotimes (k npiv)
	    (rotatef (aref pidx k) (aref pidx (1- (aref ipiv k)))))
	  (dotimes (k n)
	    (setf (matrix-ref pmat  (aref pidx k) k) 1))
	  (push pmat result)))

      ;; Return the final result
      (values-list (nreverse result)))))
