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
		(type permutation-action ipiv))
       (mlet*
	(((maj-A ld-A fop-A) (blas-matrix-compatible-p A :n) :type (symbol index-type nil)))
	(assert maj-A nil 'tensor-not-consecutive-store)
	(multiple-value-bind (new-a new-ipiv info)
	    (,lapack-func
	     (nrows A) (ncols A) (store A)
	     ld-A (repr ipiv) 0)
	  (declare (ignore new-a new-ipiv))
	  ;;Convert from 1-based indexing to 0-based indexing, and fix
	  ;;other Fortran-ic quirks
	  (assert (= info 0) nil 'invalid-arguments)
	  (format t "~a~%" ipiv)
	  (let-typed ((ipiv-repr (repr ipiv) :type perrepr-vector))
		     (loop for i of-type fixnum from 0 below (length ipiv-repr)
			do (decf (aref ipiv-repr i)))
		     (loop for i of-type fixnum from 0 below (length ipiv-repr)
			do (let ((val (aref ipiv-repr i)))
			     (setf (aref ipiv-repr val) i))))
	  (values A ipiv info))))))

(generate-typed-getrf! real-typed-getrf! (real-matrix dgetrf))
	   
       
(defgeneric getrf! (A ipiv)
  (:documentation
"
  Syntax
  ======
  (GETRF a)

  Purpose
  =======
  Given an NxM matrix A, compute its LU factorization using
  partial pivoting, row interchanges:

                A = P * L * U

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
  (:method :before ((A standard-matrix) (ipiv permutation-action))
	   (assert (>= (group-rank ipiv) (idx-min (dimensions A))) nil 'invalid-value
		   :given (group-rank ipiv) :expected '(>= (group-rank ipiv) (idx-min (dimensions A))))))

(defmethod getrf! ((a real-matrix) (ipiv permutation-action))
  (let* ((n (nrows a))
	 (m (ncols a))
	 (ipiv #+:pre-allocate-workspaces
	       (or ipiv *ipiv*)
	       #-:pre-allocate-workspaces
	       (or ipiv (make-array (min n m) :element-type '(unsigned-byte 32)))))

    (declare (type fixnum n m))
    (multiple-value-bind (new-a new-ipiv info)
	(dgetrf n          ;; M
		m          ;; N
		(store a)  ;; A
		n          ;; LDA
		ipiv       ;; IPIV
		0)         ;; INFO
      (declare (ignore new-a new-ipiv))
      (values a ipiv (if (zerop info)
			 t
		       info)))))



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



(defmethod getrf! ((a complex-matrix) &optional ipiv)
  (let* ((n (nrows a))
	 (m (ncols a))
	 (ipiv #+:pre-allocate-workspaces
	       (or ipiv *ipiv*)
	       #-:pre-allocate-workspaces
	       (or ipiv (make-array (min n m) :element-type '(unsigned-byte 32)))))

    (declare (type fixnum n m))
    (multiple-value-bind (new-a new-ipiv info)
	(zgetrf n          ;; M
		m          ;; N
		(store a)  ;; A
		n          ;; LDA
		ipiv       ;; IPIV
		0)         ;; INFO
      (declare (ignore new-a new-ipiv))
      (values a ipiv (if (zerop info)
			 t
		       info)))))


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
