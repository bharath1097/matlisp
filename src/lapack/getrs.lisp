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

(defgeneric getrs! (A B &optional job-a)
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
  (:method :before ((A standard-matrix) (B standard-matrix) &optional job-a)
	   (declare (ignore job-a))
	   (assert (= (nrows A) (ncols A)
		      (nrows B)) nil 'tensor-dimension-mismatch)
	   ;;Well, we can't re-implement every algorithm in LAPACK to work
	   ;;with two strided matrices. Threw in the towel after BLAS.
	   (assert (and (consecutive-store-p A)
			(consecutive-store-p B))
		   nil 'tensor-store-not-consecutive)))

(defmacro generate-typed-getrs! (func-name (matrix-class lapack-func))
  (let* ((opt (get-tensor-class-optimization matrix-class)))
    (assert opt nil 'tensor-cannot-find-optimization :tensor-class matrix-class)
    `(defun ,func-name (A B job-A)
       (declare (type ,matrix-class A B)
		(type symbol job-A))
       (mlet* (((maj-A ld-A fop-A) (blas-matrix-compatible-p A job-A) :type (symbol index-type (string 1)))
	       ((maj-B ld-B fop-B) (blas-matrix-compatible-p B :n) :type (symbol index-type (string 1))))
	      (cond
		((eq maj-A :col-major)

(defmethod getrs! ((A real-matrix) (B real-matrix) &optional (job-A :n)) 
  (mlet* (((maj-A ld-A fop-A) (blas-matrix-compatible-p A job-A) :type (symbol index-type (string 1)))
	  ((maj-B ld-B fop-B) (blas-matrix-compatible-p B :n) :type (symbol index-type (string 1))))
	  
  (let* ((n (nrows a))
         (m (ncols b)))

    (declare (type fixnum n m)
             (type (simple-array (unsigned-byte 32) (*)) ipiv))

    (multiple-value-bind (x info)
        (dgetrs (case trans
                  (:C "C")
                  (:T "T")
                  (t "N"))
                n
                m
                (store a)
                n
                ipiv
                (store b)
                n
                0)
        
        (values 
         (make-instance 'real-matrix :nrows n :ncols m :store x)
         (if (zerop info)
             t
           info)))))

(defmethod getrs! ((a complex-matrix) ipiv (b complex-matrix) &key trans)

  (let* ((n (nrows a))
         (m (ncols b)))

    (declare (type fixnum n m)
             (type (simple-array (unsigned-byte 32) (*)) ipiv))

    (multiple-value-bind (x info)
        (zgetrs (case trans
                  (:C "C")
                  (:T "T")
                  (t "N"))
                n
                m
                (store a)
                n
                ipiv
                (store b)
                n
                0)
        
        (values 
         (make-instance 'complex-matrix :nrows n :ncols m :store x) 
         (if (zerop info)
             t
           info)))))

(defmethod getrs! ((a standard-matrix) ipiv (b standard-matrix) &key trans)
  (let ((a (typecase a
             (real-matrix (copy! a (make-complex-matrix-dim (nrows a) (ncols a))))
             (complex-matrix a)
             (t (error "argument A given to GETRS! is not a REAL-MATRIX or COMPLEX-MATRIX"))))
        (b (typecase b
             (real-matrix (copy! b (make-complex-matrix-dim (nrows b) (ncols b))))
             (complex-matrix b)
             (t (error "argument B given to GETRS! is not a REAL-MATRIX or COMPLEX-MATRIX")))))
    (getrs! a ipiv b :trans trans)))

(defmethod getrs :before ((a standard-matrix) ipiv (b standard-matrix) &key trans)
  (declare (ignore trans))
  (if (not (= (nrows a) (ncols a) (nrows b) (length ipiv)))
      (error "dimensions of A,B,ipiv given to GETRS do not match")))

(defmethod getrs ((a real-matrix) ipiv (b real-matrix) &key trans)
  (getrs! a ipiv (copy b) :trans trans))

(defmethod getrs ((a complex-matrix) ipiv (b complex-matrix) &key trans)
  (getrs! a ipiv (copy b) :trans trans))

(defmethod getrs ((a standard-matrix) ipiv (b standard-matrix) &key trans)
  (let ((a (typecase a
             (real-matrix (copy! a (make-complex-matrix-dim (nrows a) (ncols a))))
             (complex-matrix (copy a))
             (t (error "argument A given to GETRS is not a REAL-MATRIX or COMPLEX-MATRIX"))))
        (b (typecase b
             (real-matrix (copy! b (make-complex-matrix-dim (nrows b) (ncols b))))
             (complex-matrix (copy b))
             (t (error "argument B given to GETRS is not a REAL-MATRIX or COMPLEX-MATRIX")))))
    (getrs! a ipiv b :trans trans)))

(defgeneric getrs (a b &optional job-a)
  (:documentation
 "
  Sytnax
  ======
  (GETRS a ipiv b [:trans :N])

  Purpose
  =======
  Same as GETRS! except that B is not overwritten.
"))
