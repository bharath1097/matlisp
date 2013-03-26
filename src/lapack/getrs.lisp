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

(defmacro generate-typed-getrs! (func-name (tensor-class lapack-func))
  (let* ((opt (if-ret (get-tensor-class-optimization-hashtable tensor-class)
		      (error 'tensor-cannot-find-optimization :tensor-class tensor-class)))
	 (matrix-class (getf opt :matrix))
	 (vector-class (getf opt :vector)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
	 (let ((opt (get-tensor-class-optimization-hashtable ',tensor-class)))
	   (assert opt nil 'tensor-cannot-find-optimization :tensor-class ',tensor-class)
	   (setf (getf opt :getrs) ',func-name
		 (get-tensor-class-optimization ',tensor-class) opt)))
       (defun ,func-name (A B ipiv job-A)
	 (declare (type ,matrix-class A)
		  (type (or ,matrix-class ,vector-class) B)
		  (type permutation-pivot-flip ipiv))
	 (mlet* (((maj-A ld-A fop-A) (blas-matrix-compatible-p A job-A) :type (symbol index-type string))
		 ((maj-B ld-B fop-B) (etypecase B
				       (,matrix-class (blas-matrix-compatible-p B :n))
				       (,vector-class (if (= (aref (strides B) 0) 1)
							  (values :col-major (aref (dimensions B) 0) nil)
							  (values nil 0 nil))))
		  :type (symbol index-type nil)))
		(if (and (eq maj-B :col-major) (eq maj-A :col-major))
		    (progn
		      ;;Convert to 1-based indexing.
		      (let-typed ((pidv (store ipiv) :type pindex-store-vector))
				 (very-quickly
				   (loop :for i :of-type index-type :from 0 :below (length pidv)
				      :do (incf (aref pidv i)))))
		      (multiple-value-bind (n-B info) (,lapack-func
						       fop-A (nrows A) (etypecase B (,matrix-class (ncols B)) (,vector-class 1))
						       (store A) ld-A
						       (store ipiv)
						       (store B) ld-b
						       0)
			(declare (ignore n-B))
			(assert (= info 0) nil 'invalid-arguments :argnum (1- (- info)) :message (format-to-string "GETRF returned INFO: ~a." info))
			;;Convert back to 0-based indexing.
			(let-typed ((pidv (store ipiv) :type pindex-store-vector))
				   (very-quickly
				     (loop :for i :of-type index-type :from 0 :below (length pidv)
					:do (decf (aref pidv i)))))
			(values B info)))
		    (let ((n-A (if (eq maj-A :col-major) A (,(getf opt :copy) A (with-order :col-major (,(getf opt :zero-maker) (dimensions A))))))
			  (n-B (if (eq maj-B :col-major) B (,(getf opt :copy) B (with-order :col-major (,(getf opt :zero-maker) (dimensions B)))))))
		      (multiple-value-bind (B.ret info) (,func-name n-A n-B ipiv job-A)
			(declare (ignore B.ret))
			(unless (eq maj-B :col-major)
			  (,(getf opt :copy) n-B B))
			(values B info)))))))))

(generate-typed-getrs! real-base-typed-getrs! (real-tensor dgetrs))
(definline real-typed-getrs! (A B ipiv job-A)
  (real-base-typed-getrs! A B ipiv (ecase job-A (:c :n) (:h :t) ((:n :t) job-A))))

(generate-typed-getrs! complex-base-typed-getrs! (complex-tensor zgetrs))
(definline complex-typed-getrs! (A B ipiv job-A)
  (let ((A (ecase job-A
	     ((:n :t) A)
	     ((:h :c) (let ((ret (complex-typed-copy! A (complex-typed-zeros (dimensions A)))))
			(real-typed-num-scal! -1d0 (tensor-imagpart~ ret))
			ret)))))
    (complex-base-typed-getrs! A B ipiv job-A)))
		 

;;
(defgeneric getrs! (ipiv A B &optional job-a)
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
  (:method :before ((ipiv permutation) (A standard-matrix) (B standard-matrix) &optional job-a)
	   (declare (ignore job-a))
	   (assert (and (= (nrows A) (ncols A) (nrows B)) (>= (permutation-size ipiv) (nrows A)))
		   nil 'tensor-dimension-mismatch)))

(defmethod getrs! ((ipiv permutation-pivot-flip) (A real-matrix) (B real-matrix) &optional (job-A :n))
  (real-typed-getrs! A B ipiv job-A))

(defmethod getrs! ((ipiv permutation-pivot-flip) (A real-matrix) (B real-vector) &optional (job-A :n))
  (real-typed-getrs! A B ipiv job-A))

(defmethod getrs! ((ipiv permutation-pivot-flip) (A complex-matrix) (B complex-matrix) &optional (job-A :n))
  (complex-typed-getrs! A B ipiv job-A))

(defmethod getrs! ((ipiv permutation-pivot-flip) (A complex-matrix) (B complex-vector) &optional (job-A :n))
  (complex-typed-getrs! A B ipiv job-A))

;;
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
