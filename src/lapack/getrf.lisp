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

;;Possibly add Lisp routine in the future.
(defmacro generate-typed-getrf! (func-name (tensor-class lapack-func))
  (let* ((opt (if-ret (get-tensor-class-optimization-hashtable tensor-class)
		      (error 'tensor-cannot-find-optimization :tensor-class tensor-class)))
	 (matrix-class (getf opt :matrix)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
	 (let ((opt (get-tensor-class-optimization-hashtable ',tensor-class)))
	   (assert opt nil 'tensor-cannot-find-optimization :tensor-class ',tensor-class)
	   (setf (getf opt :getrf) ',func-name
		 (get-tensor-class-optimization ',tensor-class) opt)))
       (defun ,func-name (A ipiv)
	 (declare (type ,matrix-class A)
		  (type permutation-pivot-flip ipiv))
	 (mlet* (((maj-A ld-A fop-A) (blas-matrix-compatible-p A :n) :type (symbol index-type nil)))
		(if (eq maj-A :col-major)
		    (multiple-value-bind (n-A n-ipiv info) (,lapack-func
							    (nrows A) (ncols A) (store A)
							    ld-A (store ipiv) 0)
		      (declare (ignore n-A n-ipiv))
		      (assert (= info 0) nil 'invalid-arguments :argnum (1- (- info)) :message (format-to-string "GETRF returned INFO: ~a." info))
		      ;;Convert back to 0-based indexing.
		      (let-typed ((pidv (store ipiv) :type pindex-store-vector))
				 (very-quickly
				   (loop :for i :of-type index-type :from 0 :below (length pidv)
				      :do (decf (aref pidv i)))))
		      (values A ipiv info))
		    (let ((tmp (,(getf opt :copy) A (with-order :col-major (,(getf opt :zero-maker) (dimensions A))))))
		      (multiple-value-bind (n-tmp n-ipiv info) (,func-name tmp ipiv)
			(declare (ignore n-tmp n-ipiv))
			(,(getf opt :copy) tmp A)
			(values A ipiv info)))))))))

(generate-typed-getrf! real-typed-getrf! (real-tensor dgetrf))
(generate-typed-getrf! complex-typed-getrf! (complex-tensor zgetrf))

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
"))

(defmethod getrf! ((A real-matrix))
  (let ((ipiv (let* ((*check-after-initializing?* nil)
		     (ret (make-instance 'permutation-pivot-flip :store (pindex-id (lvec-min (dimensions A))))))
		(setf (permutation-size ret) (length (store ret)))
		ret)))
    (real-typed-getrf! A ipiv)))

(defmethod getrf! ((A complex-matrix))
  (let ((ipiv (let* ((*check-after-initializing?* nil)
		     (ret (make-instance 'permutation-pivot-flip :store (pindex-id (lvec-min (dimensions A))))))
		(setf (permutation-size ret) (length (store ret)))
		ret)))
    (complex-typed-getrf! A ipiv)))

;;
(defgeneric lu (a &optional split-lu?)
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

;;Sure I can do this with a defmethod (slowly), but where's the fun in that ? :)
(defmacro make-lu (tensor-class)
  (let* ((opt (if-ret (get-tensor-class-optimization-hashtable tensor-class)
		      (error 'tensor-cannot-find-optimization :tensor-class tensor-class)))
	 (matrix-class (getf opt :matrix)))
    `(defmethod lu ((A ,matrix-class) &optional (split-lu? t))
       (multiple-value-bind (lu ipiv info)
	   (getrf! (with-order :col-major
		     (,(getf opt :copy) A (,(getf opt :zero-maker) (dimensions A)))))
	 (declare (ignore info))
	 (let* ((n (nrows a))
		  (m (ncols a))
		  (p (min n m)))
	     (declare (type fixnum n m p))
	     ;; Extract the lower triangular part, if requested
	     (if split-lu?
		 (if (= p m)
		     (let*-typed ((umat (,(getf opt :zero-maker) (list p m)) :type ,matrix-class)
				  ;;
				  (u.rstd (row-stride umat) :type index-type)
				  (u.cstd (col-stride umat) :type index-type)
				  (u.of (head umat) :type index-type)
				  (u.sto (store umat) :type ,(linear-array-type (getf opt :store-type)))
				  ;;
				  (lu.rstd (row-stride lu) :type index-type)
				  (lu.cstd (col-stride lu) :type index-type)
				  (lu.of (head lu) :type index-type)
				  (lu.sto (store lu) :type ,(linear-array-type (getf opt :store-type))))
				 (very-quickly
				   (loop :for i :of-type index-type :from 0 :below p
				      :do (let-typed ((lu.of-ii lu.of :type index-type))
						     (loop :repeat (- m i)
							:do (progn
							      (,(getf opt :reader-writer) lu.sto lu.of u.sto u.of)
							      (,(getf opt :value-writer) (,(getf opt :fid+)) lu.sto lu.of)
							      (incf lu.of lu.cstd)
							      (incf u.of u.cstd)))
						     (,(getf opt :value-writer) (,(getf opt :fid*)) lu.sto lu.of-ii)
						     (incf lu.of (- lu.rstd (the index-type (* (- m i 1) lu.cstd))))
						     (incf u.of (- u.rstd (the index-type (* (- m i 1) u.cstd)))))))
				   (values lu umat ipiv))
		     (let*-typed ((lmat (,(getf opt :zero-maker) (list n p)) :type ,matrix-class)
				  ;;
				  (l.rstd (row-stride lmat) :type index-type)
				  (l.cstd (col-stride lmat) :type index-type)
				  (l.of (head lmat) :type index-type)
				  (l.sto (store lmat) :type ,(linear-array-type (getf opt :store-type)))
				  ;;
				  (lu.rstd (row-stride lu) :type index-type)
				  (lu.cstd (col-stride lu) :type index-type)
				  (lu.of (head lu) :type index-type)
				  (lu.sto (store lu) :type ,(linear-array-type (getf opt :store-type))))
				 (very-quickly
				   (loop :for j :of-type index-type :from 0 :below p
				      :do (progn
					    (,(getf opt :value-writer) (,(getf opt :fid*)) l.sto l.of)
					    (loop :repeat (- n j 1)
					       :do (progn
						     (incf lu.of lu.rstd)
						     (incf l.of l.rstd)
						     (,(getf opt :reader-writer) lu.sto lu.of l.sto l.of)
						     (,(getf opt :value-writer) (,(getf opt :fid+)) lu.sto lu.of)))
					    (incf lu.of (- lu.cstd (the index-type (* (- n j 2) lu.rstd))))
					    (incf l.of (- l.cstd (the index-type (* (- n j 2) l.rstd)))))))
				 (values lmat lu ipiv)))
		 (values lu ipiv)))))))

(make-lu real-tensor)
(make-lu complex-tensor)
