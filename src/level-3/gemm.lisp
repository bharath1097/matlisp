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

;;Tweakable
(defparameter *gemm-fortran-call-lower-bound* 50
  "
  If the maximum dimension in the MM is lower than this
  parameter, then the lisp code is used by default, instead of
  calling BLAS. Used to avoid the FFI overhead when calling
  MM with small matrices.
  Default set with SBCL on x86-64 linux. A reasonable value
  is something between 20 and 200.")

(defmacro generate-typed-gemm! (func (matrix-class) (blas-gemm-func blas-gemv-func))
  (let* ((opt (get-tensor-class-optimization matrix-class)))
    (assert opt nil 'tensor-cannot-find-optimization :tensor-class matrix-class)
    `(defun ,func (alpha A B beta C job)
       (declare (type ,(getf opt :element-type) alpha beta)
		(type ,matrix-class A B C)
		(type symbol job))
       (mlet* (((job-a job-b) (ecase job
				(:nn (values :n :n))
				(:nt (values :n :t))
				(:tn (values :t :n))
				(:tt (values :t :t)))
		:type (symbol symbol))
	       ((maj-a ld-a fop-a) (blas-matrix-compatible-p A job-a) :type (symbol index-type (string 1)))
	       ((maj-b ld-b fop-b) (blas-matrix-compatible-p B job-b) :type (symbol index-type (string 1)))
	       ((maj-c ld-c fop-c) (blas-matrix-compatible-p C :n) :type (symbol index-type nil)))
	      (let ((call-fortran? (> (max (nrows C) (ncols C) (if (eq job-a :n) (ncols A) (nrows A)))
				      *gemm-fortran-call-lower-bound*)))
		(cond
		  ((and maj-a maj-b maj-c call-fortran?)
		   (let-typed ((nr-c (nrows C) :type index-type)
			       (nc-c (ncols C) :type index-type)
			       (dotl (ecase job-a (:n (ncols A)) (:t (nrows A))) :type index-type))
			      (when (eq maj-c :row-major)
				(rotatef A B)
				(rotatef ld-a ld-b)
				(rotatef maj-a maj-b)
				(rotatef nr-c nc-c)
				(setf (values fop-a fop-b)
				      (values (fortran-snop fop-b) (fortran-snop fop-a))))
			      (,blas-gemm-func fop-a fop-b nr-c nc-c dotl
					       alpha (store A) ld-a (store B) ld-b
					       beta (store C) ld-c
					       (head A) (head B) (head C))))
		  ((and maj-a call-fortran?)
		   (let-typed ((nc-c (ncols C) :type index-type)
			       (sto-c (store C) :type ,(linear-array-type (getf opt :store-type)))
			       (stp-c (row-stride C) :type index-type)
			       (nr-a (nrows A) :type index-type)
			       (nc-a (ncols A) :type index-type)
			       (sto-a (store A) :type ,(linear-array-type (getf opt :store-type)))
			       (hd-a (head A) :type index-type)
			       (stp-b (if (eq job-b :n) (row-stride B) (col-stride B)) :type index-type)
			       (sto-b (store B) :type ,(linear-array-type (getf opt :store-type)))
			       (strd-b (if (eq job-b :n) (col-stride B) (row-stride B)) :type index-type)
			       (strd-c (col-stride C) :type index-type))
			      (when (eq maj-a :row-major)
				(rotatef nr-a nc-a))
			      (very-quickly
				(mod-dotimes (idx (idxv nc-c))
				  with (linear-sums
					(of-b (idxv strd-b) (head B))
					(of-c (idxv strd-c) (head C)))
				  do (,blas-gemv-func fop-a nr-a nc-a
						      alpha sto-a ld-a
						      sto-b stp-b
						      beta sto-c stp-c
						      hd-a of-b of-c)))))
		  ((and maj-b call-fortran?)
		   (let-typed ((nr-c (nrows C) :type index-type)
			       (stp-c (col-stride C) :type index-type)
			       (sto-c (store c) :type ,(linear-array-type (getf opt :store-type)))
			       (stp-a (if (eq job-a :n) (col-stride A) (row-stride A)) :type index-type)
			       (sto-a (store A) :type ,(linear-array-type (getf opt :store-type)))
			       (nr-b (nrows B) :type index-type)
			       (nc-b (ncols B) :type index-type)
			       (hd-b (head B) :type index-type)
			       (fop-b (fortran-snop fop-b) :type (string 1))
			       (sto-b (store B) :type ,(linear-array-type (getf opt :store-type)))
			       (strd-a (if (eq job-A :n) (row-stride A) (col-stride A)) :type index-type)
			       (strd-c (row-stride C) :type index-type))
			      (when (eq maj-b :row-major)
				(rotatef nr-b nc-b))
			      (very-quickly
				(mod-dotimes (idx (idxv nr-c))
				  with (linear-sums
					(of-A (idxv strd-a) (head A))
					(of-c (idxv strd-c) (head C)))
				  do (,blas-gemv-func fop-b nr-b nc-b
						      alpha sto-b ld-b
						      sto-a stp-a
						      beta sto-c stp-c
						      hd-b of-a of-c)))))
		  (t
		   (let-typed ((dotl (ecase job-a (:n (ncols A)) (:t (nrows A))) :type index-type)
			       (rstp-a (row-stride A) :type index-type)
			       (cstp-a (col-stride A) :type index-type)
			       (rstp-b (row-stride A) :type index-type)
			       (cstp-b (col-stride A) :type index-type)
			       (sto-a (store A) :type ,(linear-array-type (getf opt :store-type)))
			       (sto-b (store B) :type ,(linear-array-type (getf opt :store-type)))
			       (sto-c (store C) :type ,(linear-array-type (getf opt :store-type))))
			      (when (eq job-a :t)
				(rotatef rstp-a cstp-a))
			      (when (eq job-b :t)
				(rotatef rstp-b cstp-b))
			      (very-quickly
				(mod-dotimes (idx (dimensions C))
				  with (loop-order :row-major)
				  with (linear-sums
					(of-a (idxv rstp-a 0) (head A))
					(of-b (idxv 0 cstp-b) (head B))
					(of-c (strides C) (head C)))
				  do (let-typed ((val (* beta ,(funcall (getf opt :reader) 'sto-c 'of-c)) :type ,(getf opt :element-type)))
						(loop repeat dotl
						   for dof-a of-type index-type = of-a then (+ dof-a cstp-a)
						   for dof-b of-type index-type = of-b then (+ dof-b rstp-b)
						   summing (* ,(funcall (getf opt :reader) 'sto-a 'dof-a)
							      ,(funcall (getf opt :reader) 'sto-b 'dof-b)) into tmp of-type ,(getf opt :element-type)
						   finally ,(funcall (getf opt :value-writer) '(+ (* alpha tmp) val) 'sto-c 'of-c))))))))))
       C)))

(generate-typed-gemm! real-typed-gemm! (real-matrix) (dgemm dgemv))
(generate-typed-gemm! complex-typed-gemm! (complex-matrix) (zgemm zgemv))
;;---------------------------------------------------------------;;

(defgeneric gemm! (alpha A B beta C &optional job)
  (:documentation
"
  Syntax
  ======
  (GEMM! alpha a b beta c [job])

  Purpose
  =======
  Performs the GEneral Matrix Multiplication given by
               --      -      -

            C <- alpha * op(A) * op(B) + beta * C
     
  and returns C.

  alpha,beta are scalars and A,B,C are matrices.
  op(A) means either A or A'.

     JOB                    Operation
  ---------------------------------------------------
     :NN (default)      alpha * A * B + beta * C
     :TN                alpha * A'* B + beta * C
     :NT                alpha * A * B'+ beta * C
     :TT                alpha * A'* B'+ beta * C    
")
  (:method :before ((alpha number) (A standard-matrix) (B standard-matrix)
		    (beta number) (C standard-matrix)
		    &optional (job :nn))
  (let ((nr-a (nrows A))
	(nc-a (ncols A))
	(nr-b (nrows B))
	(nc-b (ncols B))
	(nr-c (nrows C))
	(nc-c (ncols C)))
    (declare (type index-type nr-a nc-a nr-b nc-b nr-c nc-c))
    (case job
      (:nn t)
      (:tn (rotatef nr-a nc-a))
      (:nt (rotatef nr-b nc-b))
      (:tt (rotatef nr-a nc-a) (rotatef nr-b nc-b))
      (t (error 'invalid-value :given job :expected '(member job '(:nn :tn :nt :tt)))))
    (assert (not (or (eq A C) (eq B C))) nil 'invalid-arguments
	    :message "GEMM!: C = {A or B} is not allowed.")
    (assert (and (= nr-c nr-a)
		 (= nc-a nr-b)
		 (= nc-b nc-c)) nil 'tensor-dimension-mismatch))))

(defmethod gemm! ((alpha number) (a real-matrix) (b real-matrix)
		  (beta number) (c real-matrix)
		  &optional (job :nn))
  (real-typed-gemm! (coerce-real alpha) a b
		    (coerce-real beta) c job))

(defmethod gemm! ((alpha number) (a complex-matrix) (b complex-matrix)
		  (beta number) (c complex-matrix)
		  &optional (job :nn))
  (complex-typed-gemm! (coerce-complex alpha) a b
		       (coerce-complex beta) c job))

(defmethod gemm! ((alpha number) (a real-matrix) (b real-matrix)
		  (beta number) (c complex-matrix)
		  &optional (job :nn))
  (unless (= beta 1)
    (scal! beta c))
  (unless (= alpha 0)
    (if (complexp alpha)
	(let ((A.x (make-real-tensor (nrows c) (ncols c)))
	      (vw.c (tensor-realpart~ c)))
	  (real-typed-gemm! (coerce-real 1) A B (coerce-real 0) A.x job)
	  ;;Re
	  (axpy! (realpart alpha) A.x vw.c)
	  ;;Im 
	  (incf (head vw.c))
	  (axpy! (imagpart alpha) A.x vw.c))
	(let ((vw.c (tensor-realpart~ c)))
	  (real-typed-gemm! (coerce-real alpha) A B
			    (coerce-real 1) vw.c job))))
  C)

(defmethod gemm! ((alpha number) (a real-matrix) (b complex-matrix)
		  (beta number) (c complex-matrix)
		  &optional (job :nn))
  (let ((A.cplx (copy! A (make-complex-tensor (nrows a) (ncols a)))))
    (complex-typed-gemm! (coerce-complex alpha) A.cplx B
			 (coerce-complex beta) C job))
  C)

(defmethod gemm! ((alpha number) (a complex-matrix) (b real-matrix)
		  (beta number) (c complex-matrix)
		  &optional (job :nn))
  (let ((B.cplx (copy! B (make-complex-tensor (nrows B) (ncols B)))))
    (complex-typed-gemm! (coerce-complex alpha) A B.cplx
			 (coerce-complex beta) C job))
  C)

;;---------------------------------------------------------------;;
(defgeneric gemm (alpha a b beta c &optional job)
  (:documentation
"
  Syntax
  ======
  (GEMM alpha a b beta c [job])

  Purpose
  =======
  Performs the GEneral Matrix Multiplication given by
               --      -      -

             alpha * op(A) * op(B) + beta * C
     
  and returns the result in a new matrix.

  alpha,beta are scalars and A,B,C are matrices.
  op(A) means either A or A'.

     JOB                    Operation
  ---------------------------------------------------
     :NN (default)      alpha * A * B + beta * C
     :TN                alpha * A'* B + beta * C
     :NT                alpha * A * B'+ beta * C
     :TT                alpha * A'* B'+ beta * C
"))

(defmethod gemm ((alpha number) (a standard-matrix) (b standard-matrix)
		 (beta number) (c real-matrix)
		 &optional (job :nn))
  (let ((result (copy C)))
    (gemm! alpha A B beta result job)))

;; if all args are not real then at least one of them
;; is complex, so we need to call GEMM! with a complex C
(defmethod gemm ((alpha number) (a standard-matrix) (b standard-matrix)
		 (beta number) (c standard-matrix)
		 &optional (job :nn))
  (let ((result (if (or (complexp alpha) (complexp beta)
			(typep a 'complex-matrix) (typep b 'complex-matrix))
		    (make-complex-tensor (nrows C) (ncols C))
		    (make-real-tensor (nrows C) (ncols C)))))
    (copy! C result)
    (gemm! alpha A B beta result job)))
