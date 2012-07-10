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
	      (if (and maj-a maj-b maj-c)
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
					      (head A) (head B) (head C)))
		  (cond
		    (maj-a
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
		    (maj-b
		     (let-typed ((nr-c (nrows C) :type index-type)
				 (stp-c (col-stride C) :type index-type)
				 (sto-c (store c) :type ,(linear-array-type (getf opt :store-type)))
				 (stp-a (if (eq job-a :n) (col-stride B) (row-stride B)) :type index-type)
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
					  (of-a (idxv rstp-a 0) (head A)) ; cstp-a))
					  (of-b (idxv 0 cstp-b) (head B)) ; rstp-b))
					  (of-c (strides C) (head C)))    ; 0)))
				    do (let-typed ((tmp (,(getf opt :coercer) 0) :type ,(getf opt :element-type))
						   (val (* beta ,(funcall (getf opt :reader) 'sto-c 'of-c)) :type ,(getf opt :element-type)))
						  (loop repeat dotl
						     for dof-a of-type index-type = of-a then (+ dof-a cstp-a)
						     for dof-b of-type index-type = of-b then (+ dof-b rstp-b)
						     do (incf tmp (* ,(funcall (getf opt :reader) 'sto-a 'dof-a)
								     ,(funcall (getf opt :reader) 'sto-b 'dof-b))))
						  ,(funcall (getf opt :value-writer) '(+ (* alpha tmp) (* beta val)) 'sto-c 'of-c)))))))))
       C)))

(generate-typed-gemm! real-typed-gemm! (real-matrix) (dgemm dgemv))
(generate-typed-gemm! complex-typed-gemm! (complex-matrix) (zgemm zgemv))

(let ((A (tensor-realpart~
	  (make-complex-tensor '((1 2 3)
				 (4 5 6)
				 (7 8 9)))))
      (C (make-real-tensor 3 3)))
  (real-typed-gemm! 1d0 A A 0d0 C :nn))

;;;;
(defgeneric gemm! (alpha a b beta c &optional job)
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

 Note
 ====
 Take caution when using GEMM! as follows:

           (GEMM! alpha a b beta b)

     or

          (GEMM! alpha a b beta a)

  The results may be unpredictable depending
  on the underlying DGEMM, ZGEMM routines
  from BLAS, ATLAS or LIBCRUFT.
"))

(defmethod gemm! :before ((alpha number) (a standard-matrix) (b standard-matrix)
			  (beta number) (c standard-matrix)
			  &optional (job :nn))
  (let ((n-a (nrows a))
	(m-a (ncols a))
	(n-b (nrows b))
	(m-b (ncols b))
	(n-c (nrows c))
	(m-c (ncols c)))
    (declare (type fixnum n-a m-a n-b m-b n-c m-c))
    (case job
      (:nn t)
      (:tn (rotatef n-a m-a))
      (:nt (rotatef n-b m-b))
      (:tt (rotatef n-a m-a) (rotatef n-b m-b))
      (t (error "argument JOB to GEMM! is not recognized")))

    (if (not (and (= m-a n-b)
		  (= n-a n-c)
		  (= m-b m-c)))
	(error "dimensions of A,B,C given to GEMM! do not match"))))

;;
(generate-typed-gemm!-func real-double-gemm!-typed
			   double-float real-matrix-store-type real-matrix
			   blas:dgemm real-double-gemv!-typed)

(defmethod gemm! ((alpha cl:real) (a real-matrix) (b real-matrix)
		  (beta cl:real) (c real-matrix)
		  &optional (job :nn))
  (real-double-gemm!-typed (coerce alpha 'double-float) a b
			   (coerce beta 'double-float) c
			   job))

;;
(generate-typed-gemm!-func complex-double-gemm!-typed
			   complex-double-float complex-matrix-store-type complex-matrix
			   blas:zgemm complex-double-gemv!-typed)

(defmethod gemm! ((alpha number) (a complex-matrix) (b complex-matrix)
		  (beta number) (c complex-matrix)
		  &optional (job :nn))
  (complex-double-gemm!-typed (complex-coerce alpha) a b
			      (complex-coerce beta) c job))

;
(defmethod gemm! ((alpha number) (a real-matrix) (b real-matrix)
		  (beta cl:real) (c complex-matrix)
		  &optional (job :nn))
  (let ((r-c (mrealpart~ c)))
    (declare (type real-matrix c))
    (gemm! alpha a b 0d0 r-c job))
  c)

(defmethod gemm! ((alpha number) (a real-matrix) (b real-matrix)
		  (beta complex) (c complex-matrix)
		  &optional (job :nn))
  (let ((r-c (mrealpart~ c))
	(c-be (complex-coerce beta)))
    (declare (type real-matrix c)
	     (type complex-double-float c-al))
    (scal! c-be c)
    (gemm! alpha a b 1d0 r-c job))
  c)

;
(defmethod gemm! ((alpha number) (a real-matrix) (b complex-matrix)
		  (beta complex) (c complex-matrix)
		  &optional (job :nn))
  (scal! (complex-coerce beta) c)
  (gemm! alpha a b 1d0 c job))

(defmethod gemm! ((alpha cl:real) (a real-matrix) (b complex-matrix)
		  (beta cl:real) (c complex-matrix)
		  &optional (job :nn))
  (let ((r-b (mrealpart~ b))
	(i-b (mimagpart~ b))
	(r-c (mrealpart~ c))
	(i-c (mimagpart~ c))
	(r-al (coerce alpha 'double-float))
	(r-be (coerce beta 'double-float)))
    (declare (type real-matrix r-b i-b r-c i-c)
	     (type double-float r-al r-be))
    (real-double-gemm!-typed r-al a r-b r-be r-c job)
    (real-double-gemm!-typed r-al a i-b r-be i-c job)))

(defmethod gemm! ((alpha complex) (a real-matrix) (b complex-matrix)
		  (beta cl:real) (c complex-matrix)
		  &optional (job :nn))
  (let ((r-b (mrealpart~ b))
	(i-b (mimagpart~ b))
	(r-c (mrealpart~ c))
	(i-c (mimagpart~ c))
	(r-al (coerce (realpart alpha) 'double-float))
	(i-al (coerce (imagpart alpha) 'double-float))
	(r-be (coerce beta 'double-float)))
    (declare (type real-matrix r-b i-b r-c i-c)
	     (type double-float r-al r-be))
    ;;
    (real-double-gemm!-typed r-al a r-b r-be r-c job)
    (real-double-gemm!-typed (- i-al) a i-b 1d0 r-c job)
    ;;
    (real-double-gemm!-typed r-al a i-b r-be i-c job)
    (real-double-gemm!-typed i-al a r-b 1d0 i-c job)))

;
(defmethod gemm! ((alpha number) (a complex-matrix) (b real-matrix)
		  (beta complex) (c complex-matrix)
		  &optional (job :nn))
  (scal! (complex-coerce beta) c)
  (gemm! alpha a b 1d0 c job))

(defmethod gemm! ((alpha cl:real) (a complex-matrix) (b real-matrix)
		  (beta cl:real) (c complex-matrix)
		  &optional (job :nn))
  (let ((r-a (mrealpart~ a))
	(i-a (mimagpart~ a))
	(r-c (mrealpart~ c))
	(i-c (mimagpart~ c))
	(r-al (coerce alpha 'double-float))
	(r-be (coerce beta 'double-float)))
    (declare (type real-matrix r-a i-a r-c i-c)
	     (type double-float r-al r-be))
    (real-double-gemm!-typed r-al r-a b r-be r-c job)
    (real-double-gemm!-typed r-al i-a b r-be i-c job)))

(defmethod gemm! ((alpha complex) (a complex-matrix) (b real-matrix)
		  (beta cl:real) (c complex-matrix)
		  &optional (job :nn))
  (let ((r-a (mrealpart~ a))
	(i-a (mimagpart~ a))
	(r-c (mrealpart~ c))
	(i-c (mimagpart~ c))
	(r-al (coerce (realpart alpha) 'double-float))
	(i-al (coerce (imagpart alpha) 'double-float))
	(r-be (coerce beta 'double-float)))
    (declare (type real-matrix r-a i-a r-c i-c)
	     (type double-float r-al r-be))
    ;;
    (real-double-gemm!-typed r-al r-a b r-be r-c job)
    (real-double-gemm!-typed (- i-al) i-a b 1d0 r-c job)
    ;;
    (real-double-gemm!-typed r-al i-a b r-be i-c job)
    (real-double-gemm!-typed i-al r-a b 1d0 i-c job)))

;;;;
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

;;
(defmethod gemm :before ((alpha number) (a standard-matrix) (b standard-matrix)
			 (beta number) (c standard-matrix)
			 &optional (job :nn))
  (let ((n-a (nrows a))
	(m-a (ncols a))
	(n-b (nrows b))
	(m-b (ncols b))
	(n-c (nrows c))
	(m-c (ncols c)))
    (declare (type fixnum n-a m-a n-b m-b n-c m-c))

    (case job
      (:nn t)
      (:tn (rotatef n-a m-a))
      (:nt (rotatef n-b m-b))
      (:tt (rotatef n-a m-a) (rotatef n-b m-b))
      (t (error "argument JOB to GEMM! is not recognized")))

    (if (not (and (= m-a n-b)
		  (= n-a n-c)
		  (= m-b m-c)))
	(error "dimensions of A,B,C given to GEMM! do not match"))))

;; if all args are not real then at least one of them
;; is complex, so we need to call GEMM! with a complex C
(defmethod gemm ((alpha number) (a standard-matrix) (b standard-matrix)
		 (beta number) (c standard-matrix)
		 &optional (job :nn))
  (let ((result (scal (if (or (typep alpha 'complex) (typep a 'complex-matrix)
			      (typep b 'complex-matrix) (typep beta 'complex))
			  (complex-coerce beta)
			  beta)
		      c)))
    (gemm! alpha a b 1d0 result job)))
