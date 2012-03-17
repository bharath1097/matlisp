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
;;;
;;; Originally written by Raymond Toy.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id: gemm.lisp,v 1.8 2011/01/25 18:36:56 rtoy Exp $
;;;
;;; $Log: gemm.lisp,v $
;;; Revision 1.8  2011/01/25 18:36:56  rtoy
;;; Merge changes from automake-snapshot-2011-01-25-1327 to get the new
;;; automake build infrastructure.
;;;
;;; Revision 1.7.2.1  2011/01/25 18:16:53  rtoy
;;; Use cl:real instead of real.
;;;
;;; Revision 1.7  2004/05/24 16:34:22  rtoy
;;; More SBCL support from Robert Sedgewick.  The previous SBCL support
;;; was incomplete.
;;;
;;; Revision 1.6  2001/06/22 12:52:41  rtoy
;;; Use ALLOCATE-REAL-STORE and ALLOCATE-COMPLEX-STORE to allocate space
;;; instead of using the error-prone make-array.
;;;
;;; Revision 1.5  2001/02/26 17:44:54  rtoy
;;; Remove the complex-alpha,beta special variables.  (Make a closure out
;;; of them.)
;;;
;;; Revision 1.4  2000/07/11 18:02:03  simsek
;;; o Added credits
;;;
;;; Revision 1.3  2000/07/11 02:11:56  simsek
;;; o Added support for Allegro CL
;;;
;;; Revision 1.2  2000/05/08 17:19:18  rtoy
;;; Changes to the STANDARD-MATRIX class:
;;; o The slots N, M, and NXM have changed names.
;;; o The accessors of these slots have changed:
;;;      NROWS, NCOLS, NUMBER-OF-ELEMENTS
;;;   The old names aren't available anymore.
;;; o The initargs of these slots have changed:
;;;      :nrows, :ncols, :nels
;;;
;;; Revision 1.1  2000/04/14 00:11:12  simsek
;;; o This file is adapted from obsolete files 'matrix-float.lisp'
;;;   'matrix-complex.lisp' and 'matrix-extra.lisp'
;;; o Initial revision.
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MATLISP")

(defmacro generate-typed-gemm!-func (func element-type store-type matrix-type blas-gemm-func lisp-gemv-func)
  `(defun ,func (alpha a b beta c job)
     (declare (optimize (safety 0) (speed 3))
	      (type ,element-type alpha beta)
	      (type ,matrix-type a b c)
	      (type symbol job))
     (mlet* ((job-a (ecase job ((:nn :nt) :n) ((:tn :tt) :t)) :type symbol)
	     (job-b (ecase job ((:nn :tn) :n) ((:nt :tt) :t)) :type symbol)
	     ((hd-c nr-c nc-c st-c) (slot-values c '(head number-of-rows number-of-cols store))
	      :type (fixnum fixnum fixnum (,store-type *)))
	     ((hd-a st-a) (slot-values a '(head store))
	      :type (fixnum (,store-type *)))
	     ((hd-b st-b) (slot-values b '(head store))
	      :type (fixnum (,store-type *)))
	     (k (if (eq job-a :n)
		    (ncols a)
		    (nrows a))
		:type fixnum)
	     ((order-a lda fort-job-a) (blas-matrix-compatible-p a job-a)
	      :type (symbol fixnum (string 1)))
	     ((order-b ldb fort-job-b) (blas-matrix-compatible-p b job-b)
	      :type (symbol fixnum (string 1)))
	     ((order-c ldc fort-job-c) (blas-matrix-compatible-p c :n)
	      :type (nil fixnum (string 1))))
	    ;;
	    (if (and (> lda 0) (> ldb 0) (> ldc 0))
		(progn
		  (when (string= fort-job-c "T")
		    (rotatef a b)
		    (rotatef lda ldb)
		    (rotatef fort-job-a fort-job-b)
		    (rotatef hd-a hd-b)
		    (rotatef st-a st-b)
		    (rotatef nr-c nc-c)
		    ;;
		    (setf fort-job-a (fortran-string-nop fort-job-a))
		    (setf fort-job-b (fortran-string-nop fort-job-b)))
		  (,blas-gemm-func fort-job-a fort-job-b
				   nr-c nc-c k
				   alpha
				   st-a lda
				   st-b ldb
				   beta
				   st-c ldc
				   :head-a hd-a :head-b hd-b :head-c hd-c))
	     (progn
	       (when (eq job-a :t) (transpose-i! a))
	       (when (eq job-b :t) (transpose-i! b))
	       ;;
	       (symbol-macrolet
		   ((loop-col
		       (mlet* ((cs-b (col-stride b) :type fixnum)
			       (cs-c (col-stride c) :type fixnum)
			       (col-b (col! b 0) :type ,matrix-type)
			       (col-c (col! c 0) :type ,matrix-type))
			      (dotimes (j nc-c)
				(when (> j 0)
				  (setf (head col-b) (+ (head col-b) cs-b))
				  (setf (head col-c) (+ (head col-c) cs-c)))
				(,lisp-gemv-func alpha a col-b beta col-c :n))))
		    (loop-row
		       (mlet* ((rs-a (row-stride a) :type fixnum)
			       (rs-c (row-stride c) :type fixnum)
			       (row-a (transpose-i! (row! a 0)) :type ,matrix-type)
			       (row-c (transpose-i! (row! c 0)) :type ,matrix-type))
			 (dotimes (i nr-c)
			   (when (> i 0)
			     (setf (head row-a) (+ (head row-a) rs-a))
			     (setf (head row-c) (+ (head row-c) rs-c)))
			   (,lisp-gemv-func alpha b row-a beta row-c :t)))))
		 (cond
		   (order-a loop-col)
		   (order-b loop-row)
		   ((< nr-c nc-c) loop-row)
		   (t loop-col)))
	       ;;
	       (when (eq job-a :t) (transpose-i! a))
	       (when (eq job-b :t) (transpose-i! b))
	       )))
     c))
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
(defmethod gemm! ((alpha number) (a real-matrix) (b complex-matrix)
		  (beta complex) (c complex-matrix)
		  &optional (job :nn))
  (scal! (complex-coerce beta) c)
  (gemm! alpha a b 1d0 c))

(defmethod gemm! ((alpha cl:real) (a real-matrix) (b complex-matrix)
		  (beta cl:real) (c complex-matrix)
		  &optional (job :nn))
  (let ((r-b (realpart! b))
	(i-b (imagpart! b))
	(r-c (realpart! c))
	(i-c (imagpart! c))
	(r-al (coerce alpha 'double-float))
	(r-be (coerce beta 'double-float)))
    (declare (type real-matrix r-b i-b r-c i-c)
	     (type double-float r-al r-be))
    (real-double-gemm!-typed r-al a r-b r-be r-c job)
    (real-double-gemm!-typed r-al a i-b r-be i-c job)))

(defmethod gemm! ((alpha complex) (a real-matrix) (b complex-matrix)
		  (beta cl:real) (c complex-matrix)
		  &optional (job :nn))
  (let ((r-b (realpart! b))
	(i-b (imagpart! b))
	(r-c (realpart! c))
	(i-c (imagpart! c))
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
  (gemm! alpha a b 1d0 c))

(defmethod gemm! ((alpha cl:real) (a complex-matrix) (b real-matrix)
		  (beta cl:real) (c complex-matrix)
		  &optional (job :nn))
  (let ((r-a (realpart! a))
	(i-a (imagpart! a))
	(r-c (realpart! c))
	(i-c (imagpart! c))
	(r-al (coerce alpha 'double-float))
	(r-be (coerce beta 'double-float)))
    (declare (type real-matrix r-a i-a r-c i-c)
	     (type double-float r-al r-be))
    (real-double-gemm!-typed r-al r-a r-b r-be r-c job)
    (real-double-gemm!-typed r-al i-a r-b r-be i-c job)))

(defmethod gemm! ((alpha complex) (a complex-matrix) (b real-matrix)
		  (beta cl:real) (c complex-matrix)
		  &optional (job :nn))
  (let ((r-a (realpart! a))
	(i-a (imagpart! a))
	(r-c (realpart! c))
	(i-c (imagpart! c))
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
    (gemm! alpha a b 1d0 c job)))