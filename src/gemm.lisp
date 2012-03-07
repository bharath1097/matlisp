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

;; Why write things again and again, when Lisp will gladly do it for you :)
(defmacro generate-typed-gemm!-func (func element-type matrix-type blas-func)
  `(defun ,func (alpha a b beta c job)
     (declare (type ,element-type alpha beta)
	      (type ,matrix-type a b c)
	      (type symbol job))
     (mlet ((n (nrows c)
	       :declare ((type fixnum n)))
	    (m (ncols c)
	       :declare ((type fixnum m)))
	    (k (if (member job '(:nn :nt))
		   (ncols a)
		   (nrows a))
	       :declare ((type fixnum k)))
	    (order-a lda job-a (ecase job
				 ((:nn :nt) (get-order-stride a "N"))
				 ((:tn :tt) (get-order-stride a "T")))
		     :declare ((ignore order-a)
			       (type fixnum lda)
			       (type (string 1) job-a)))
	    (order-b ldb job-b (ecase job
				 ((:nn :tn) (get-order-stride b "N"))
				 ((:nt :tt) (get-order-stride b "T")))
		     :declare ((ignore order-b)
			       (type fixnum ldb)
			       (type (string 1) job-b)))
	    (order-c ldc job-c (get-order-stride c "N")
		     :declare ((ignore order-c)
			       (type fixnum ldc)
			       (type (string 1) job-c))))
	   
	   (when (string= job-c "T")
	     (rotatef a b)
	     (rotatef lda ldb)
	     (rotatef n m)
	     (rotatef job-a job-b)
	     ;;
	     (setf job-a (cond
			   ((string= "N" job-a) "T")
			   ((string= "T" job-a) "N")
			   (t "N")))
	     (setf job-b (cond
			   ((string= "N" job-b) "T")
			   ((string= "T" job-b) "N")
			   (t "N"))))
	   
	   (,blas-func job-a     ; TRANSA
		       job-b     ; TRANSB
		       n         ; M
		       m         ; N (LAPACK takes N M opposite our convention)
		       k         ; K
		       alpha     ; ALPHA
		       (store a) ; A
		       lda       ; LDA
		       (store b) ; B
		       ldb       ; LDB
		       beta      ; BETA
		       (store c) ; C
		       ldc       ; LDC
		       :inc-a (head a) :inc-b (head b) :inc-c (head c))
	   c)))

;;
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

(defmethod gemm! :before ((alpha number)
			  (a standard-matrix)
			  (b standard-matrix)
			  (beta number)
			  (c standard-matrix)
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
(generate-typed-gemm!-func real-double-gemm!-typed real-matrix-element-type real-matrix dgemm)

(defmethod gemm! ((alpha cl:real) (a real-matrix) (b real-matrix)
		  (beta cl:real) (c real-matrix) &optional (job :nn))
  (real-double-gemm!-typed (coerce alpha 'real-matrix-element-type) a b
			   (coerce beta 'real-matrix-element-type) c
			   job))

;;

(generate-typed-gemm!-func complex-double-gemm!-typed (complex (double-float * *)) complex-matrix zgemm)

(defmethod gemm! ((alpha number) (a complex-matrix) (b complex-matrix)
		  (beta number) (c complex-matrix) &optional (job :nn))
  (complex-double-gemm!-typed (complex-coerce alpha) a b
			      (complex-coerce beta) c job))

;;
(defmethod gemm! ((alpha number) 
		  (a standard-matrix)
		  (b standard-matrix)
		  (beta number) 
		  (c complex-matrix) 
		  &optional (job :NN))

  (let ((a (typecase a
	     (real-matrix (copy! a (make-complex-matrix-dim (nrows a) (ncols a))))
	     (complex-matrix a)
	     (t (error "argument A given to GEMM! is not a REAL-MATRIX or COMPLEX-MATRIX"))))
	(b (typecase b
	     (real-matrix (copy! b (make-complex-matrix-dim (nrows b) (ncols b))))
	     (complex-matrix b)
	     (t (error "argument B given to GEMM! is not a REAL-MATRIX or COMPLEX-MATRIX")))))

    (gemm! (complex-coerce alpha)
	   a
	   b
	   (complex-coerce beta)
	   c
	   job)))


;;;;;
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


(defmethod gemm :before ((alpha number) 
			 (a standard-matrix) 
			 (b standard-matrix)
			 (beta number) 
			 (c standard-matrix) 
			 &optional (job :NN))
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


(defmethod gemm ((alpha cl:real) 
		 (a real-matrix) 
		 (b real-matrix)
		 (beta cl:real) 
		 (c real-matrix) 
		 &optional (job :nn))

  (gemm! (coerce alpha 'real-matrix-element-type)
	 a
	 b
	 (coerce beta 'real-matrix-element-type)
	 (copy c)
	 job))


;; if all args are not real then at least one of them
;; is complex, so we need to call GEMM! with a complex C
(defmethod gemm ((alpha number) 
		 (a standard-matrix) 
		 (b standard-matrix)
		 (beta number) 
		 (c standard-matrix) 
		 &optional (job :NN))

  (let	((c (typecase c
	     (real-matrix (copy! c (make-complex-matrix-dim (nrows c) (ncols c))))
	     (complex-matrix (copy c))
	     (t (error "argument C given to GEMM is not a REAL-MATRIX or COMPLEX-MATRIX")))))

    (gemm! (complex-coerce alpha)
	   a
	   b
	   (complex-coerce beta)
	   c
	   job)))