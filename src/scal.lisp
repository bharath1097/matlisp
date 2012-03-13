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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Originally written by Raymond Toy
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id: scal.lisp,v 1.6 2011/01/25 18:36:56 rtoy Exp $
;;;
;;; $Log: scal.lisp,v $
;;; Revision 1.6  2011/01/25 18:36:56  rtoy
;;; Merge changes from automake-snapshot-2011-01-25-1327 to get the new
;;; automake build infrastructure.
;;;
;;; Revision 1.5.2.1  2011/01/25 18:16:53  rtoy
;;; Use cl:real instead of real.
;;;
;;; Revision 1.5  2004/05/24 16:34:22  rtoy
;;; More SBCL support from Robert Sedgewick.  The previous SBCL support
;;; was incomplete.
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

(defmacro generate-typed-scal!-func (func element-type store-type matrix-type blas-func)
  `(defun ,func (alpha mat-x)
     (declare (type ,matrix-type mat-x)
	      (type ,element-type alpha)
	      (optimize (safety 0) (speed 3)))
     (mlet* (((cp-x inc-x sz-x) (blas-copyable-p mat-x)
	      :type (boolean fixnum fixnum))
	     ((hd-x st-x) (slot-values mat-x '(head store))
	      :type (fixnum (,store-type *))))
	    (if cp-x
		(,blas-func sz-x alpha st-x inc-x :head-x hd-x)
		(symbol-macrolet
		    ((common-code
		      (mlet* (((nr-x nc-x rs-x cs-x) (slot-values mat-x '(number-of-rows number-of-cols row-stride col-stride))
			       :type (fixnum fixnum fixnum fixnum)))
			     (loop for i from 0 below nr-x
				do (,blas-func nc-x alpha st-x cs-x :head-x (+ hd-x (* i rs-x)))))))
		  (if (> (nrows mat-x) (ncols mat-x))
		      (with-transpose! (mat-x)
			common-code)
		      common-code)))
	    mat-x)))

;;
(defgeneric scal! (alpha x)
  (:documentation
"
  Syntax
  ======
  (SCAL! alpha x)

  Purpose
  =======
  Same as SCAL except that the result is
  stored in X.
"))

;;
(generate-typed-scal!-func real-double-dscal!-typed double-float real-matrix-store-type real-matrix blas:dscal)

(defmethod scal! ((alpha number) (x number))
  (error "Cannot SCAL! two scalars, arg X must 
be a matrix to SCAL!"))

(defmethod scal! ((alpha complex) (x real-matrix))
  (error "Cannot SCAL! a REAL-MATRIX by a COMPLEX, don't know
how to coerce COMPLEX to REAL"))

(defmethod scal! ((alpha cl:real) (x real-matrix))
  (real-double-dscal!-typed (coerce alpha 'double-float) x))

;;
(generate-typed-scal!-func complex-double-dscal!-typed double-float complex-matrix-store-type complex-matrix blas:zdscal)

(generate-typed-scal!-func complex-double-zscal!-typed complex-double-float complex-matrix-store-type complex-matrix blas:zscal)

(defmethod scal! ((alpha cl:real) (x complex-matrix))
  (complex-double-dscal!-typed (coerce alpha 'double-float) x))

(defmethod scal! ((alpha complex) (x complex-matrix))
  (complex-double-zscal!-typed (complex-coerce alpha) x))

;;;;
(defgeneric scal (alpha x)
  (:documentation
"
  Syntax
  ======
  (SCAL alpha x)

  Purpose
  =======
  Computes and returns a new matrix equal to

             alpha * X

  where alpha is a scalar and X is a matrix.

"))

(defmethod scal ((alpha number) (x number))
  (* alpha x))

;;
(defmethod scal ((alpha cl:real) (x real-matrix))
  (let ((result (copy x)))
    (scal! alpha result)))

(defmethod scal ((alpha complex) (x real-matrix))
  (let* ((n (nrows x))
	 (m (ncols x))
	 (result (make-complex-matrix-dim n m)))
    (declare (type fixnum n m))
    (copy! x result)
    (scal! alpha result)))

;;
(defmethod scal ((alpha number) (x complex-matrix))
  (let ((result (copy x)))
    (scal! alpha result)))