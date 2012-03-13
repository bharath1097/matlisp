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
;;; Originally written by Raymond Toy
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id: axpy.lisp,v 1.8 2011/01/25 18:36:56 rtoy Exp $
;;;
;;; $Log: axpy.lisp,v $
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
;;; Revision 1.6  2003/02/14 05:42:11  rtoy
;;; Undo previous change.  We really need the 1x1-complex-array for
;;; Allegro because we don't (currently) pass in complex double-floats as
;;; an array.  (Not needed for CMUCL which handles this correctly.)
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

(defmacro generate-typed-axpy!-func (func element-type store-type matrix-type blas-func)
  `(defun ,func (alpha mat-a mat-b)
     (declare (type ,element-type alpha)
	      (type ,matrix-type mat-a mat-b)
	      (optimize (safety 0) (speed 3)))
     (mlet* (((cp-a inc-a sz-a) (blas-copyable-p mat-a) :type (boolean fixnum nil))
	     ((cp-b inc-b sz-b) (blas-copyable-p mat-b) :type (boolean fixnum nil))
	     ((hd-a st-a sz) (slot-values mat-a '(head store number-of-elements)) :type (fixnum (,store-type *) fixnum))
	     ((hd-b st-b) (slot-values mat-b '(head store)) :type (fixnum (,store-type *))))
	    (if (and cp-a cp-b)
		(,blas-func sz alpha st-a inc-a st-b inc-b :head-x hd-a :head-y hd-b)
		(symbol-macrolet
		    ((common-code
		      (mlet* (((nr-a nc-a rs-a cs-a) (slot-values mat-a '(number-of-rows number-of-cols row-stride col-stride))
			       :type (fixnum fixnum fixnum fixnum))
			      ((rs-b cs-b) (slot-values mat-b '(row-stride col-stride))
			       :type (fixnum fixnum)))
			     (loop for i from 0 below nr-a
				do (,blas-func nc-a alpha st-a cs-a st-b cs-b :head-x (+ hd-a (* i rs-a)) :head-y (+ hd-b (* i rs-b)))))))
		  ;;Choose the smaller of the loops
		  (if (> (nrows mat-a) (ncols mat-a))
		      (with-transpose! (mat-a mat-b)
			common-code)
		      common-code)))
	    mat-b)))

;;
(defgeneric axpy! (alpha x y)
  (:documentation
 " 
 Syntax
 ======
 (AXPY! alpha x y)

 Y <- alpha * x + y

 Purpose
 =======
  Same as AXPY except that the result
  is stored in Y and Y is returned.
"))

(defmethod axpy! :before ((alpha number) (x standard-matrix) (y standard-matrix))
  (mlet* (((nr-x nc-x) (slot-values x '(number-of-rows number-of-cols)) :type (fixnum fixnum))
	  ((nr-y nc-y) (slot-values y '(number-of-rows number-of-cols)) :type (fixnum fixnum)))
	 (unless (and (= nr-x nr-y) (= nc-x nc-y))
	   (error "Arguments X,Y to AXPY! are of different dimensions."))))

;;
(generate-typed-axpy!-func real-double-axpy!-typed double-float real-matrix-store-type real-matrix blas:daxpy)

(defmethod axpy! ((alpha number) (x complex-matrix) (y real-matrix))
  (error "cannot AXPY! a complex X to a real Y,
don't know how to coerce COMPLEX to REAL"))

(defmethod axpy! ((alpha cl:real) (x real-matrix) (y real-matrix))
  (real-double-axpy!-typed (coerce alpha 'double-float) x y))

;;
(generate-typed-axpy!-func complex-double-axpy!-typed complex-double-float complex-matrix-store-type complex-matrix blas:zaxpy)

(defmethod axpy! ((alpha cl:real) (x real-matrix) (y complex-matrix))
  (real-double-axpy!-typed (coerce alpha 'double-float) x (realpart! y)))

(defmethod axpy! ((alpha complex) (x real-matrix) (y complex-matrix))
  (real-double-axpy!-typed (coerce (realpart alpha) 'double-float) x (realpart! y))
  (real-double-axpy!-typed (coerce (imagpart alpha) 'double-float) x (imagpart! y)))

(defmethod axpy! ((alpha number) (x complex-matrix) (y complex-matrix))
  (complex-double-axpy!-typed (complex-coerce alpha) x y))

;;;;
(defgeneric axpy (alpha x y)
  (:documentation
 "
 Syntax
 ======
 (AXPY alpha x y)

 Purpose
 =======
 Computes  
      
                 ALPHA * X + Y

 where ALPHA is a scalar and X,Y are
 matrices.

 The result is stored in a new matrix 
 that has the same dimensions as Y.

 X,Y need not have the same dimensions,
 but must have the same total number of 
 elements.  Practically, this is useful
 for adding a row and column vector of 
 the same size etc ...
"))

(defmethod axpy :before ((alpha number) (x standard-matrix) (y standard-matrix))
  (mlet* (((nr-x nc-x) (slot-values x '(number-of-rows number-of-cols)) :type (fixnum fixnum))
	  ((nr-y nc-y) (slot-values y '(number-of-rows number-of-cols)) :type (fixnum fixnum)))
	 (unless (and (= nr-x nr-y) (= nc-x nc-y))
	   (error "Arguments X,Y to AXPY are of different dimensions."))))

;;
(defmethod axpy ((alpha cl:real) (x real-matrix) (y real-matrix))
  (let ((result (copy y)))
    (axpy! alpha x result)))

(defmethod axpy ((alpha complex) (x real-matrix) (y real-matrix))
  (let ((result (scal alpha x)))
    (axpy! 1d0 y result)))

(defmethod axpy ((alpha number) (x complex-matrix) (y real-matrix))
  (let ((result (scal alpha x)))
    (axpy! 1d0 y result)))

;;
(defmethod axpy ((alpha number) (x real-matrix) (y complex-matrix))
  (let ((result (copy y)))
    (axpy! alpha x result)))

(defmethod axpy ((alpha number) (x complex-matrix) (y complex-matrix))
  (let ((result (copy y)))
    (axpy! alpha x result)))