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
;;; $Id: copy.lisp,v 1.9 2011/01/25 18:36:56 rtoy Exp $
;;;
;;; $Log: copy.lisp,v $
;;; Revision 1.9  2011/01/25 18:36:56  rtoy
;;; Merge changes from automake-snapshot-2011-01-25-1327 to get the new
;;; automake build infrastructure.
;;;
;;; Revision 1.8.2.1  2011/01/25 18:16:53  rtoy
;;; Use cl:real instead of real.
;;;
;;; Revision 1.8  2004/05/24 16:34:22  rtoy
;;; More SBCL support from Robert Sedgewick.  The previous SBCL support
;;; was incomplete.
;;;
;;; Revision 1.7  2003/02/14 05:42:12  rtoy
;;; Undo previous change.  We really need the 1x1-complex-array for
;;; Allegro because we don't (currently) pass in complex double-floats as
;;; an array.  (Not needed for CMUCL which handles this correctly.)
;;;
;;; Revision 1.5  2001/10/29 16:23:10  rtoy
;;; COPY! was broken on CMUCL because FORTRAN-DSCAL is no longer
;;; exported.  Use the Allegro version.  From M. Koerber.
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

;;
(defmacro generate-typed-copy!-func (func store-type matrix-type blas-func)
  `(defun ,func (mat-a mat-b)
     (declare (type ,matrix-type mat-a mat-b)
	      (optimize (safety 0) (speed 3)))
     (mlet* (((cp-a inc-a sz-a) (blas-copyable-p mat-a) :type (boolean fixnum nil))
	     ((cp-b inc-b sz-b) (blas-copyable-p mat-b) :type (boolean fixnum nil))
	     ((hd-a st-a sz) (slot-values mat-a '(head store number-of-elements)) :type (fixnum (,store-type *) fixnum))
	     ((hd-b st-b) (slot-values mat-b '(head store)) :type (fixnum (,store-type *))))
	    (if (and cp-a cp-b)
		(,blas-func sz st-a inc-a st-b inc-b :head-x hd-a :head-y hd-b)
		(symbol-macrolet
		    ((common-code
		      (mlet* (((nr-a nc-a rs-a cs-a) (slot-values mat-a '(number-of-rows number-of-cols row-stride col-stride))
			       :type (fixnum fixnum fixnum fixnum))
			      ((rs-b cs-b) (slot-values mat-b '(row-stride col-stride))
			       :type (fixnum fixnum)))
			     (loop for i from 0 below nr-a
				do (,blas-func nc-a st-a cs-a st-b cs-b :head-x (+ hd-a (* i rs-a)) :head-y (+ hd-b (* i rs-b)))))))
		  ;;Choose the smaller of the loops
		  (if (> (nrows mat-a) (ncols mat-a))
		      (with-transpose! (mat-a mat-b)
			common-code)
		      common-code)))
	    mat-b)))

;;
(defmacro generate-typed-num-copy!-func (func element-type store-type matrix-type blas-func
					 array-decl)
  (let ((num-arg (car array-decl)))
    (destructuring-bind (var var-maker-form var-setf-form &key type) (cadr array-decl)
      `(mlet* (((,var) (,@var-maker-form) ,@(if type
						`(:type (,type)))))
	      (defun ,func (,num-arg mat-x)
		(declare (type ,element-type ,num-arg)
			 (type ,matrix-type mat-x)
			 (optimize (safety 0) (speed 3)))
		(,@var-setf-form)
		(mlet* (((cp-x inc-x sz-x) (blas-copyable-p mat-x) :type (boolean fixnum nil))
			((hd-x st-x sz) (slot-values mat-x '(head store number-of-elements)) :type (fixnum (,store-type *) fixnum)))
		       (if cp-x
			   (,blas-func sz ,var 0 st-x inc-x :head-y hd-x)
			   (symbol-macrolet
			       ((common-code
				 (mlet* (((nr-x nc-x rs-x cs-x) (slot-values mat-x '(number-of-rows number-of-cols row-stride col-stride))
					  :type (fixnum fixnum fixnum fixnum)))
					(loop for i from 0 below nr-x
					   do (,blas-func nc-x ,var 0 st-x cs-x :head-y (+ hd-x (* i rs-x)))))))
			     ;;Choose the smaller of the loops
			     (if (> (nrows mat-x) (ncols mat-x))
				 (with-transpose! (mat-x)
				   common-code)
				 common-code)))
		       mat-x))))))

;;
(defgeneric copy! (matrix new-matrix)
  (:documentation
   "
  Syntax
  ======
  (COPY! x y)

  Purpose
  =======
  Copies the contents of the matrix X to
  matrix Y, returns Y.

  X,Y need not have the same dimensions,
  nor the same number of elements.

  Furthermore, x may be a scalar, in which
  case Y is filled with X.

  The contents of X must be coercable to
  the type of Y.  For example,
  a COMPLEX-MATRIX cannot be copied to a
  REAL-MATRIX but the converse is possible.
"))

(defmethod copy! :before ((x standard-matrix) (y standard-matrix))
  (mlet* (((nr-x nc-x) (slot-values x '(number-of-rows number-of-cols)) :type (fixnum fixnum))
	  ((nr-y nc-y) (slot-values y '(number-of-rows number-of-cols)) :type (fixnum fixnum)))
	 (unless (and (= nr-x nr-y) (= nc-x nc-y))
	   (error "Arguments X,Y to COPY! are of different dimensions."))))

;;
(defmethod copy! ((x standard-matrix) (y standard-matrix))
  (mlet* (((nr-x nc-x) (slot-values x '(number-of-rows number-of-cols))
	   :type (fixnum fixnum)))
	 (dotimes (i nr-x)
	   (dotimes (j nc-x)
	     (declare (type fixnum i j))
	     (setf (matrix-ref-2d y i j) (matrix-ref-2d x i j))))
    y))

;;
(generate-typed-copy!-func real-double-copy!-typed real-matrix-store-type real-matrix blas:dcopy)

(generate-typed-num-copy!-func real-double-num-copy!-typed
			       double-float real-matrix-store-type real-matrix
			       blas:dcopy
			       (num
				(1x1-array
				 (allocate-real-store 1)
				 (setf (aref 1x1-array 0) num)
				 :type (real-matrix-store-type 1))))

(defmethod copy! ((x complex-matrix) (y real-matrix))
  (error "Cannot copy a COMPLEX-MATRIX into a REAL-MATRIX,
don't know how to coerce a COMPLEX to a REAL"))

(defmethod copy! ((x complex) (y real-matrix))
  (error "Cannot copy ~a to ~a, don't know how to coerce COMPLEX to REAL"
	 x y))

(defmethod copy! ((x real-matrix) (y real-matrix))
  (real-double-copy!-typed x y))

(defmethod copy! ((x cl:real) (y real-matrix))
  (real-double-num-copy!-typed (coerce x 'double-float) y))

;;
(generate-typed-copy!-func complex-double-copy!-typed complex-matrix-store-type complex-matrix blas:zcopy)

(generate-typed-num-copy!-func complex-double-num-copy!-typed
			       complex-double-float complex-matrix-store-type complex-matrix
			       blas:zcopy
			       (num
				(1x1-z-array
				 (allocate-complex-store 1)
				 (setf (aref 1x1-z-array 0) (realpart num)
				       (aref 1x1-z-array 1) (imagpart num))
				 :type (complex-matrix-store-type 2))))

(defmethod copy! ((x complex-matrix) (y complex-matrix))
  (complex-double-copy!-typed x y))

(defmethod copy! ((x real-matrix) (y complex-matrix))
  (real-double-copy!-typed x (realpart! y))
  (scal! 0d0 (imagpart! y))
  y)

(defmethod copy! ((x number) (y complex-matrix))
  (complex-double-num-copy!-typed (complex-coerce x) y))

;;;;
(defgeneric copy (matrix)
  (:documentation 
   "
  Syntax
  ======
  (COPY x)
 
  Purpose
  =======
  Return a copy of the matrix X"))

(defmethod copy ((matrix real-matrix))
  (let* ((n (nrows matrix))
	 (m (nrows matrix))
	 (result (make-real-matrix-dim n m)))
    (declare (type fixnum n m))
    (copy! matrix result)))

(defmethod copy ((matrix complex-matrix))
  (let* ((n (nrows matrix))
	 (m (ncols matrix))
	 (result (make-complex-matrix-dim n m)))
    (declare (type fixnum n m))
    (copy! matrix result)))

(defmethod copy ((matrix number))
  matrix)

;;
(defgeneric convert-to-lisp-array (matrix)
  (:documentation
   "
  Syntax
  ======
  (CONVERT-TO-LISP-ARRAY matrix)

  Purpose
  =======
  Create a new Lisp array with the same dimensions as the matrix and
  with the same elements.  This is a copy of the matrix.

  Row and column vectors are converted to a 1D lisp vector.  Other
  matrices are converted a 2D lisp array.
"))

(defun convert-1d-array (m eltype)
  (let ((array (make-array (* (number-of-rows m)
			      (number-of-cols m))
			   :element-type eltype)))
    ;; We could do this faster by accessing the storage directly, but
    ;; this is easy.
    (dotimes (k (length array))
      (setf (aref array k) (matrix-ref m k)))
    array))

(defun convert-2d-array (m eltype)
  (let* ((nrows (number-of-rows m))
	 (ncols (number-of-cols m))
	 (array (make-array (list (number-of-rows m)
				  (number-of-cols m))
			    :element-type eltype)))
    ;; We could do this faster by accessing the storage directly, but
    ;; this is easy.
    (dotimes (r nrows)
      (dotimes (c ncols)
	(setf (aref array r c)
	      (matrix-ref m r c))))
    array))

(defmethod convert-to-lisp-array ((m real-matrix))
  (if (or (row-vector-p m) (col-vector-p m))
      (convert-1d-array m 'double-float)
      (convert-2d-array m 'double-float)))

(defmethod convert-to-lisp-array ((m complex-matrix))
  (if (or (row-vector-p m) (col-vector-p m))
      (convert-1d-array m '(complex double-float))
      (convert-2d-array m '(complex double-float))))
