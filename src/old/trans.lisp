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
;;; $Id: trans.lisp,v 1.6 2011/01/25 18:36:56 rtoy Exp $
;;;
;;; $Log: trans.lisp,v $
;;; Revision 1.6  2011/01/25 18:36:56  rtoy
;;; Merge changes from automake-snapshot-2011-01-25-1327 to get the new
;;; automake build infrastructure.
;;;
;;; Revision 1.5.2.1  2011/01/25 18:16:53  rtoy
;;; Use cl:real instead of real.
;;;
;;; Revision 1.5  2001/06/22 12:52:41  rtoy
;;; Use ALLOCATE-REAL-STORE and ALLOCATE-COMPLEX-STORE to allocate space
;;; instead of using the error-prone make-array.
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

;; notes
;; =====
;; transposition is usually a redundant operation.  For example, all BLAS/LAPACK
;; operators take an extra argument asking whether the matrix is transposed,
;; for small operations transposition doesn't make a difference, for repeated
;; small operations -- it may, so you need to use this feature of the 
;; interfaced BLAS/LAPACK functions.
;;
;; also, the intent that TRANSPOSE creates a new matrix should be made clear,
;; for example, taking the transpose of a row/column vector is easy, due to
;; representation, but this will not create a new matrix.

(in-package #:matlisp)

(defun transpose! (matrix)
"
   Syntax
   ======
   (TRANSPOSE! matrix)

   Purpose
   =======
   Exchange row and column strides so that effectively
   the matrix is destructively transposed in place
   (without much effort).
"
  (typecase matrix
    (standard-matrix
     (progn
       (rotatef (nrows matrix) (ncols matrix))
       (rotatef (row-stride matrix) (col-stride matrix))
       matrix))
    (number matrix)
    (t (error "Don't know how to take the transpose of ~A." matrix))))

(defmacro with-transpose! (matlst &rest body)
  `(progn
     ,@(mapcar #'(lambda (mat) `(transpose! ,mat)) matlst)
     ,@body
     ,@(mapcar #'(lambda (mat) `(transpose! ,mat)) matlst)))

;;
(defgeneric transpose~ (matrix)
  (:documentation
"
   Syntax
   ======
   (TRANSPOSE~ matrix)

   Purpose
   =======
   Create a new matrix object which represents the transpose of the
   the given matrix.

   Store is shared with \"matrix\".

   Settable
   ========
   (setf (TRANSPOSE~ matrix) value)

   is basically the same as

   (copy! value (TRANSPOSE~ matrix))
"))

(defun (setf transpose~) (value matrix)
  (copy! value (transpose~ matrix)))

;;
(defmethod transpose~ ((matrix number))
  matrix)

(defmethod transpose~ ((matrix real-matrix))
  (mlet* (((hd nr nc rs cs st) (slot-values matrix '(head number-of-rows number-of-cols row-stride col-stride store))
	   :type (fixnum fixnum fixnum fixnum fixnum (real-matrix-store-type *))))
	 (make-instance 'sub-real-matrix
			:nrows nc :ncols nr
			:store st
			:head hd
			:row-stride cs :col-stride rs
			:parent matrix)))

(defmethod transpose~ ((matrix complex-matrix))
  (mlet* (((hd nr nc rs cs st) (slot-values matrix '(head number-of-rows number-of-cols row-stride col-stride store))
	   :type (fixnum fixnum fixnum fixnum fixnum (complex-matrix-store-type *))))
	 (make-instance 'sub-complex-matrix
			:nrows nc :ncols nr
			:store st
			:head hd
			:row-stride cs :col-stride rs
			:parent matrix)))

;;
(declaim (inline transpose))
(defun transpose (matrix)
"
  Syntax
  ======
  (TRANSPOSE matrix)
 
  Purpose
  =======
  Creates a new matrix which is the transpose of MATRIX.
"
  (copy (transpose~ matrix)))

;;
(defun ctranspose! (matrix)
"
   Syntax
   ======
   (CTRANSPOSE! matrix)

   Purpose
   =======
   Exchange row and column strides so that effectively
   the matrix is destructively transposed in place
   (without much effort). Also scale the imagpart with -1,
   so that the end result is the Hermitian conjugate.
"
  (transpose! matrix)
  (when (typep matrix 'complex-matrix)
    (scal! -1d0 (mimagpart~ matrix)))
  matrix)

;;
(defun ctranspose (matrix)
"
  Syntax
  ======
  (CTRANSPOSE matrix)

  Purpose
  =======
  Returns a new matrix which is the conjugate transpose
  of MATRIX.
"
  (let ((result (copy matrix)))
    (ctranspose! result)))