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
;;; $Id: scal.lisp,v 1.1 2000/04/14 00:11:12 simsek Exp $
;;;
;;; $Log: scal.lisp,v $
;;; Revision 1.1  2000/04/14 00:11:12  simsek
;;; o This file is adapted from obsolete files 'matrix-float.lisp'
;;;   'matrix-complex.lisp' and 'matrix-extra.lisp'
;;; o Initial revision.
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MATLISP")

(use-package "BLAS")
(use-package "LAPACK")
(use-package "FORTRAN-FFI-ACCESSORS")

(export '(scal!
	  scal))

(defgeneric scal (alpha x)
  (:documentation
"
  Sytnax
  ======
  (SCAL alpha x)

  Purpose
  =======
  Computes and returns a new matrix equal to

             alpha * X

  where alpha is a scalar and X is a matrix.

"))

(defgeneric scal! (alpha x)
  (:documentation
"
  Sytnax
  ======
  (SCAL! alpha x)

  Purpose
  =======
  Same as SCAL except that the result is
  stored in X.
"))

(defmethod scal ((alpha number) (x number))
  (* alpha x))

(defmethod scal ((alpha double-float) (x real-matrix))
  (let ((nxm (nxm x))
	(result (copy x)))
    (declare (type fixnum nxm))
    
    (dscal nxm alpha (store result) 1)
    result))

(defmethod scal ((alpha real) (x real-matrix))
  (scal (coerce alpha 'real-matrix-element-type) x))

(defmethod scal ((alpha kernel::complex-double-float) (x real-matrix))
  (let* ((nxm (nxm x))
	 (n (n x))
	 (m (m x))
	 (result (make-complex-matrix-dim n m)))
    (declare (type fixnum n m nxm))
    
    (copy! x result)
    (setf (aref *1x1-complex-array* 0) (realpart alpha))
    (setf (aref *1x1-complex-array* 1) (imagpart alpha))
    (zscal nxm *1x1-complex-array* (store result) 1)

    result))

(defmethod scal ((alpha complex) (x real-matrix))
  (scal (complex-coerce alpha) x))

(defmethod scal ((alpha double-float) (x complex-matrix))
  (let ((nxm (nxm x))
	(result (copy x)))
    (declare (type fixnum nxm))
    (zdscal nxm alpha (store result) 1)
    
    result))

(defmethod scal ((alpha real) (x complex-matrix))
  (scal (coerce alpha 'real-matrix-element-type) x))

(defmethod scal ((alpha kernel::complex-double-float) (x complex-matrix))
  (let ((nxm (nxm x))
	(result (copy x)))
    (declare (type fixnum nxm))
    (setf (aref *1x1-complex-array* 0) (realpart alpha))
    (setf (aref *1x1-complex-array* 1) (imagpart alpha))
    (zscal nxm *1x1-complex-array* (store result) 1)

    result))

(defmethod scal ((alpha complex) (x complex-matrix))
  (scal (complex-coerce alpha) x))


(defmethod scal! ((alpha number) (x number))
  (error "cannot SCAL! two scalars, arg X must 
be a matrix to SCAL!"))

(defmethod scal! ((alpha double-float) (x real-matrix))
  (let ((nxm (nxm x)))
    (declare (type fixnum nxm))
    
    (dscal nxm alpha (store x) 1)
    x))

(defmethod scal! ((alpha real) (x real-matrix))
  (scal! (coerce alpha 'real-matrix-element-type) x))

(defmethod scal! ((alpha complex) (x real-matrix))
  (error "cannot SCAL! a REAL-MATRIX by a COMPLEX, don't know
how to coerce COMPLEX to REAL"))

(defmethod scal! ((alpha double-float) (x complex-matrix))
  (let ((nxm (nxm x)))
    (declare (type fixnum nxm))
    (zdscal nxm alpha (store x) 1)
    
    x))

(defmethod scal! ((alpha real) (x complex-matrix))
  (scal! (coerce alpha 'real-matrix-element-type) x))

(defmethod scal! ((alpha kernel::complex-double-float) (x complex-matrix))
  (let ((nxm (nxm x)))
    (declare (type fixnum nxm))
    (setf (aref *1x1-complex-array* 0) (realpart alpha))
    (setf (aref *1x1-complex-array* 1) (imagpart alpha))
    (zscal nxm *1x1-complex-array* (store x) 1)

    x))

(defmethod scal! ((alpha complex) (x complex-matrix))
  (scal! (complex-coerce alpha) x))



    

    
    
