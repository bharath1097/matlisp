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
;;; $Id: mplus.lisp,v 1.1 2000/04/14 00:11:12 simsek Exp $
;;;
;;; $Log: mplus.lisp,v $
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

(export '(m+
	  m.+
	  m+!
	  m.+!))

(defgeneric m+ (a b)
  (:documentation
   "
  Syntax
  ======
  (M+ a b)

  Purpose
  =======
  Create a new matrix which is the sum of A and B.
  A or B (but not both) may be a scalar, in which
  case the addition is element-wise.
"))

(defgeneric m+! (a b)
  (:documentation
   "
  Syntax
  ======
  (M+! a b)

  Purpose
  =======
  Desctructive version of M+:

       B <- A + B
"))

(defgeneric m.+ (a b)
  (:documentation
   "
  Syntax
  ======
  (M.+ a b)

  Purpose
  =======
  Same as M+
"))

(defgeneric m.+! (a b)
  (:documentation
   "
  Syntax
  ======
  (M.+! a b)

  Purpose
  =======
  Same as M.+!
"))

(defmethod m.+ (a b)
  (M+ a b))

(defmethod m.+! (a b)
  (M+! a b))

(defmethod m+ :before ((a standard-matrix) (b standard-matrix))
  (let ((n-a (n a))
	(m-a (m a))
	(n-b (n b))
	(m-b (m b)))
    (declare (type fixnum n-a m-a n-b m-b))

    (unless (and (= n-a n-b)
		 (= m-a m-b))
      (error "Cannot add a ~d x ~d matrix and a ~d x ~d matrix"
	     n-a m-a
	     n-b m-b))))


(defmethod m+ ((a standard-matrix) (b standard-matrix))
  (axpy 1.0d0 a b))

(defmethod m+ ((a real-matrix) (b double-float))
  (let ((nxm (nxm a))
	(result (copy a)))
    (declare (type fixnum nxm))

    (setf (aref *1x1-real-array* 0) b)
    (daxpy nxm 1.0d0 *1x1-real-array* 0 (store result) 1)
    result))

(defmethod m+ ((a real-matrix) (b real))
  (m+ a (coerce b 'real-matrix-element-type)))

(defmethod m+ ((a double-float) (b real-matrix))
  (m+ b a))

(defmethod m+ ((a real) (b real-matrix))
  (m+ b (coerce a 'real-matrix-element-type)))

(defmethod m+ ((a real-matrix) (b kernel::complex-double-float))
  (let* ((n (n a))
	 (m (m a))
	 (result (make-complex-matrix-dim n m b)))
    (declare (type fixnum n m))

    (axpy! 1.0d0 a result)))

(defmethod m+ ((a real-matrix) (b complex))
  (m+ a (complex-coerce b)))

(defmethod m+ ((a kernel::complex-double-float) (b real-matrix))
  (m+ b a))

(defmethod m+ ((a complex) (b real-matrix))
  (m+ b (complex-coerce a)))

;;;
(defmethod m+ ((a complex-matrix) (b double-float))
  (let ((nxm (nxm a))
	(result (copy a)))
    (declare (type fixnum nxm))

    (setf (aref *1x1-real-array* 0) b)
    (daxpy nxm 1.0d0 *1x1-real-array* 0 (store result) 2)
    result))

(defmethod m+ ((a complex-matrix) (b real))
  (m+ a (coerce b 'complex-matrix-element-type)))

(defmethod m+ ((a double-float) (b complex-matrix))
  (m+ b a))

(defmethod m+ ((a real) (b complex-matrix))
  (m+ b (coerce a 'complex-matrix-element-type)))

(defmethod m+ ((a complex-matrix) (b kernel::complex-double-float))
  (let* ((n (n a))
	 (m (m a))
	 (result (make-complex-matrix-dim n m b)))
    (declare (type fixnum n m))

    (axpy! 1.0d0 a result)))

(defmethod m+ ((a complex-matrix) (b complex))
  (m+ a (complex-coerce b)))

(defmethod m+ ((a kernel::complex-double-float) (b complex-matrix))
  (m+ b a))

(defmethod m+ ((a complex) (b complex-matrix))
  (m+ b (complex-coerce a)))


(defmethod m+! :before ((a standard-matrix) (b standard-matrix))
  (let ((n-a (n a))
	(m-a (m a))
	(n-b (n b))
	(m-b (m b)))
    (declare (type fixnum n-a m-a n-b m-b))
    (unless (and (= n-a n-b)
		 (= m-a m-b))
      (error "Cannot add a ~d x ~d matrix and a ~d x ~d matrix"
	     n-a m-a
	     n-b m-b))))

(defmethod m+! ((a standard-matrix) (b standard-matrix))
  (axpy! 1.0d0 a b))

(defmethod m+! ((a complex-matrix) (b real-matrix))
  (error "cannot M+! a COMPLEX-MATRIX A and a REAL-MATRIX B,
don't know how to coerce COMPLEX to REAL."))

;;;

(defmethod m+! ((a real-matrix) (b double-float))
  (let ((nxm (nxm a)))
    (declare (type fixnum nxm))

    (setf (aref *1x1-real-array* 0) b)
    (daxpy nxm 1.0d0 *1x1-real-array* 0 (store a) 1)
    a))

(defmethod m+! ((a real-matrix) (b real))
  (m+! a (coerce b 'real-matrix-element-type)))

(defmethod m+! ((a double-float) (b real-matrix))
  (m+! b a))

(defmethod m+! ((a real) (b real-matrix))
  (m+! b (coerce a 'real-matrix-element-type)))

(defmethod m+! ((a real-matrix) (b complex))
  (error "cannon M+! a REAL-MATRIX and a COMPLEX,
don't know how to coerce COMPLEX to REAL"))

(defmethod m+! ((a complex) (b real-matrix))
  (error "cannon M+! a REAL-MATRIX and a COMPLEX,
don't know how to coerce COMPLEX to REAL"))

(defmethod m+! ((a complex-matrix) (b double-float))
  (let ((nxm (nxm a)))
    (declare (type fixnum nxm))

    (setf (aref *1x1-real-array* 0) b)
    (daxpy nxm 1.0d0 *1x1-real-array* 0 (store a) 2)
    a))

(defmethod m+! ((a complex-matrix) (b real))
  (m+! a (coerce b 'complex-matrix-element-type)))

(defmethod m+! ((a double-float) (b complex-matrix))
  (m+! b a))

(defmethod m+! ((a real) (b complex-matrix))
  (m+! b (coerce a 'complex-matrix-element-type)))

(defconstant *complex-unity-as-array* 
  (make-array 2 :element-type 'complex-matrix-element-type
	      :initial-contents '(1.0d0 0.0d0)))

(defmethod m+! ((a complex-matrix) (b kernel::complex-double-float))
  (let* ((nxm (nxm a)))
    (declare (type fixnum nxm))

    (setf (aref *1x1-complex-array* 0) (realpart b))
    (setf (aref *1x1-complex-array* 1) (imagpart b))
    (zaxpy nxm *complex-unity-as-array* *1x1-complex-array* 0 (store a) 1)
    a))

(defmethod m+! ((a complex-matrix) (b complex))
  (m+! a (complex-coerce b)))

(defmethod m+! ((a kernel::complex-double-float) (b complex-matrix))
  (m+! b a))

(defmethod m+! ((a complex) (b complex-matrix))
  (m+! b (complex-coerce a)))

