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
;;; $Id: realimag.lisp,v 1.2 2000/05/08 17:19:18 rtoy Exp $
;;;
;;; $Log: realimag.lisp,v $
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

(export '(real
	  imag))

(defgeneric real (matrix)
  (:documentation
   "
  Syntax
  ======
  (REAL matrix)
 
  Purpose
  =======
  Returns a new REAL-MATRIX which is the real part of MATRIX. 
  If MATRIX is a scalar, returns its real part.

  See IMAG, REALPART, IMAGPART
"))

(defgeneric imag (matrix)
  (:documentation
   "
  Syntax
  ======
  (IMAG matrix)
 
  Purpose
  =======
  Returns a new REAL-MATRIX which is the imaginary part of MATRIX. 
  If MATRIX is a scalar, returns its imaginary part.

  See REAL, REALPART, IMAGPART
"))

(defmethod real ((x number))
  (realpart x))

(defmethod real ((mat real-matrix))
  (copy mat))

(defmethod real ((mat complex-matrix))
  (let* ((n (nrows mat))
	 (m (ncols mat))
	 (nxm (number-of-elements mat))
	 (store (store mat))
	 (new-store (make-array nxm :element-type 'real-matrix-element-type)))
    (declare (type fixnum n m nxm)
	     (type (complex-matrix-store-type (*)) store)
	     (type (real-matrix-store-type (*)) new-store))

    (dcopy nxm store 2 new-store 1)

    (make-instance 'real-matrix :nrows n :ncols m :store new-store)))

(defmethod real ((mat standard-matrix))
  (error "don't know how to take the real part of a STANDARD-MATRIX,
its element types are unknown"))

(defmethod imag ((x number))
  (imagpart x))

(defmethod imag ((mat real-matrix))
  (let ((n (nrows mat))
	(m (ncols mat)))
    (declare (type fixnum n m))
    (make-real-matrix-dim n m)))


(defmethod imag ((mat complex-matrix))
  (let* ((n (nrows mat))
	 (m (ncols mat))
	 (nxm (number-of-elements mat))
	 (store (store mat))
	 (new-store (make-array nxm :element-type 'real-matrix-element-type)))
    (declare (type fixnum n m nxm)
	     (type (complex-matrix-store-type (*)) store)
	     (type (real-matrix-store-type (*)) new-store))

    (with-vector-data-addresses ((addr-store store)
				 (addr-new-store new-store))
	(incf-sap :double-float addr-store)
	(blas::fortran-dcopy nxm addr-store 2 addr-new-store 1))
    
    (make-instance 'real-matrix :nrows n :ncols m :store new-store)))
