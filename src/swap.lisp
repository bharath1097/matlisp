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
;;; $Id: swap.lisp,v 1.1 2000/04/14 00:11:12 simsek Exp $
;;;
;;; $Log: swap.lisp,v $
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

(export '(swap!))

(defgeneric swap! (x y)
  (:documentation
"
  Sytnax
  ======
  (SWAP! x y)

  Purpose
  =======
  Given matrices X,Y, performs:

              X <-> Y

  and returns Y.

 X,Y need not have the same dimensions,
 but must have the same total number of 
 elements.  Practically, this is useful
 for adding a row and column vector of 
 the same size etc ...
"))

(defmethod swap! :before ((x standard-matrix) (y standard-matrix))
  (let ((nxm-x (nxm x))
	(nxm-y (nxm y)))
    (declare (type fixnum nxm-x nxm-y))
    (if (not (= nxm-x nxm-y))
	(error "arguments X,Y to SWAP! not the same size"))))

(defmethod swap! ((x real-matrix) (y real-matrix))
  (let ((nxm (nxm x)))
    (dswap nxm (store x) 1 (store y) 1)
    y))

(defmethod swap! ((x complex-matrix) (y complex-matrix))
  (let ((nxm (nxm x)))
    (zswap nxm (store x) 1 (store y) 1)
    y))

(defmethod swap! ((x real-matrix) (y complex-matrix))
  (error "cannot SWAP! a real matrix with a complex one,
don't know how to coerce COMPLEX to REAL"))

(defmethod swap! ((x complex-matrix) (y real-matrix))
  (error "cannot SWAP! a real matrix with a complex one,
don't know how to coerce COMPLEX to REAL"))
