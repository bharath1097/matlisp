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
;;; $Id: realimag.lisp,v 1.6 2004/05/24 16:34:22 rtoy Exp $
;;;
;;; $Log: realimag.lisp,v $
;;; Revision 1.6  2004/05/24 16:34:22  rtoy
;;; More SBCL support from Robert Sedgewick.  The previous SBCL support
;;; was incomplete.
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

(in-package #:matlisp)

(defun tensor-realpart~ (tensor)
"
  Syntax
  ======
  (tensor-realpart~ tensor)
 
  Purpose
  =======
  Returns a new tensor object which points to  the real part of TENSOR.
  Store is shared with TENSOR.

  If TENSOR is a scalar, returns its real part.
"
  (etypecase tensor
    (real-tensor tensor)
    (complex-tensor (make-instance 'real-sub-tensor
				   :parent-tensor tensor :store (store tensor)
				   :dimensions (dimensions tensor)
				   :strides (map '(index-array *) #'(lambda (x) (* 2 x)) (strides xten))
				   :head (the index-type (* 2 (head tensor)))))
    (number (realpart tensor))))

(defun tensor-imagpart~ (tensor)
"
  Syntax
  ======
  (tensor-imagpart~ tensor)
 
  Purpose
  =======
  Returns a new tensor object which points to  the \"imaginary\" part of TENSOR.
  Store is shared with TENSOR.

  If TENSOR is a scalar, returns its imaginary part.
"
  (etypecase tensor
    (real-tensor tensor)
    (complex-tensor (make-instance 'real-sub-tensor
				   :parent-tensor tensor :store (store tensor)
				   :dimensions (dimensions tensor)
				   :strides (map '(index-array *) #'(lambda (x) (* 2 x)) (strides xten))
				   :head (the index-type (+ 1 (* 2 (head tensor))))))
    (number (imagpart tensor))))

(defun tensor-realpart (mat)
"
  Syntax
  ======
  (MREALPART matrix)
 
  Purpose
  =======
  Returns a copy of the real part of \"matrix\".

  If \"matrix\" is a scalar, returns its real part.

  See IMAG, REALPART, IMAGPART
"
  (typecase mat
    (real-matrix (copy mat))
    (complex-matrix (copy (make-instance 'sub-real-matrix
					 :parent mat :store (store mat)
					 :nrows (nrows mat) :ncols (ncols mat)
					 :row-stride (* 2 (row-stride mat)) :col-stride (* 2 (col-stride mat))
					 :head (* 2 (head mat)))))
    (number (cl:realpart mat))))

(defun tensor-imagpart (mat)
"
  Syntax
  ======
  (MIMAGPART~ matrix)
 
  Purpose
  =======
  Returns a copy of the imaginary part of \"matrix\".

  If \"matrix\" is a scalar, returns its imaginary part.

  See IMAG, REALPART, IMAGPART
"
  
  (typecase mat
    (real-matrix (make-real-matrix-dim (nrows mat) (ncols mat)))
    (complex-matrix (copy (make-instance 'sub-real-matrix
					 :parent mat :store (store mat)
					 :nrows (nrows mat) :ncols (ncols mat)
					 :row-stride (* 2 (row-stride mat)) :col-stride (* 2 (col-stride mat))
					 :head (+ 1 (* 2 (head mat))))))
    (number (cl:imagpart mat))))


(declaim (inline real))
(defun real (matrix)
"
  Syntax
  ======
  (REAL matrix)
 
  Purpose
  =======
  Returns a new REAL-MATRIX which is the real part of MATRIX. 
  If MATRIX is a scalar, returns its real part.

  See IMAG, REALPART, IMAGPART
"
  (mrealpart matrix))
  

(defun imag (matrix)
"
  Syntax
  ======
  (IMAG matrix)
 
  Purpose
  =======
  Returns a new REAL-MATRIX which is the imaginary part of MATRIX. 
  If MATRIX is a scalar, returns its imaginary part.

  See REAL, REALPART, IMAGPART
"
  (mimagpart matrix))
