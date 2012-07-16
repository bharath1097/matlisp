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
    (complex-tensor (make-instance 'real-tensor
				   :parent-tensor tensor :store (store tensor)
				   :dimensions (dimensions tensor)
				   :strides (map 'index-store-vector #'(lambda (x) (* 2 x)) (strides tensor))
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
    (complex-tensor (make-instance 'real-tensor
				   :parent-tensor tensor :store (store tensor)
				   :dimensions (dimensions tensor)
				   :strides (map 'index-store-vector #'(lambda (x) (* 2 x)) (strides tensor))
				   :head (the index-type (+ 1 (* 2 (head tensor))))))
    (number (imagpart tensor))))

(definline tensor-realpart (tensor)
"
  Syntax
  ======
  (tensor-realpart tensor)
 
  Purpose
  =======
  Returns a copy of the real part of tensor.

  If \"tensor\" is a scalar, returns its real part.

  See IMAG, REALPART, IMAGPART
"
  (copy (tensor-realpart~ tensor)))

(definline tensor-imagpart (tensor)
"
  Syntax
  ======
  (tensor-imagpart matrix)
 
  Purpose
  =======
  Returns a copy of the imaginary part of \"matrix\".

  If \"matrix\" is a scalar, returns its imaginary part.

  See IMAG, REALPART, IMAGPART
"
  (copy (tensor-imagpart~ tensor)))
