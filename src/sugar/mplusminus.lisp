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
")
  (:method ((a number) (b number))
    (+ a b))
  (:method ((a standard-tensor) (b standard-tensor))
    (axpy 1 a b))
  (:method ((a number) (b standard-tensor))
    (axpy a nil b))
  (:method ((a standard-tensor) (b number))
    (axpy b nil a)))

(definline m.+ (a b)
  "
  Syntax
  ======
  (M.+ a b)

  Purpose
  =======
  Same as M+
"
  (m+ a b))
;;---------------------------------------------------------------;;

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
")
  (:method ((a number) (b number))
    (+ a b))
  (:method ((a standard-tensor) (b standard-tensor))
    (axpy! 1 a b))
  (:method ((a number) (b standard-tensor))
    (axpy! a nil b)))

(definline m.+! (a b)
  "
  Syntax
  ======
  (M.+! a b)

  Purpose
  =======
  Same as M+!
"
  (m+! a b))
;;---------------------------------------------------------------;;

(defgeneric m- (a b)
  (:documentation
   "
  Syntax
  ======
  (M- a b)

  Purpose
  =======
  Create a new matrix which is the difference of A and B.
  B may be a scalar, in which case the subtraction
  is elementwise.
")
  (:method ((a number) (b number))
    (- a b))
  (:method ((a standard-tensor) (b standard-tensor))
    (axpy -1 b a))
  (:method ((a number) (b standard-tensor))
    (scal! -1 (axpy (- a) nil b)))
  (:method ((a standard-tensor) (b number))
    (axpy (- b) nil a)))

(definline m.- (a b)
  "
  Syntax
  ======
  (M.- a b)

  Purpose
  =======
  Same as M-
"
  (m- a b))
;;---------------------------------------------------------------;;

(defgeneric m-! (a b)
  (:documentation
   "
  Syntax
  ======
  (M-! a b)

  Purpose
  =======
  Desctructive version of M-:

       B <- A - B
")
  (:method ((a number) (b number))
    (- a b))
  (:method ((a standard-tensor) (b standard-tensor))
    (scal! -1 (axpy! -1 a b)))
  (:method ((a number) (b standard-tensor))
    (scal! -1 (axpy! (- a) nil b))))

(definline m.-! (a b)
  "
  Syntax
  ======
  (M.-! a b)

  Purpose
  =======
  Same as M-!
"
  (m+! a b))
;;---------------------------------------------------------------;;
