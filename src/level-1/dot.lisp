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

(defgeneric dot (x y &optional conjugate-p)
  (:documentation
"
  Sytnax
  ======
  (DOT x y [conjugate-p])

  Purpose
  =======
  Computes the inner product of X,Y.

  CONJUGATE-P       Computed Result
  ---------------------------------
                         H
  T (default)           X * Y
                         T
  NIL                   X * Y


  If X is real then CONJUGATE-P has no 
  effect since for real vectors:

                H   T
               X = X

  X,Y must be vectors but it doesn't matter
  if they are row or column vectors.

  If X and Y are both scalars then this is the same
  as (* (CONJUGATE X) Y) if CONJUAGTE-P and (* X Y)
  otherwise.
"))

(defmethod dot ((x number) (y number) &optional (conjugate-p t))
  (if conjugate-p
      (* (conjugate x) y)
      (* x y)))

(defmethod dot :before ((x standard-tensor) (y standard-tensor) &optional (conjugate-p t))
  (declare (ignore conjugate-p))
  (unless (and (vector-p x) (vector-p y))
    (error 'tensor-not-vector
	   :rank (cond
		   ((not (vector-p x))
		    (rank x))
		   ((not (vector-p y))
		    (rank y)))))
  (unless (idx= (dimensions x) (dimensions y))
    (error 'tensor-dimension-mismatch)))

(defmethod dot ((x real-tensor) (y real-tensor) &optional (conjugate-p t))
  (declare (ignore conjugate-p))
  (ddot (number-of-elements x)
	(store x) (aref (strides x) 0)
	(store y) (aref (strides y) 0)
	(head x) (head y)))

(defmethod dot ((x real-tensor) (y complex-tensor) &optional (conjugate-p t))
  (declare (ignore conjugate-p))
  (let ((nele (number-of-elements x))
	(std-x (aref (strides x) 0))
	(hd-x (head x))
	(std-y (aref (strides y) 0))
	(hd-y (head y)))
    (declare (type index-type nele std-x std-y hd-x hd-y))
    (let ((rpart (ddot nele (store x) std-x (store y) (* 2 std-y) hd-x (* 2 hd-y)))
	  (ipart (ddot nele (store x) std-x (store y) (* 2 std-y) hd-x (1+ (* 2 hd-y)))))
      (declare (type complex-base-type rpart ipart))
      (if (zerop ipart)
	  rpart
	  (complex rpart ipart)))))

(defmethod dot ((x complex-tensor) (y real-tensor) &optional (conjugate-p t))
  (let ((cres (dot y x)))
    (if conjugate-p
	(conjugate cres)
	cres)))

(defmethod dot ((x complex-tensor) (y complex-tensor) &optional (conjugate-p t))
  (let ((nele (number-of-elements x))
	(std-x (aref (strides x) 0))
	(hd-x (head x))
	(std-y (aref (strides y) 0))
	(hd-y (head y)))
    (declare (type index-type nele std-x hd-x std-y hd-y))
    (let ((ret (if conjugate-p
		   (zdotc nele
			  (store x) std-x
			  (store y) std-y
			  hd-x hd-y)
		   (zdotu nele
			  (store x) std-x
			  (store y) std-y
			  hd-x hd-y))))
      (if (zerop (imagpart ret))
	  (realpart ret)
	  ret))))
