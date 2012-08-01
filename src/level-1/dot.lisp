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

(defun real-typed-dot (x y conjugate-p)
  (declare (type real-vector x y)
	   (ignore conjugate-p))
  (let ((call-fortran? (> (number-of-elements x)
			  *real-l1-fcall-lb*)))
    (cond
      (call-fortran?
       (ddot (number-of-elements x)
	     (store x) (aref (strides x) 0)
	     (store y) (aref (strides y) 0)
	     (head x) (head y)))
      (t
       (let-typed
	((stp-x (aref (strides x) 0) :type index-type)
	 (sto-x (store x) :type real-store-vector)
	 (stp-y (aref (strides y) 0) :type index-type)
	 (sto-y (store y) :type real-store-vector)
	 (nele (number-of-elements x) :type index-type))
	(very-quickly
	  (loop repeat nele
	     for of-x of-type index-type = (head x) then (+ of-x stp-x)
	     for of-y of-type index-type = (head y) then (+ of-y stp-y)
	     summing (* (aref sto-x of-x) (aref sto-y of-y)) into dot of-type real-type
	     finally (return dot))))))))

(defun complex-typed-dot (x y conjugate-p)
  (declare (type complex-vector x y))
  (let ((call-fortran? (> (number-of-elements x)
			  *complex-l1-fcall-lb*)))
    (cond
      (call-fortran?
       (if conjugate-p
	   (zdotc (number-of-elements x)
		  (store x) (aref (strides x) 0)
		  (store y) (aref (strides y) 0)
		  (head x) (head y))
	   (zdotu (number-of-elements x)
		  (store x) (aref (strides x) 0)
		  (store y) (aref (strides y) 0)
		  (head x) (head y))))
      (t
       (let-typed
	((stp-x (aref (strides x) 0) :type index-type)
	 (sto-x (store x) :type complex-store-vector)
	 (stp-y (aref (strides y) 0) :type index-type)
	 (sto-y (store y) :type complex-store-vector)
	 (nele (number-of-elements x) :type index-type))
	(if conjugate-p
	    (very-quickly
	      (loop repeat nele
		 for of-x of-type index-type = (head x) then (+ of-x stp-x)
		 for of-y of-type index-type = (head y) then (+ of-y stp-y)
		 summing (let-typed ((xval (complex (aref sto-x (* 2 of-x)) (- (aref sto-x (1+ (* 2 of-x))))) :type complex-type)
				     (yval (complex (aref sto-y (* 2 of-y)) (aref sto-y (1+ (* 2 of-y)))) :type complex-type))
				    (* xval yval))
		         into dot of-type complex-type
		 finally (return dot)))
	    (very-quickly
	      (loop repeat nele
		 for of-x of-type index-type = (head x) then (+ of-x stp-x)
		 for of-y of-type index-type = (head y) then (+ of-y stp-y)
		 summing (let-typed ((xval (complex (aref sto-x (* 2 of-x)) (aref sto-x (1+ (* 2 of-x)))) :type complex-type)
				     (yval (complex (aref sto-y (* 2 of-y)) (aref sto-y (1+ (* 2 of-y)))) :type complex-type))
				    (* xval yval))
		         into dot of-type complex-type
		 finally (return dot)))))))))

;;---------------------------------------------------------------;;
		  
       
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
")
  (:method :before ((x standard-vector) (y standard-vector) &optional (conjugate-p t))
  (declare (ignore conjugate-p))
  (unless (lvec-eq (dimensions x) (dimensions y) #'=)
    (error 'tensor-dimension-mismatch))))

(defmethod dot ((x number) (y number) &optional (conjugate-p t))
  (if conjugate-p
      (* (conjugate x) y)
      (* x y)))

(defmethod dot ((x real-vector) (y real-vector) &optional (conjugate-p t))
  (declare (ignore conjugate-p))
  (real-typed-dot x y nil))

(defmethod dot ((x real-vector) (y complex-vector) &optional (conjugate-p t))
  (declare (ignore conjugate-p))
  (let ((vw.y (tensor-realpart~ y)))
    (declare (type real-vector vw.y))
    (let ((rpart (prog1 (real-typed-dot x vw.y nil)
		   ;;Move view to complex-part
		   (incf (head vw.y))))
	  (ipart (real-typed-dot x vw.y nil)))
      (declare (type complex-base-type rpart ipart))
      (if (zerop ipart)
	  rpart
	  (complex rpart ipart)))))

(defmethod dot ((x complex-vector) (y real-vector) &optional (conjugate-p t))
  (let ((cres (dot y x)))
    (if conjugate-p
	(conjugate cres)
	cres)))

(defmethod dot ((x complex-vector) (y complex-vector) &optional (conjugate-p t))
  (complex-typed-dot x y conjugate-p))
