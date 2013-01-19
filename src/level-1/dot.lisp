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

(defmacro generate-typed-dot (func (tensor-class blas-func blasc-func fortran-lb))
  (let* ((opt (get-tensor-class-optimization-hashtable tensor-class))
	 (conj? (getf opt :fconj))
	 (blas? (and blas-func (if conj? blasc-func t))))
    (assert opt nil 'tensor-cannot-find-optimization :tensor-class tensor-class)
    (setf (getf opt :dot) func
	  (get-tensor-class-optimization tensor-class) opt)
    `(defun ,func (x y conjugate-p)
       (declare (type ,tensor-class x y)
		,(if conj?
		     `(type boolean conjugate-p)
		     `(ignore conjugate-p)))
       ,(let
	 ((lisp-routine
	   `(let-typed
	     ((stp-x (aref (strides x) 0) :type index-type)
	      (sto-x (store x) :type ,(linear-array-type (getf opt :store-type)))
	      (stp-y (aref (strides y) 0) :type index-type)
	      (sto-y (store y) :type ,(linear-array-type (getf opt :store-type)))
	      (nele (number-of-elements x) :type index-type))
	     ,(labels ((main-loop (conjp)
				  `(very-quickly
				     (loop :repeat nele
					:for of-x :of-type index-type = (head x) :then (+ of-x stp-x)
					:for of-y :of-type index-type = (head y) :then (+ of-y stp-y)
					:with dot :of-type ,(getf opt :element-type) = (,(getf opt :fid+))
					:do (let-typed ((xval ,(recursive-append
								(when conjp `(,conj?))
								`(,(getf opt :reader) sto-x of-x)) :type ,(getf opt :element-type))
							(yval (,(getf opt :reader) sto-y of-y) :type ,(getf opt :element-type)))
						       (setf dot (,(getf opt :f+) dot (,(getf opt :f*) xval yval))))
					:finally (return dot)))))
		      (if conj?
			  `(if conjugate-p
			       ,(main-loop t)
			       ,(main-loop nil))
			  (main-loop nil))))))
	 (if blas?
	     `(let ((call-fortran? (> (number-of-elements x)
				      ,fortran-lb)))
		(cond
		  (call-fortran?
		   ,(recursive-append
		     (when conj?
			    `(if conjugate-p
				 (,blasc-func (number-of-elements x)
					      (store x) (aref (strides x) 0)
					      (store y) (aref (strides y) 0)
					      (head x) (head y))))
		     `(,blas-func (number-of-elements x)
				  (store x) (aref (strides x) 0)
				  (store y) (aref (strides y) 0)
				  (head x) (head y))))
		  (t
		   ,lisp-routine)))
	     lisp-routine)))))

(generate-typed-dot real-typed-dot
  (real-tensor ddot nil *real-l1-fcall-lb*))

(generate-typed-dot complex-typed-dot
  (complex-tensor zdotu zdotc *complex-l1-fcall-lb*))

#+maxima
(generate-typed-dot symbolic-typed-dot
  (symbolic-tensor nil nil 0))

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
  (declare (ignore conjugate-p)
	   (type complex-vector y))
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

#+nil
(defmethod dot ((x standard-vector) (y standard-vector) &optional (conjugate-p t))
  (let ((xcl (class-of x))
	(ycl (class-of y)))
    (unless (and (eq xcl (find-class (get (class-name xcl) :vector)))
		 (eq ycl (find-class (get (class-name ycl) :vector))))
      (error "Arguments are not vectors!"))
    (cond
      ((eq (class-of x) (class-of y))
       ;;Generate method
       (let* ((classn (get (class-name xcl) :vector))
	      (dot-func (if-ret (get classn :dot)
				(let ((dot-name (gensym (string+ (symbol-name classn) "-dot-"))))
				  (compile-and-eval
				   `(generate-typed-dot ,dot-name
							(,classn nil nil 0)))
				  dot-name))))
	 (compile-and-eval
	  `(defmethod dot ((x ,classn) (y ,classn) &optional (conjugate-p t))
	     ,@(unless (get classn :fconj)
		       `((declare (ignore conjugate-p))))
	     ,(if (get classn :fconj)
		  `(,dot-func x y conjugate-p)
		  `(,dot-func x y t))))
       ;;Call method
	 (dot x y conjugate-p)))
      ((coercable? (class-name xcl) (class-name ycl))
       ...)
      ((coercable? (class-name xcl) (class-name ycl))
       ...))))
