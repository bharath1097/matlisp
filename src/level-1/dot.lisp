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

(deft/generic (t/blas-dot-func #'subfieldp) sym (&optional conjp))
(deft/method t/blas-dot-func (sym real-tensor) (&optional conjp)
  'ddot)
(deft/method t/blas-dot-func (sym complex-tensor) (&optional (conjp t))
  (if conjp 'zdotc 'zdotu))
;;a

(deft/generic (t/blas-dot #'subtypep) sym (x y &optional conjp num-y?))
(deft/method t/blas-dot (sym blas-numeric-tensor) (x y &optional (conjp t) (num-y? nil))
   (using-gensyms (decl (x y))
     (with-gensyms (sto)
       `(let (,@decl)
	  (declare (type ,sym ,x)
		   (type ,(if num-y? (field-type sym) sym) ,y))
	  ,(recursive-append
	    (when num-y?
	      `(with-field-element ,sym (,sto ,y)))
	    `(,(macroexpand-1 `(t/blas-dot-func ,sym ,conjp))
	       (aref (dimensions ,x) 0)
	       (the ,(store-type sym) (store ,x)) (aref (strides ,x) 0)
	       ,(if num-y? sto `(the ,(store-type sym) (store ,y))) ,(if num-y? 0 `(aref (strides ,y) 0))
	       (head ,x) ,(if num-y? 0 `(head ,y))))))))

(deft/generic (t/dot #'subtypep) sym (x y &optional conjp num-y?))
(deft/method t/dot (sym standard-tensor) (x y &optional (conjp t) (num-y? nil))
  (using-gensyms (decl (x y))
    (with-gensyms (sto-x sto-y of-x of-y stp-x stp-y dot)
      `(let (,@decl)
	 (declare (type ,sym ,x ,@(unless num-y? `(,y)))
		  ,@(when num-y? `((type ,(field-type sym) ,y))))
	 (let ((,sto-x (store ,x))
	       (,stp-x (aref (the index-store-vector (strides ,x)) 0))
	       (,of-x (head ,x))
	       ,@(unless num-y?
		   `((,sto-y (store ,y))
		     (,stp-y (aref (the index-store-vector (strides ,y)) 0))
		     (,of-y (head ,y))))
	       (,dot (t/fid+ ,(field-type sym))))
	   (declare (type ,(store-type sym) ,sto-x ,@(unless `(,sto-y)))
		    (type index-type ,stp-x ,of-x ,@(unless num-y? `(,stp-y  ,of-y)))
		    (type ,(field-type sym) ,dot))
	   (very-quickly
	     (loop :repeat (aref (the index-store-vector (dimensions ,x)) 0)
		:do (setf ,dot (t/f+ ,(field-type sym) ,dot
				     (t/f* ,(field-type sym)
					   ,(recursive-append (when conjp `(t/fc ,(field-type sym))) `(t/store-ref ,sym ,sto-x ,of-x))
					   ,(if num-y? y `(t/store-ref ,sym ,sto-y ,of-y))))
			  ,of-x (+ ,of-x ,stp-x)
			  ,@(unless num-y? `(,of-y (+ ,of-y ,stp-y))))))
	   ,dot)))))
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

  If X and Y are both scalars then this is the same
  as (* (CONJUGATE X) Y) if CONJUAGTE-P and (* X Y)
  otherwise.
")
  (:method :before ((x standard-tensor) (y standard-tensor) &optional (conjugate-p t))
    (declare (ignore conjugate-p))
    (unless (and (tensor-vectorp x) (tensor-vectorp y) (= (aref (the index-store-vector (dimensions x)) 0) (aref (the index-store-vector (dimensions y)) 0)))
      (error 'tensor-dimension-mismatch))))

(defmethod dot ((x number) (y number) &optional (conjugate-p t))
  (if conjugate-p
      (* (conjugate x) y)
      (* x y)))

(defmethod dot ((x standard-tensor) (y standard-tensor) &optional (conjugate-p t))
  (let ((clx (class-name (class-of x)))
	(cly (class-name (class-of y))))
    (assert (and (member clx *tensor-type-leaves*)
		 (member cly *tensor-type-leaves*))
	    nil 'tensor-abstract-class :tensor-class (list clx cly))
    (cond
      ((eq clx cly)
       (compile-and-eval
	`(defmethod dot ((x ,clx) (y ,cly) &optional (conjugate-p t))
	   ,(recursive-append
	     (when (subtypep clx 'blas-numeric-tensor)
	       `(if (call-fortran? x (t/l1-lb ,clx))
		    (if conjugate-p
			(t/blas-dot ,clx x y t)
			(t/blas-dot ,clx x y nil))))
	     `(if conjugate-p
		  ;;Please do your checks before coming here.
		  (t/dot ,clx x y t)
		  (t/dot ,clx x y nil)))))
       (dot x y conjugate-p))
      (t
       (error "Don't know how to compute the dot product of ~a , ~a." clx cly)))))

(defmethod dot ((x standard-tensor) (y t) &optional (conjugate-p t))
  (let ((clx (class-name (class-of x))))
    (assert (member clx *tensor-type-leaves*)
	    nil 'tensor-abstract-class :tensor-class (list clx))
    (compile-and-eval
     `(defmethod dot ((x ,clx) (y t) &optional (conjugate-p t))
	(let ((y (t/coerce ,(field-type clx) y)))
	  (declare (type ,(field-type clx) y))
	  ,(recursive-append
	    (when (subtypep clx 'blas-numeric-tensor)
	      `(if (call-fortran? x (t/l1-lb ,clx))
		   (if conjugate-p
		       (t/blas-dot ,clx x y t t)
		       (t/blas-dot ,clx x y nil t))))
	    `(if conjugate-p
		 ;;Please do your checks before coming here.
		 (t/dot ,clx x y t t)
		 (t/dot ,clx x y nil t))))))
    (dot x y conjugate-p)))
