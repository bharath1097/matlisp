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

(deft/generic (t/blas-swap-func #'subtypep) sym ())
(deft/method t/blas-swap-func (sym real-tensor) ()
  'dswap)
(deft/method t/blas-swap-func (sym complex-tensor) ()
  'zswap)
;;
(deft/generic (t/blas-swap! #'subtypep) sym (sz x st-x y st-y))
(deft/method t/blas-swap! (sym blas-numeric-tensor) (sz x st-x y st-y)
  (let* ((decl (zipsym (list x y)))
	 (args (mapcar #'car decl))
	 (func (macroexpand-1 `(t/blas-swap-func ,sym))))
    (let ((x (first args)) (y (second args)))
      `(let (,@decl)
	 (declare (type ,sym ,@args))
	 (,func
	  (the index-type ,sz)
	  (the ,(store-type sym) (store ,x)) (the index-type ,st-x)
	  (the ,(store-type sym) (store ,y)) (the index-type ,st-y)
	  (head ,x) (head ,y))
	 ,y))))
  
(deft/generic (t/swap! #'subtypep) sym (x y))
(deft/method t/swap! (sym standard-tensor) (x y)
  (let* ((decl (zipsym (list x y)))
	 (args (mapcar #'car decl)))
    (let ((x (first args)) (y (second args)))
      `(let* (,@decl
	      (sto-x (store ,x))
	      (sto-y (store ,y)))
	 (declare (type ,sym ,@args)
		  (type ,(store-type sym) sto-x sto-y))
	 (mod-dotimes (idx (dimensions ,x))
	   :with (linear-sums
		  (of-x (strides ,x) (head ,x))
		  (of-y (strides ,y) (head ,y)))
	   :do (let-typed ((y-val (t/store-ref ,sym sto-y of-y) :type ,(field-type sym)))
		 (t/store-set ,sym
			      (t/store-ref ,sym sto-x of-x) sto-y of-y)
		 (t/store-set ,sym
			      y-val sto-x of-x)))
	 ,y))))
;;---------------------------------------------------------------;;
(defmethod swap! :before ((x standard-tensor) (y standard-tensor))
  (assert (very-quickly (lvec-eq (the index-store-vector (dimensions x)) (the index-store-vector (dimensions y)) #'=)) nil
	  'tensor-dimension-mismatch))

(defmethod swap! ((x standard-tensor) (y standard-tensor))
  (let ((clx (class-name (class-of x)))
	(cly (class-name (class-of y))))
    (assert (and (member clx *tensor-type-leaves*)
		 (member cly *tensor-type-leaves*))
	    nil 'tensor-abstract-class :tensor-class (list clx cly))
    (if (eq clx cly)
	(progn
	  (compile-and-eval
	   `(defmethod swap! ((x ,clx) (y ,cly))
	      ,(recursive-append
		(when (subtypep clx 'blas-numeric-tensor)
		  `(if-let (strd (and (call-fortran? x (t/l1-lb ,clx)) (blas-copyablep x y)))
		     (let ((sz (size x))) (t/blas-swap! ,clx sz x (first strd) y (second strd)))))
		`(very-quickly (t/swap! ,clx x y)))
	      y))
	  (swap! x y))
	;;It is silly to swap a real vector with a complex one, no?
	(error "Don't know how to swap ~a and ~a." clx cly))))
