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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package #:matlisp)

(deft/generic (t/blas-scdi-func #'subfieldp) sym (&optional scal?))

(deft/method t/blas-scdi-func (sym real-tensor) (&optional (scal? t))
  (if scal?
      'descal
      'dediv))

(deft/method t/blas-scdi-func (sym complex-tensor) (&optional (scal? t))
  (if scal?
      'zescal
      'zediv))
;;
(deft/generic (t/blas-scdi! #'subfieldp) sym (x st-x y st-y &optional scal?))
(deft/generic (t/scdi! #'subtypep) sym (x y &key scal? numx?))

(deft/method t/blas-scdi! (sym blas-numeric-tensor) (x st-x y st-y &optional (scal? t))
  (let ((numx? (null st-x)))
    (using-gensyms (decl (x y))
      (with-gensyms (sto-x stp-x)
        `(let (,@decl)
	   (declare (type ,sym ,@(unless numx? `(,x)) ,y)
		    ,@(when numx? `((type ,(field-type sym) ,x))))
	   (let ((,sto-x ,(if numx? `(t/store-allocator ,sym 1) `(store ,x)))
		 (,stp-x ,(if numx? 0 st-x)))
	     (declare (type ,(store-type sym) ,sto-x)
		      (type index-type ,stp-x))
	     ,@(when numx?
		     `((t/store-set ,sym ,x ,sto-x 0)))
	     (,(macroexpand-1 `(t/blas-scdi-func ,sym ,scal?))
	       (the index-type (size ,y))
	       ,sto-x ,stp-x
	       (the ,(store-type sym) (store ,y)) (the index-type ,st-y)
	       ,(if numx? 0 `(head ,x)) (head ,y))
	     ,y))))))

(deft/method t/scdi! (sym standard-tensor) (x y &key (scal? t) (numx? nil))
  (using-gensyms (decl (x y))
    (with-gensyms (sto-x sto-y of-x of-y idx)
      `(let (,@decl)
       (declare (type ,sym ,@(unless numx? `(,x)) ,y)
		,@(when numx? `((type ,(field-type sym) ,x))))
       (let (,@(unless numx? `((,sto-x (store ,x))))
	     (,sto-y (store ,y)))
	 (declare (type ,(store-type sym) ,@(unless numx? `(,sto-x)) ,sto-y))
	 (very-quickly
	   (mod-dotimes (,idx (dimensions ,y))
	     :with (linear-sums
		    ,@(unless numx? `((,of-x (strides ,x) (head ,x))))
		    (,of-y (strides ,y) (head ,y)))
	     :do (t/store-set ,sym (,(if scal? 't/f* 't/f/) ,(field-type sym)
				     (t/store-ref ,sym ,sto-y ,of-y)
				     ,@(if numx? `(,x) `((t/store-ref ,sym ,sto-x ,of-x))))
			      ,sto-y ,of-y))))
	 ,y))))
;;
(defgeneric scal! (alpha x)
  (:documentation
   "
  Syntax
  ======
  (SCAL! alpha x)

  Purpose
  =======
  X <- alpha .* X
")
  (:method :before ((x standard-tensor) (y standard-tensor))
	   (assert (very-quickly (lvec-eq (the index-store-vector (dimensions x)) (the index-store-vector (dimensions y)) #'=)) nil
		   'tensor-dimension-mismatch)))

(defmethod scal! ((x standard-tensor) (y standard-tensor))
  (let ((clx (class-name (class-of x)))
	(cly (class-name (class-of y))))
    (assert (and (member clx *tensor-type-leaves*)
		 (member cly *tensor-type-leaves*))
	    nil 'tensor-abstract-class :tensor-class (list clx cly))
    (cond
      ((eq clx cly)
       (compile-and-eval
	`(defmethod scal! ((x ,clx) (y ,cly))
	   ,(recursive-append
	     (when (subtypep clx 'blas-numeric-tensor)
	       `(if-let (strd (and (call-fortran? x (t/l1-lb ,clx)) (blas-copyablep x y)))
		  (t/blas-scdi! ,clx x (first strd) y (second strd) t)))
	     `(t/scdi! ,clx x y :scal? t :numx? nil))
	   y))
       (scal! x y))
      (t
       (error "Don't know how to apply scal! to classes ~a, ~a." clx cly)))))

(defmethod scal! ((x t) (y standard-tensor))
  (let ((cly (class-name (class-of y))))
    (assert (member cly *tensor-type-leaves*)
	    nil 'tensor-abstract-class :tensor-class cly)
    (compile-and-eval
     `(defmethod scal! ((x t) (y ,cly))
	(let ((x (t/coerce ,(field-type cly) x)))
	  (declare (type ,(field-type cly) x))
	  ,(recursive-append
	    (when (subtypep cly 'blas-numeric-tensor)
	      `(if-let (strd (and (call-fortran? y (t/l1-lb ,cly)) (consecutive-storep y)))
		 (t/blas-scdi! ,cly x nil y strd t)))
	    `(t/scdi! ,cly x y :scal? t :numx? t))
	  y)))
    (scal! x y)))

;;These should've auto-generated.
(defgeneric div! (alpha x)
  (:documentation
   "
  Syntax
  ======
  (DIV! alpha x)

  Purpose
  =======
  X <- X ./ alpha

  Yes the calling order is twisted.
")
  (:method :before ((x standard-tensor) (y standard-tensor))
	   (assert (very-quickly (lvec-eq (the index-store-vector (dimensions x)) (the index-store-vector (dimensions y)) #'=)) nil
		   'tensor-dimension-mismatch)))

(defmethod div! ((x standard-tensor) (y standard-tensor))
  (let ((clx (class-name (class-of x)))
	(cly (class-name (class-of y))))
    (assert (and (member clx *tensor-type-leaves*)
		 (member cly *tensor-type-leaves*))
	    nil 'tensor-abstract-class :tensor-class (list clx cly))
    (cond
      ((eq clx cly)
       (compile-and-eval
	`(defmethod div! ((x ,clx) (y ,cly))
	   ,(recursive-append
	     (when (subtypep clx 'blas-numeric-tensor)
	       `(if-let (strd (and (call-fortran? x (t/l1-lb ,clx)) (blas-copyablep x y)))
		  (t/blas-scdi! ,clx x (first strd) y (second strd) nil)))
	     `(t/scdi! ,clx x y :scal? nil :numx? nil))
	   y))
       (div! x y))
      (t
       (error "Don't know how to apply div! to classes ~a, ~a." clx cly)))))

(defmethod div! ((x t) (y standard-tensor))
  (let ((cly (class-name (class-of y))))
    (assert (member cly *tensor-type-leaves*)
	    nil 'tensor-abstract-class :tensor-class cly)
    (compile-and-eval
     `(defmethod div! ((x t) (y ,cly))
	(let ((x (t/coerce ,(field-type cly) x)))
	  (declare (type ,(field-type cly) x))
	  ,(recursive-append
	    (when (subtypep cly 'blas-numeric-tensor)
	      `(if-let (strd (and (call-fortran? y (t/l1-lb ,cly)) (consecutive-storep y)))
		 (t/blas-scdi! ,cly x nil y strd nil)))
	    `(t/scdi! ,cly x y :scal? nil :numx? t))
	  y)))
    (div! x y)))

;;
(defgeneric scal (alpha x)
  (:documentation
   "
  Syntax
  ======
  (SCAL alpha x)

  Purpose
  =======
  Computes and returns a new tensor equal to

             alpha .* X

  where alpha is a scalar and X is a tensor.

")
  (:method (alpha x)
     (scal! alpha (copy x)))
  ;;TODO: There is an issue here when x is not coerceable into the tensor class of alpha
  (:method ((alpha standard-tensor) (x t))
	   (scal! alpha (copy! x (zeros (dimensions alpha) (class-of alpha))))))

(defgeneric div (x y)
  (:documentation "
  Syntax
  ======
  (div! alpha x)

  Purpose
  =======
  X ./ alpha

  Yes the calling order is twisted.
")
  (:method (alpha x)
     (div! alpha (copy x)))
  ;;TODO: There is an issue here when x is not coerceable into the tensor class of alpha
  (:method ((alpha standard-tensor) (x t))
    (div! alpha (copy! x (zeros (dimensions alpha) (class-of alpha))))))
