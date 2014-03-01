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

(deft/generic (t/blas-axpy-func #'subfieldp) sym ())
(deft/method t/blas-axpy-func (sym real-tensor) ()
  'daxpy)
(deft/method t/blas-axpy-func (sym sreal-tensor) ()
  'saxpy)
(deft/method t/blas-axpy-func (sym complex-tensor) ()
  'zaxpy)
(deft/method t/blas-axpy-func (sym scomplex-tensor) ()
  'caxpy)
;;
(deft/generic (t/blas-axpy! #'subtypep) sym (a x st-x y st-y))
(deft/method t/blas-axpy! (sym blas-numeric-tensor) (a x st-x y st-y)
  (let ((apy? (null x)))
    (using-gensyms (decl (a x y))
      (with-gensyms (sto-x)
	`(let (,@decl)
	   (declare (type ,sym ,@(unless apy? `(,x)) ,y)
		    ,@(when apy? `((ignore ,x))))
	   ,(recursive-append
	     (when apy?
	       `(with-field-element ,sym (,sto-x (t/fid* ,(field-type sym)))))
	     `(,(macroexpand-1 `(t/blas-axpy-func ,sym))
		(the index-type (size ,y))
		(the ,(field-type sym) ,a)
		,(if apy? sto-x `(the ,(store-type sym) (store ,x))) (the index-type ,(if apy? 0 st-x))
		(the ,(store-type sym) (store ,y)) (the index-type ,st-y)
		,(if apy? 0 `(head ,x)) (head ,y)))
	   ,y)))))

(deft/generic (t/axpy! #'subtypep) sym (a x y))
(deft/method t/axpy! (sym standard-tensor) (a x y)
  (let ((apy? (null x)))
    (using-gensyms (decl (a x y))
      (with-gensyms (idx sto-x sto-y of-x of-y)
	`(let (,@decl)
	   (declare (type ,sym ,@(unless apy? `(,x)) ,y)
		    (type ,(field-type sym) ,a)
		    ,@(when apy? `((ignore ,x))))
	   (let (,@(unless apy? `((,sto-x (store ,x))))
		 (,sto-y (store ,y)))
	     (declare (type ,(store-type sym) ,@(unless apy? `(,sto-x)) ,sto-y))
	     (very-quickly
	       (mod-dotimes (,idx (dimensions ,y))
		 :with (linear-sums
			,@(unless apy? `((,of-x (strides ,x) (head ,x))))
			(,of-y (strides ,y) (head ,y)))
		 :do (t/store-set ,sym (t/f+ ,(field-type sym)
					     ,@(if apy?
						   `(,a)
						   `((t/f* ,(field-type sym)
							   ,a (t/store-ref ,sym ,sto-x ,of-x))))
					     (t/store-ref ,sym ,sto-y ,of-y))
				  ,sto-y ,of-y)))
	     ,y))))))
;;---------------------------------------------------------------;;
(defgeneric axpy! (alpha x y)
  (:documentation
   " 
 Syntax
 ======
 (AXPY! alpha x y)

 Y <- alpha * x + y

 If x is T, then

 Y <- alpha + y

 Purpose
 =======
  Same as AXPY except that the result
  is stored in Y and Y is returned.
")
  (:method :before ((alpha number) (x base-tensor) (y base-tensor))
    (assert (lvec-eq (dimensions x) (dimensions y) #'=) nil
	    'tensor-dimension-mismatch)))

(define-tensor-method axpy! (alpha (x standard-tensor :input) (y standard-tensor :output))
  `(let ((alpha (t/coerce ,(field-type (cl x)) alpha)))
     (declare (type ,(field-type (cl x)) alpha))
     ,(recursive-append
       (when (subtypep (cl x) 'blas-numeric-tensor)
	 `(if-let (strd (and (call-fortran? x (t/l1-lb ,(cl x))) (blas-copyablep x y)))
	    (t/blas-axpy! ,(cl x) alpha x (first strd) y (second strd))))
       `(t/axpy! ,(cl x) alpha x y))
     y))

(define-tensor-method axpy! (alpha (x (eql nil)) (y standard-tensor :output))
  `(let ((alpha (t/coerce ,(field-type (cl y)) alpha)))
     (declare (type ,(field-type (cl y)) alpha))
     ,(recursive-append
       (when (subtypep (cl y) 'blas-numeric-tensor)
	 `(if-let (strd (and (call-fortran? y (t/l1-lb ,(cl y))) (consecutive-storep y)))
	    (t/blas-axpy! ,(cl y) alpha nil nil y strd)))
       `(t/axpy! ,(cl y) alpha nil y))
     y))
;;
(defgeneric axpy (alpha x y)
  (:documentation
   "
 Syntax
 ======
 (AXPY alpha x y)

 Purpose
 =======
 Computes  
      
                 ALPHA * X + Y

 where ALPHA is a scalar and X,Y are
 tensors.

 The result is stored in a new matrix 
 that has the same dimensions as Y.

 X,Y must have the same dimensions.
")
  (:method (alpha x (y standard-tensor))
    (axpy! alpha x (copy y))))

(defmethod axpy (alpha (x standard-tensor) (y (eql nil)))
  (axpy! alpha x (zeros (dimensions x) (class-of x))))

(defmethod axpy ((alpha complex) (x real-numeric-tensor) (y (eql nil)))
  (axpy! alpha x (zeros (dimensions x) 'complex-tensor)))
