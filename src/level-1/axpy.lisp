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

(deft/generic (t/blas-axpy-func #'subtypep) sym ())
(deft/method t/blas-axpy-func (sym real-tensor) ()
  'daxpy)
(deft/method t/blas-axpy-func (sym complex-tensor) ()
  'zaxpy)
;;
    
(deft/generic (t/blas-axpy! #'subtypep) sym (sz a x st-x y st-y))
(deft/method t/blas-axpy! (sym blas-numeric-tensor) (sz a x st-x y st-y)
  (using-gensyms (decl (x y))
    `(let (,@decl)
       (declare (type ,sym ,x ,y))
       (,(macroexpand-1 `(t/blas-axpy-func ,sym))
	 (the index-type ,sz)
	 (the ,(field-type sym) ,a)
	 (the ,(store-type sym) (store ,x)) (the index-type ,st-x)
	 (the ,(store-type sym) (store ,y)) (the index-type ,st-y)
	 (head ,x) (head ,y))
       ,y)))

(deft/generic (t/blas-apy! #'subtypep) sym (sz a y st-y))
(deft/method t/blas-apy! (sym blas-numeric-tensor) (sz a y st-y)
  (using-gensyms (decl (a y))
   `(let (,@decl)
      (declare (type ,sym ,y)
	       (type ,(field-type sym) ,a))
	 (let ((sto-a (t/store-allocator ,sym 1)))
	   (declare (type ,(store-type sym) sto-a))
	   (t/store-set ,sym ,a sto-a 0)
	   (,(macroexpand-1 `(t/blas-axpy-func ,sym))
	     (the index-type ,sz)
	     (t/fid* ,(field-type sym))
	     (the ,(store-type sym) sto-a) 0
	     (the ,(store-type sym) (store ,y)) (the index-type ,st-y)
	     0 (head ,y)))
	 ,y)))

(deft/generic (t/axpy! #'subtypep) sym (a x y))
(deft/method t/axpy! (sym standard-tensor) (a x y)
  (using-gensyms (decl (a x y))
    `(let (,@decl)
       (declare (type ,sym ,x ,y)
		(type ,(field-type sym) ,a))
       (let ((sto-x (store ,x))
	     (sto-y (store ,y)))
	 (declare (type ,(store-type sym) sto-x sto-y))
	 (mod-dotimes (idx (dimensions ,x))
	   :with (linear-sums
		  (of-x (strides ,x) (head ,x))
		  (of-y (strides ,y) (head ,y)))
	   :do (t/store-set ,sym (t/f+ ,(field-type sym)
				       (t/f* ,(field-type sym)
					     ,a (t/store-ref ,sym sto-x of-x))
				       (t/store-ref ,sym sto-y of-y))
			    sto-y of-y)))
       ,y)))

(deft/generic (t/apy! #'subtypep) sym (a y))
(deft/method t/apy! (sym standard-tensor) (a y)
  (using-gensyms (decl (a y))
    `(let (,@decl)
       (declare (type ,sym ,y)
		(type ,(field-type sym) ,a))
       (let ((sto-y (store ,y)))
	 (declare (type ,(store-type sym) sto-y))
	 (mod-dotimes (idx (dimensions ,y))
	   :with (linear-sums
		  (of-y (strides ,y) (head ,y)))
	   :do (t/store-set ,sym (t/f+ ,(field-type sym)
				       ,a
				       (t/store-ref ,sym sto-y of-y))
			    sto-y of-y)))
       ,y)))
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
  (:method :before ((alpha number) (x standard-tensor) (y standard-tensor))
    (assert (lvec-eq (dimensions x) (dimensions y) #'=) nil
	    'tensor-dimension-mismatch))
  (:method ((alpha number) (x complex-tensor) (y real-tensor))
    (error 'coercion-error :from 'complex-tensor :to 'real-tensor)))

(defmethod axpy! ((alpha number) (x (eql nil)) (y real-tensor))
  (real-typed-num-axpy! (coerce-real alpha) y))

(defmethod axpy! ((alpha number) (x (eql nil)) (y complex-tensor))
  (complex-typed-num-axpy! (coerce-complex alpha) y))

(defmethod axpy! ((alpha number) (x real-tensor) (y real-tensor))
  (real-typed-axpy! (coerce-real alpha) x y))

(defmethod axpy! ((alpha number) (x real-tensor) (y complex-tensor))
  ;;Weird, shouldn't SBCL know this already ?
  (declare (type complex-tensor y))
  (let ((tmp (tensor-realpart~ y)))
    (declare (type real-tensor tmp))
    (etypecase alpha
      (cl:real (real-typed-axpy! (coerce-real alpha) x tmp))
      (cl:complex
       (real-typed-axpy! (coerce-real (realpart alpha)) x tmp)
       ;;Move tensor to the imagpart.
       (incf (head tmp))
       (real-typed-axpy! (coerce-real (realpart alpha)) x tmp))))
  y)

(defmethod axpy! ((alpha number) (x complex-tensor) (y complex-tensor))
  (complex-typed-axpy! (coerce-complex alpha) x y))

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
  (:method :before ((alpha number) (x standard-tensor) (y standard-tensor))
    (unless (lvec-eq (dimensions x) (dimensions y) #'=)
      (error 'tensor-dimension-mismatch))))

(defmethod axpy ((alpha number) (x real-tensor) (y real-tensor))
  (let ((ret (if (complexp alpha)
		 (copy! y (apply #'make-complex-tensor (lvec->list (dimensions y))))
		 (copy y))))
    (axpy! alpha x ret)))

(defmethod axpy ((alpha number) (x complex-tensor) (y real-tensor))
  (let ((ret (copy! y (apply #'make-complex-tensor (lvec->list (dimensions y))))))
    (axpy! alpha y ret)))

(defmethod axpy ((alpha number) (x real-tensor) (y complex-tensor))
  (let ((ret (copy y)))
    (axpy! alpha x ret)))

(defmethod axpy ((alpha number) (x complex-tensor) (y complex-tensor))
  (let ((ret (copy y)))
    (axpy! alpha x ret)))

(defmethod axpy ((alpha number) (x (eql nil)) (y complex-tensor))
  (let ((ret (copy y)))
    (axpy! alpha nil ret)))

(defmethod axpy ((alpha number) (x (eql nil)) (y real-tensor))
  (let ((ret (if (complexp alpha)
		 (copy! y (apply #'make-complex-tensor (lvec->list (dimensions y))))
		 (copy y))))
    (axpy! alpha nil ret)))

(defmethod axpy ((alpha number) (x standard-tensor) (y (eql nil)))
  (scal alpha x))
