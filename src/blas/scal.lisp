t;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: :matlisp; Base: 10 -*-
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

;;
(deft/generic (t/blas-scdi! #'subfieldp) sym (x st-x y st-y &optional scal?))
(deft/method t/blas-scdi! (sym blas-numeric-tensor) (x st-x y st-y &optional (scal? t))
  (let ((numx? (null st-x)) (ftype (field-type sym)))
    (using-gensyms (decl (x y) (sto-x))
      `(let (,@decl)
	 (declare (type ,sym ,@(unless numx? `(,x)) ,y)
		  ,@(when numx? `((type ,(field-type sym) ,x))))
	 ,(recursive-append
	   (when numx? `(with-field-element ,sym (,sto-x ,x)))
	   `(ffuncall ,(blas-func (if scal? "escalm" "edivm") ftype)
	      (:& :integer) (the index-type (size ,y))
	      (:* ,(lisp->ffc ftype) ,@(unless numx? `(:+ (head ,x)))) ,(if numx? sto-x `(the ,(store-type sym) (store ,x)))
	      (:& :integer) (the index-type ,(if numx? 0 st-x))
	      (:* ,(lisp->ffc ftype) :+ (head ,y)) (the ,(store-type sym) (store ,y)) (:& :integer) (the index-type ,st-y)))
	 ,y))))

(deft/generic (t/scdi! #'subtypep) sym (x y &key scal? numx?))
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

(define-tensor-method scal! ((x standard-tensor :input) (y standard-tensor :output))
  (recursive-append
   (when (subtypep (cl x) 'blas-numeric-tensor)
     `(if-let (strd (and (call-fortran? x (t/l1-lb ,(cl x))) (blas-copyablep x y)))
	(t/blas-scdi! ,(cl x) x (first strd) y (second strd) t)))
   `(t/scdi! ,(cl x) x y :scal? t :numx? nil))
  'y)

(define-tensor-method scal! ((x t) (y standard-tensor :output))
  `(let ((x (t/coerce ,(field-type (cl y)) x)))
     (declare (type ,(field-type (cl y)) x))
     (unless (t/f= ,(field-type (cl y)) x (t/fid* ,(field-type (cl y))))
       ,(recursive-append
	 (when (subtypep (cl y) 'blas-numeric-tensor)
	   `(if-let (strd (and (call-fortran? y (t/l1-lb ,(cl y))) (consecutive-storep y)))
	      (t/blas-scdi! ,(cl y) x nil y strd t)))
	 `(t/scdi! ,(cl y) x y :scal? t :numx? t)))
     y))

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
  (:method ((alpha number) (x number))
    (* alpha x))
  (:method (alpha x)
     (scal! alpha (copy x)))
  ;;TODO: There is an issue here when x is not coerceable into the tensor class of alpha
  (:method ((alpha standard-tensor) (x t))
    ;;We assume commutation of course.
    (scal! x (copy alpha)))
  (:method ((alpha complex) (x real-numeric-tensor))
    (scal! alpha (copy x 'complex-tensor))))

;;These should've been auto-generated.
(defgeneric div! (alpha x)
  (:documentation
   "
  Syntax
  ======
  (DIV! alpha x)

s  Purpose
  =======
  X <- X ./ alpha

  Yes the calling order is twisted.
")
  (:method :before ((x standard-tensor) (y standard-tensor))
	   (assert (very-quickly (lvec-eq (the index-store-vector (dimensions x)) (the index-store-vector (dimensions y)) #'=)) nil
		   'tensor-dimension-mismatch)))

(define-tensor-method div! ((x standard-tensor :input) (y standard-tensor :output))  
  (recursive-append
   (when (subtypep (cl x) 'blas-numeric-tensor)
     `(if-let (strd (and (call-fortran? x (t/l1-lb ,(cl x))) (blas-copyablep x y)))
	(t/blas-scdi! ,(cl x) x (first strd) y (second strd) nil)))
   `(t/scdi! ,(cl x) x y :scal? nil :numx? nil))
  'y)

(define-tensor-method div! ((x t) (y standard-tensor :output))
  `(let ((x (t/coerce ,(field-type (cl y)) x)))     
     (declare (type ,(field-type (cl y)) x))
     (unless (t/f= ,(field-type (cl y)) x (t/fid* ,(field-type (cl y))))
       ,(recursive-append
	 (when (subtypep (cl y) 'blas-numeric-tensor)
	   `(if-let (strd (and (call-fortran? y (t/l1-lb ,(cl y))) (consecutive-storep y)))
	      (t/blas-scdi! ,(cl y) x nil y strd nil)))
	 `(t/scdi! ,(cl y) x y :scal? nil :numx? t)))
     y))

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
  (:method ((alpha number) (x number))
    (/ x alpha))
  (:method (alpha x)
     (div! alpha (copy x)))
  ;;TODO: There is an issue here when x is not coerceable into the tensor class of alpha
  (:method ((alpha standard-tensor) (x t))
    (div! alpha (copy! x (zeros (dimensions alpha) (class-of alpha))))))

;;Diagonal scaling.
(defgeneric scald! (x m &optional axis)
  (:method :before ((x standard-tensor) (m standard-tensor) &optional (axis 0))
	   (declare (type index-type axis))
	   (assert (and (tensor-vectorp x) (= (dimensions x 0) (dimensions m axis))) nil 'tensor-dimension-mismatch)))

(define-tensor-method scald! ((x standard-tensor :input) (m standard-tensor :output) &optional (axis 0))
  `(let-typed ((sto-m (store m) :type ,(store-type (cl m)))
	       (sto-x (store x) :type ,(store-type (cl x)))
	       (axis (modproj axis (order m))))
     (very-quickly
       (mod-dotimes (idx (dimensions m))
	 :with (linear-sums
		(of-m (strides m) (head m))
		(of-x (let ((tmp (allocate-index-store (order m))))
			(setf (aref tmp axis) (strides x 0))
			tmp)
		      (head x)))
	 :do (t/store-set ,(cl m) (t/f* ,(field-type (cl m)) (t/store-ref ,(cl x) sto-x of-x) (t/store-ref ,(cl m) sto-m of-m)) sto-m of-m))))
  'm)
