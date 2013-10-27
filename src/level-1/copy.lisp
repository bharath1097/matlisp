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

(deft/generic (t/blas-copy-func #'subtypep) sym ())
(deft/method t/blas-copy-func (sym real-tensor) ()
  'dcopy)
(deft/method t/blas-copy-func (sym complex-tensor) ()
  'zcopy)
;;
(deft/generic (t/blas-copy! #'subtypep) sym (x st-x y st-y))
(deft/method t/blas-copy! (sym blas-numeric-tensor) (x st-x y st-y)
  (let ((ncp? (null st-x)))
    (using-gensyms (decl (x y))
      (with-gensyms (sto-x stp-x)
	`(let (,@decl)
	   (declare (type ,sym ,@(unless ncp? `(,x)) ,y)
		    ,@(when ncp? `((type ,(field-type sym) ,x))))
	   (let ((,sto-x ,(if ncp? `(t/store-allocator ,sym 1) `(store ,x)))
		 (,stp-x ,(if ncp? 0 st-x)))
	     (declare (type ,(store-type sym) ,sto-x)
		      (type index-type ,stp-x))
	     ,@(when ncp?
		     `((t/store-set ,sym ,x ,sto-x 0)))
	   (,(macroexpand-1 `(t/blas-copy-func ,sym))
	     (the index-type (size ,y))
	     (the ,(store-type sym) ,sto-x) (the index-type ,stp-x)
	     (the ,(store-type sym) (store ,y)) (the index-type ,st-y)
	     ,(if ncp? 0 `(head ,x)) (head ,y)))
	   ,y)))))
  
;;
(deft/generic (t/copy! #'(lambda (a b) (strict-compare (list #'subtypep #'subtypep) a b))) (clx cly) (x y))
(deft/method t/copy! ((clx standard-tensor) (cly standard-tensor)) (x y)
  (using-gensyms (decl (x y))
    (with-gensyms (sto-x sto-y of-x of-y idx)
      `(let* (,@decl
	      (,sto-x (store ,x))
	      (,sto-y (store ,y)))
	 (declare (type ,clx ,x)
		  (type ,cly ,y)
		  (type ,(store-type clx) ,sto-x)
		  (type ,(store-type cly) ,sto-y))
	 (very-quickly
	   (mod-dotimes (,idx (dimensions ,x))
	     :with (linear-sums
		    (,of-x (strides ,x) (head ,x))
		    (,of-y (strides ,y) (head ,y)))
	     :do (t/store-set ,cly
			      ,(recursive-append
				(unless (eq clx cly)
				  `(t/strict-coerce (,(field-type clx) ,(field-type cly)) ))
				`(t/store-ref ,clx ,sto-x ,of-x))
			      ,sto-y ,of-y)))
	 ,y))))

;;Coercion messes up optimization in SBCL, so we specialize.
(deft/method t/copy! ((clx real-numeric-tensor) (cly complex-numeric-tensor)) (x y)
  (using-gensyms (decl (x y))
    (with-gensyms (sto-x sto-y of-x of-y idx)
      `(let* (,@decl
	      (,sto-x (store ,x))
	      (,sto-y (store ,y)))
	 (declare (type ,clx ,x)
		  (type ,cly ,y)
		  (type ,(store-type clx) ,sto-x)
		  (type ,(store-type cly) ,sto-y))
	 (very-quickly
	   (mod-dotimes (,idx (dimensions ,x))
	     :with (linear-sums
		    (,of-x (strides ,x) (head ,x))
		    (,of-y (strides ,y) (head ,y)))
	     :do (t/store-set ,cly
			      (the ,(field-type cly) (complex (t/coerce ,(store-element-type cly) (t/store-ref ,clx ,sto-x ,of-x)) (t/fid+ ,(store-element-type cly))))
			      ,sto-y ,of-y)))
	 ,y))))

;;
(deft/method t/copy! ((clx t) (cly standard-tensor)) (x y)
  (using-gensyms (decl (x y))
    (with-gensyms (sto-y of-y idx cx)
      `(let* (,@decl
	      (,sto-y (store ,y))
	      (,cx (t/coerce ,(field-type cly) ,x)))
	 (declare (type ,cly ,y)
		  (type ,(field-type cly) ,cx)
		  (type ,(store-type cly) ,sto-y))
	 ;;This should be safe
	 (very-quickly
	   (mod-dotimes (,idx (dimensions ,y))
	     :with (linear-sums
		    (,of-y (strides ,y) (head ,y)))
	     :do (t/store-set ,cly ,cx ,sto-y ,of-y)))
	 ,y))))
;;
;;This macro is used for interfacing with lapack
;;Only to be used with matrices!

#|
(deft/generic (t/copy-triangle! #'subtypep) sym (a b &optional upper?))
(deft/method t/copy-triangle! (sym standard-tensor) (a b &optional (upper? t))
  (using-gensyms (decl (diag a b))
    (with-gensyms (sto-a sto-b strd-a strd-b dof-a dof-b of-a of-b)
      `(let* (,@decl
	      (,sto-a (store ,a))
	      (,strd-a (strides ,a))
	      (,sto-b (store ,b))
	      (,strd-b (strides ,b)))
       (declare (type ,sym ,a ,b)
		(type ,(store-type sym) ,sto-a ,sto-b)
		(type index-store-vector ,strd-a ,strd-b))
       (ecase ,diag
	 (t
           (with-marking 
	       (very-quickly
		 (:mark* ((ndiags (min (nrows ,a) (ncols ,a))))
			 (loop :for i :from 0 :below ndiags
			    :for ,dof-a :of-type index-type := (head ,a) :then (+ ,dof-a (:mark (lvec-foldr #'+ ,strd-a) :type index-type))
			    :for ,dof-b :of-type index-type := (head ,b) :then (+ ,dof-b (:mark (lvec-foldr #'+ ,strd-b) :type index-type))
			    :do (loop :for j :from 0 :below ,(if upper? `(1+ i) `(- ndiags i))
				   :for ,of-a :of-type index-type := ,dof-a :then (,(if upper? '- '+) ,of-a (:mark (aref ,strd-a 0)))
				   :for ,of-b :of-type index-type := ,dof-b :then (,(if upper? '- '+) ,of-b (:mark (aref ,strd-b 0)))
				   :do (t/store-set ,sym (t/store-ref ,sym ,sto-a ,of-a) ,sto-b ,of-b)))))))
	 
	   ,b))))
;;
(deft/generic (t/copy-triangle! #'subtypep) sym (a b &optional upper?))
(deft/method t/copy-triangle! (sym standard-tensor) (a b &optional (upper? t))
  (using-gensyms (decl (a b))
    (with-gensyms (sto-a sto-b strd-a strd-b dof-a dof-b of-a of-b)
      `(let* (,@decl
	      (,sto-a (store ,a))
	      (,strd-a (strides ,a))
	      (,sto-b (store ,b))
	      (,strd-b (strides ,b)))
       (declare (type ,sym ,a ,b)
		(type ,(store-type sym) ,sto-a ,sto-b)
		(type index-store-vector ,strd-a ,strd-b))
           (with-marking 
	       (very-quickly
		 (:mark* ((ndiags (min (nrows ,a) (ncols ,a))))
			 (loop :for i :from 0 :below ndiags
			    :for ,dof-a :of-type index-type := (head ,a) :then (+ ,dof-a (:mark (lvec-foldr #'+ ,strd-a) :type index-type))
			    :for ,dof-b :of-type index-type := (head ,b) :then (+ ,dof-b (:mark (lvec-foldr #'+ ,strd-b) :type index-type))
			    :do (loop :for j :from 0 :below ,(if upper? `(1+ i) `(- ndiags i))
				   :for ,of-a :of-type index-type := ,dof-a :then (,(if upper? '- '+) ,of-a (:mark (aref ,strd-a 0)))
				   :for ,of-b :of-type index-type := ,dof-b :then (,(if upper? '- '+) ,of-b (:mark (aref ,strd-b 0)))
				   :do (t/store-set ,sym (t/store-ref ,sym ,sto-a ,of-a) ,sto-b ,of-b))))))
	   ,b))))
;;
(deft/generic (t/copy-diagonal! #'subtypep) sym (a b &optional num?))
(deft/method t/copy-diagonal! (sym standard-tensor) (a b &optional (num? nil))
  (using-gensyms (decl (a b))
    (with-gensyms (sto-a sto-b of-a of-b)
      `(let* (,@decl
	      ,@(unless num? `((,sto-a (store ,a))))
	      (,sto-b (store ,b)))
	 (declare (type ,sym ,@(unless num? `(,a)) ,b)
		  (type ,(store-type sym) ,@(unless num? `(,sto-a)) ,sto-b)
		  ,@(when num? `((type ,(field-type sym) ,a))))
	 (with-marking 
	     (very-quickly
	       (:mark* ((ndiags (lvec-min (dimensions ,b))))
		       (loop :for i :from 0 :below ndiags
			  ,@(unless num? `(:for ,of-a :of-type index-type := (head ,a) :then (+ ,of-a (:mark (lvec-foldr #'+ (strides ,a)) :type index-type))))
			  :for ,of-b :of-type index-type := (head ,b) :then (+ ,of-b (:mark (lvec-foldr #'+ (strides ,b)) :type index-type))
			  :do (t/store-set ,sym ,@(if num? `(,a) `((t/store-ref ,sym ,sto-a ,of-a))) ,sto-b ,of-b)))))
	 ,b))))

;;
(defgeneric copy-triangle! (x y &key upper? diag?)
  (:method :before ((x standard-tensor) (y standard-tensor) &key upper? diag?)
	   (assert (and (tensor-matrixp x) (tensor-matrixp y)
			(= (lvec-min (dimensions x)) (lvec-min (dimensions y))))
		   nil 'tensor-dimension-mismatch)))


(defmethod copy-triangle! ((x standard-tensor) (y standard-tensor) &key (upper? t) (diag? t))
  (let ((clx (class-name (class-of x)))
	(cly (class-name (class-of y))))
    (assert (and (member clx *tensor-type-leaves*)
		 (member cly *tensor-type-leaves*)
		 (eql clx cly))
	    nil 'tensor-abstract-class :tensor-class (list clx cly))
    (compile-and-eval
     (let ((expr `())))
       `(defmethod copy-triangle! ((x ,clx) (y ,cly) &key (upper? t) (diag? t))
	  (ecase diag?
	    (t ;;copy diagonal
	     (if upper? (t/copy-triangle! ,clx x y t) (t/copy-triangle! ,clx x y nil)))
	    (number
	     (let ((num (t/coerce ,(t/field-type clx) diag?)))
	       (if upper? (t/copy-triangle! ,clx x y t) (t/copy-triangle! ,clx x y nil))
	       (t/copy-diagonal! ,clx num y t)))
	    (nil
	     (let ((num 

|#		    

;;
(defmethod copy! :before ((x standard-tensor) (y standard-tensor))
  (assert (very-quickly (lvec-eq (the index-store-vector (dimensions x)) (the index-store-vector (dimensions y)) #'=)) nil
	  'tensor-dimension-mismatch))

(defmethod copy! ((x standard-tensor) (y standard-tensor))
  (let ((clx (class-name (class-of x)))
	(cly (class-name (class-of y))))
    (assert (and (member clx *tensor-type-leaves*)
		 (member cly *tensor-type-leaves*))
	    nil 'tensor-abstract-class :tensor-class (list clx cly))
    (cond
      ((eq clx cly)
       (compile-and-eval
	`(defmethod copy! ((x ,clx) (y ,cly))
	   ,(recursive-append
	     (when (subtypep clx 'blas-numeric-tensor)
	       `(if-let (strd (and (call-fortran? x (t/l1-lb ,clx)) (blas-copyablep x y)))
		  (t/blas-copy! ,clx x (first strd) y (second strd))))
	     `(t/copy! (,clx ,cly) x y))
	   y)))
      ((coerceable? clx cly)
       (compile-and-eval
	`(defmethod copy! ((x ,clx) (y ,cly))
	   (t/copy! (,clx ,cly) x y)
	   y)))
      (t
       (error "Don't know how to copy from ~a to ~a" clx cly))))
  (copy! x y))

(defmethod copy! ((x t) (y standard-tensor))
  (let ((cly (class-name (class-of y))))
    (assert (and (member cly *tensor-type-leaves*))
	    nil 'tensor-abstract-class :tensor-class cly)
    (compile-and-eval
     `(defmethod copy! ((x t) (y ,cly))
	,(recursive-append
	  (when (subtypep cly 'blas-numeric-tensor)
	    `(if-let (strd (and (call-fortran? y (t/l1-lb ,cly)) (consecutive-storep y)))
	       (t/blas-copy! ,cly (t/coerce ,(field-type cly) x) nil y strd)))
	  `(t/copy! (t ,cly) x y))))
    (copy! x y)))

;;Generic function defined in src;base;generic-copy.lisp
(defmethod copy ((tensor standard-tensor))
  (let* ((ret (zeros (the index-store-vector (dimensions tensor)) (class-name (class-of tensor)))))
    (copy! tensor ret)))
