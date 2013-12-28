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

(deft/generic (t/blas-copy-func #'subfieldp) sym ())
(deft/method t/blas-copy-func (sym real-tensor) ()
  'dcopy)
(deft/method t/blas-copy-func (sym complex-tensor) ()
  'zcopy)
;;
(deft/generic (t/blas-copy! #'subtypep) sym (x st-x y st-y))
(deft/method t/blas-copy! (sym blas-numeric-tensor) (x st-x y st-y)
  (let ((ncp? (null st-x)))
    (using-gensyms (decl (x y))
      (with-gensyms (sto-x)
	`(let (,@decl)
	   (declare (type ,sym ,@(unless ncp? `(,x)) ,y)
		    ,@(when ncp? `((type ,(field-type sym) ,x))))
	   ,(recursive-append
	     (when ncp?
	       `(with-field-element ,sym (,sto-x ,x)))
	     `(,(macroexpand-1 `(t/blas-copy-func ,sym))
		(the index-type (size ,y))
		,(if ncp? sto-x `(the ,(store-type sym) (store ,x))) (the index-type ,(if ncp? 0 st-x))
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
(defmethod copy-generic ((tensor standard-tensor) type)
  (cond
    ((eql type 'array)
     (let ((ret (make-array (lvec->list (dimensions tensor)))))
       (copy! tensor ret)))
    ((member type '(list cons))
     (labels ((mtree (arr idx)
		(let ((n (length idx)))
		  (if (= n (rank arr)) (apply #'ref arr idx)
		      (loop :for i :from 0 :below (aref (dimensions arr) n)
			 :collect (mtree arr (append idx (list i))))))))
       (mtree tensor nil)))
    ((or (not type) (subtypep type 'standard-tensor))
     (let ((ret (zeros (dimensions tensor) (or type (class-of tensor)))))
       (copy! tensor ret)))))
