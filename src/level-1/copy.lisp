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
(deft/generic (t/blas-copy! #'subtypep) sym (sz x st-x y st-y))
(deft/method t/blas-copy! (sym blas-numeric-tensor) (sz x st-x y st-y)
  (let* ((decl (zipsym (list x y)))
	 (args (mapcar #'car decl))
	 (func (macroexpand-1 `(t/blas-copy-func ,sym))))
    (let ((x (first args)) (y (second args)))
      `(let (,@decl)
	 (declare (type ,sym ,@args))
	 (,func
	  (the index-type ,sz)
	  (the ,(store-type sym) (store ,x)) (the index-type ,st-x)
	  (the ,(store-type sym) (store ,y)) (the index-type ,st-y)
	  (head ,x) (head ,y))
	 ,y))))

(deft/generic (t/blas-num-copy! #'subtypep) sym (sz x y st-y))
(deft/method t/blas-num-copy! (sym blas-numeric-tensor) (sz x y st-y)
  (let* ((decl (zipsym (list x y)))
	 (args (mapcar #'car decl))
	 (func (macroexpand-1 `(t/blas-copy-func ,sym))))
    (let ((x (first args)) (y (second args)))
      `(let (,@decl)
	 (declare (type ,sym ,y)
		  (type ,(field-type sym) ,x))
	 (let ((sto-x (t/store-allocator ,sym 1)))
	   (declare (type ,(store-type sym) sto-x))
	   (t/store-set ,sym ,x sto-x 0)
	   (,func
	    (the index-type ,sz)
	    (the ,(store-type sym) sto-x) 0
	    (the ,(store-type sym) (store ,y)) (the index-type ,st-y)
	    0 (head ,y)))
	   ,y))))

;;
(deft/generic (t/copy! #'(lambda (a b) (strict-compare (list #'subtypep #'subtypep) a b))) (clx cly) (x y))
(deft/method t/copy! ((clx standard-tensor) (cly standard-tensor)) (x y)
  (let* ((decl (zipsym (list x y)))
	 (args (mapcar #'car decl)))
    (let ((x (first args)) (y (second args)))
      `(let* (,@decl
	      (sto-x (store ,x))
	      (sto-y (store ,y)))
	 (declare (type ,clx ,(first args))
		  (type ,cly ,(second args))
		  (type ,(store-type clx) sto-x)
		  (type ,(store-type cly) sto-y))
	 (mod-dotimes (idx (dimensions ,x))
	   :with (linear-sums
		  (of-x (strides ,x) (head ,x))
		  (of-y (strides ,y) (head ,y)))
	   :do (t/store-set ,cly
			    ,(recursive-append
			      (unless (eq clx cly)
				`(t/strict-coerce (,(field-type clx) ,(field-type cly)) ))
			      `(t/store-ref ,clx sto-x of-x))
			    sto-y of-y))
	 ,y))))

;;Coercion messes up optimization in SBCL, so we specialize.
(deft/method t/copy! ((clx real-numeric-tensor) (cly complex-numeric-tensor)) (x y)
  (let* ((decl (zipsym (list x y)))
	 (args (mapcar #'car decl)))
    (let ((x (first args)) (y (second args)))
      `(let* (,@decl
	      (sto-x (store ,x))
	      (sto-y (store ,y)))
	 (declare (type ,clx ,(first args))
		  (type ,cly ,(second args))
		  (type ,(store-type clx) sto-x)
		  (type ,(store-type cly) sto-y))
	 (mod-dotimes (idx (dimensions ,x))
	   :with (linear-sums
		  (of-x (strides ,x) (head ,x))
		  (of-y (strides ,y) (head ,y)))
	   :do (t/store-set ,cly
			    (the ,(field-type cly) (complex (t/coerce ,(store-element-type cly) (t/store-ref ,clx sto-x of-x)) (t/fid+ ,(store-element-type cly))))
			    sto-y of-y))
	 ,y))))

;;
(deft/method t/copy! ((clx t) (cly standard-tensor)) (x y)
  (let* ((decl (zipsym (list x y)))
	 (args (mapcar #'car decl)))
    (let ((x (first args)) (y (second args)))
      `(let* (,@decl
	      (sto-y (store ,y))
	      (cx (t/coerce ,(field-type cly) ,x)))
	 (declare (type ,cly ,(second args))
		  (type ,(field-type cly) cx)
		  (type ,(store-type cly) sto-y))
	 ;;This should be safe
	 (mod-dotimes (idx (dimensions ,y))
	   :with (linear-sums
		  (of-y (strides ,y) (head ,y)))
	   :do (t/store-set ,cly cx sto-y of-y))
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
    (if (eq clx cly)
	(progn
	  (compile-and-eval
	   `(defmethod copy! ((x ,clx) (y ,cly))
	      ,(recursive-append
		(when (subtypep clx 'blas-numeric-tensor)
		  `(if-let (strd (and (call-fortran? x (t/l1-lb ,clx)) (blas-copyablep x y)))
		     (let ((sz (size x))) (t/blas-copy! ,clx sz x (first strd) y (second strd)))))
		`(very-quickly (t/copy! (,clx ,cly) x y)))
	      y)))
	(compile-and-eval
	 `(defmethod copy! ((x ,clx) (y ,cly))
	    (t/copy! (,clx ,cly) x y)
	    y)))
    (copy! x y)))

(defmethod copy! ((x t) (y standard-tensor))
  (let ((cly (class-name (class-of y))))
    (assert (and (member cly *tensor-type-leaves*))
	    nil 'tensor-abstract-class :tensor-class cly)
    (compile-and-eval
     `(defmethod copy! ((x t) (y ,cly))
	,(recursive-append
	  (when (subtypep cly 'blas-numeric-tensor)
	    `(if-let (strd (and (call-fortran? y (t/l1-lb ,cly)) (consecutive-storep y)))
	       	(let ((sz (size y))) (t/blas-num-copy! ,cly sz x y strd))))
	  `(very-quickly (t/copy! (t ,cly) x y)))))
    (copy! x y)))

;;Generic function defined in src;base;generic-copy.lisp
(defmethod copy ((tensor standard-tensor))
  (let* ((ret (zeros (the index-store-vector (dimensions tensor)) (class-name (class-of tensor)))))
    (copy! tensor ret)))
