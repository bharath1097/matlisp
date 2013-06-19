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
  (assert (lvec-eq (dimensions x) (dimensions y) #'=) nil
	  'tensor-dimension-mismatch))

;;This shouldn't happen ideally
(defmethod copy! ((x t) (y standard-tensor))
  (let ((clname (class-name (class-of y))))
    (assert (member clname *tensor-type-leaves*) nil 'tensor-abstract-class :tensor-class clname)
    (warn "copy! method being generated for '(t ~a), does not use BLAS." clname)
    (compile-and-eval
     `(defmethod copy! ((x t) (y ,clname))
	(let-typed ((sto-y (store y) :type (simple-array ,(store-element-type clname)))
		    (cx (t/coerce ,(field-type clname) x) :type ,(field-type clname)))
		   ;;This should be safe
		   (very-quickly
		     (mod-dotimes (idx (dimensions y))
		       :with (linear-sums
			      (of-y (strides y) (head y)))
		       :do (t/store-set ,clname cx sto-y of-y))))
	y))
    (copy! x y)))

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
		  
  
  (mod-dotimes (idx (dimensions x))
    do (setf (tensor-ref y idx) (tensor-ref x idx)))
  y)

(defmethod copy! ((x complex-tensor) (y real-tensor))
  (error 'coercion-error :from 'complex-tensor :to 'real-tensor))

(defmethod copy! ((x real-tensor) (y real-tensor))
  (real-typed-copy! x y))

(defmethod copy! ((x number) (y real-tensor))
  (real-typed-num-copy! (coerce-real x) y))

(defmethod copy! ((x complex-tensor) (y complex-tensor))
  (complex-typed-copy! x y))

(defmethod copy! ((x real-tensor) (y complex-tensor))
  ;;Borrowed from realimag.lisp
  (let ((tmp (make-instance 'real-tensor
			    :parent-tensor y :store (store y)
			    :dimensions (dimensions y)
			    :strides (map 'index-store-vector #'(lambda (n) (* 2 n)) (strides y))
			    :head (the index-type (* 2 (head y))))))
    (declare (type real-tensor tmp))
    (real-typed-copy! x tmp)
    ;;Increasing the head by 1 points us to the imaginary part.
    (incf (head tmp))
    (real-typed-num-copy! 0d0 tmp))
  y)

(defmethod copy! ((x number) (y complex-tensor))
  (complex-typed-num-copy! (coerce-complex x) y))

;; Copy between a Lisp array and a tensor
(defun convert-to-lisp-array (tensor)
  "
  Syntax
  ======
  (convert-to-lisp-array tensor)

  Purpose
  =======
  Create a new Lisp array with the same dimensions as the tensor and
  with the same elements.  This is a copy of the tensor.
"
  (declare (type standard-tensor tensor))
  (let*-typed ((dims (dimensions tensor) :type index-store-vector)
	       (ret (make-array (lvec->list dims)
				:element-type (or (getf (get-tensor-object-optimization tensor) :element-type)
						  (error 'tensor-cannot-find-optimization :tensor-class (class-name (class-of tensor)))))))
	      (let ((lst (make-list (rank tensor))))
		(very-quickly
		  (mod-dotimes (idx dims)
		    do (setf (apply #'aref ret (lvec->list! idx lst)) (tensor-ref tensor idx))))
		ret)))

(defmethod copy! :before ((x standard-tensor) (y array))
  (assert (subtypep (getf (get-tensor-object-optimization x) :element-type)
		    (array-element-type y))
	  nil 'invalid-type
	  :given (getf (get-tensor-object-optimization x) :element-type)
	  :expected (array-element-type y))
  (assert (and
	   (= (rank x) (array-rank y))
	   (dolist (ele (mapcar #'= (lvec->list (dimensions x)) (array-dimensions y)) t)
	     (unless ele (return nil))))
	  nil 'dimension-mismatch))

(defmethod copy! ((x real-tensor) (y array))
  (let-typed ((sto-x (store x) :type real-store-vector)
	      (lst (make-list (rank x)) :type cons))
	     (mod-dotimes (idx (dimensions x))
	       with (linear-sums
		     (of-x (strides x) (head x)))
	       do (setf (apply #'aref y (lvec->list! idx lst))
			(aref sto-x of-x))))
  y)

(defmethod copy! ((x complex-tensor) (y array))
  (let-typed ((sto-x (store x) :type complex-store-vector)
	      (lst (make-list (rank x)) :type cons))
	     (mod-dotimes (idx (dimensions x))
	       with (linear-sums
		     (of-x (strides x) (head x)))
	       do (setf (apply #'aref y (lvec->list! idx lst))
			(complex (aref sto-x (* 2 of-x)) (aref sto-x (1+ (* 2 of-x)))))))
  y)

;;
(defmethod copy! :before ((x array) (y standard-tensor))
  (assert (subtypep (array-element-type x)
		    (getf (get-tensor-object-optimization y) :element-type))
	  nil 'invalid-type
	  :given (array-element-type x) :expected (getf (get-tensor-object-optimization y) :element-type))
  (assert (and
	   (= (array-rank x) (rank y))
	   (dolist (ele (mapcar #'= (array-dimensions x) (lvec->list (dimensions y))) t)
	     (unless ele (return nil))))
	  nil 'dimension-mismatch))

(defmethod copy! ((x array) (y real-tensor))
  (let-typed ((sto-y (store y) :type real-store-vector)
	      (lst (make-list (array-rank x)) :type cons))
	     (very-quickly
	       (mod-dotimes (idx (dimensions y))
		 with (linear-sums
		       (of-y (strides y) (head y)))
		 do (setf (aref sto-y of-y) (apply #'aref x (lvec->list! idx lst))))))
  y)

(defmethod copy! ((x array) (y complex-tensor))
  (let-typed ((sto-y (store y) :type real-store-vector)
	      (lst (make-list (array-rank x)) :type cons))
	     (very-quickly
	       (mod-dotimes (idx (dimensions y))
		 with (linear-sums
		       (of-y (strides y) (head y)))
		 do (let-typed ((ele (apply #'aref x (lvec->list! idx lst)) :type complex-type))
			       (setf (aref sto-y (* 2 of-y)) (realpart ele)
				     (aref sto-y (1+ (* 2 of-y))) (imagpart ele))))))
  y)

;;
;;Generic function defined in src;base;generic-copy.lisp

(defmethod copy ((tensor real-tensor))
  (let* ((ret (apply #'make-real-tensor (lvec->list (dimensions tensor)))))
    (declare (type real-tensor ret))
    (copy! tensor ret)))

(defmethod copy ((tensor complex-tensor))
  (let* ((ret (apply #'make-complex-tensor (lvec->list (dimensions tensor)))))
    (declare (type complex-tensor ret))
    (copy! tensor ret)))

(defmethod copy ((tensor number))
  tensor)
