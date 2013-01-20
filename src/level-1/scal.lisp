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

(defmacro generate-typed-scal! (func (tensor-class fortran-func fortran-lb))
  (let* ((opt (get-tensor-class-optimization-hashtable tensor-class)))
    (assert opt nil 'tensor-cannot-find-optimization :tensor-class tensor-class)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let ((opt (get-tensor-class-optimization-hashtable ',tensor-class)))
	 (assert opt nil 'tensor-cannot-find-optimization :tensor-class ',tensor-class)
	 (setf (getf opt :scal) ',func
	       (get-tensor-class-optimization ',tensor-class) opt))
       (defun ,func (from to)
	 (declare (type ,tensor-class from to))
	 ,(let
	      ((lisp-routine
		 `(let ((f-sto (store from))
			(t-sto (store to)))
		    (declare (type ,(linear-array-type (getf opt :store-type)) f-sto t-sto))
		    (very-quickly
		      (mod-dotimes (idx (dimensions from))
			with (linear-sums
			      (f-of (strides from) (head from))
			      (t-of (strides to) (head to)))
			do (let*-typed ((val-f (,(getf opt :reader) f-sto f-of) :type ,(getf opt :element-type))
					(val-t (,(getf opt :reader) t-sto t-of) :type ,(getf opt :element-type))
					(mul (,(getf opt :f*) val-f val-t) :type ,(getf opt :element-type)))
				       (,(getf opt :value-writer) mul t-sto t-of)))))))
	    (if fortran-func
		`(let* ((call-fortran? (> (number-of-elements to) ,fortran-lb))
			(strd-p (when call-fortran? (blas-copyable-p from to))))
		   (cond
		     ((and strd-p call-fortran?)
		      (,fortran-func (number-of-elements from)
				     (store from) (first strd-p)
				     (store to) (second strd-p)
				     (head from) (head to)))
		     (t
		      ,lisp-routine)))
		lisp-routine))
	 to))))

(defmacro generate-typed-num-scal! (func (tensor-class blas-func fortran-lb))
  (let ((opt (get-tensor-class-optimization-hashtable tensor-class)))
    (assert opt nil 'tensor-cannot-find-optimization :tensor-class tensor-class)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let ((opt (get-tensor-class-optimization-hashtable ',tensor-class)))
	 (assert opt nil 'tensor-cannot-find-optimization :tensor-class ',tensor-class)
	 (setf (getf opt :num-scal) ',func
	       (get-tensor-class-optimization ',tensor-class) opt))    
       (defun ,func (alpha to)
	 (declare (type ,tensor-class to)
		  (type ,(getf opt :element-type) alpha))
	 ,(let
	      ((lisp-routine
		 `(let ((t-sto (store to)))
		    (declare (type ,(linear-array-type (getf opt :store-type)) t-sto))
		    (very-quickly
		      (mod-dotimes (idx (dimensions to))
			with (linear-sums
			      (t-of (strides to) (head to)))
			do (let ((scal-val (,(getf opt :f*) (,(getf opt :reader) t-sto t-of) alpha)))
			     (,(getf opt :value-writer) scal-val t-sto t-of)))))))
	    (if blas-func
		`(let* ((call-fortran? (> (number-of-elements to) ,fortran-lb))
			(min-stride (when call-fortran? (consecutive-store-p to))))
		   (cond
		     ((and call-fortran? min-stride)
		      (,blas-func (number-of-elements to) alpha (store to) min-stride (head to)))
		     (t
		      ,lisp-routine)))
		lisp-routine))
	 to))))

(defmacro generate-typed-div! (func (tensor-class fortran-func fortran-lb))
  (let* ((opt (get-tensor-class-optimization-hashtable tensor-class)))
    (assert opt nil 'tensor-cannot-find-optimization :tensor-class tensor-class)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let ((opt (get-tensor-class-optimization-hashtable ',tensor-class)))
	 (assert opt nil 'tensor-cannot-find-optimization :tensor-class ',tensor-class)
	 (setf (getf opt :div) ',func
	       (get-tensor-class-optimization ',tensor-class) opt))    
       (defun ,func (from to)
	 (declare (type ,tensor-class from to))
	 ,(let
	      ((lisp-routine
		 `(let ((f-sto (store from))
			(t-sto (store to)))
		    (declare (type ,(linear-array-type (getf opt :store-type)) f-sto t-sto))
		    (very-quickly
		      (mod-dotimes (idx (dimensions from))
			with (linear-sums
			      (f-of (strides from) (head from))
			      (t-of (strides to) (head to)))
			do (let*-typed ((val-f (,(getf opt :reader) f-sto f-of) :type ,(getf opt :element-type))
					(val-t (,(getf opt :reader) t-sto t-of) :type ,(getf opt :element-type))
					(mul (,(getf opt :f/) val-f val-t) :type ,(getf opt :element-type)))
				       (,(getf opt :value-writer) mul t-sto t-of)))))))
	    (if fortran-func	     
		`(let* ((call-fortran? (> (number-of-elements to) ,fortran-lb))
			(strd-p (when call-fortran? (blas-copyable-p from to))))
		   (cond
		     ((and strd-p call-fortran?)
		      (,fortran-func (number-of-elements from)
				     (store from) (first strd-p)
				     (store to) (second strd-p)
				     (head from) (head to)))
		     (t
		      ,lisp-routine)))
		lisp-routine))
	 to))))

(defmacro generate-typed-num-div! (func (tensor-class fortran-func fortran-lb))
  (let ((opt (get-tensor-class-optimization tensor-class)))
    (assert opt nil 'tensor-cannot-find-optimization :tensor-class tensor-class)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let ((opt (get-tensor-class-optimization-hashtable ',tensor-class)))
	 (assert opt nil 'tensor-cannot-find-optimization :tensor-class ',tensor-class)
	 (setf (getf opt :num-div) ',func
	       (get-tensor-class-optimization ',tensor-class) opt))
       (defun ,func (alpha to)
	 (declare (type ,tensor-class to)
		  (type ,(getf opt :element-type) alpha))
	 ,(let
	      ((lisp-routine
		 `(let ((t-sto (store to)))
		    (declare (type ,(linear-array-type (getf opt :store-type)) t-sto))
		    (very-quickly
		      (mod-dotimes (idx (dimensions to))
			with (linear-sums
			      (t-of (strides to) (head to)))
			do (let-typed ((scal-val (,(getf opt :f/) alpha (,(getf opt :reader) t-sto t-of)) :type ,(getf opt :element-type)))
				      (,(getf opt :value-writer) scal-val t-sto t-of)))))))
	    (if fortran-func
		`(let* ((call-fortran? (> (number-of-elements to) ,fortran-lb))
			(min-stride (when call-fortran? (consecutive-store-p to))))
		   (cond
		     ((and call-fortran? min-stride)
		      (let ((num-array (,(getf opt :store-allocator) 1)))
			(declare (type ,(linear-array-type (getf opt :store-type)) num-array))
			(let-typed ((id (,(getf opt :fid*)) :type ,(getf opt :element-type)))
				   (,(getf opt :value-writer) id num-array 0))
			(,fortran-func (number-of-elements to) num-array 0 (store to) min-stride (head to))))
		     (t
		      ,lisp-routine)))
		lisp-routine))
	 to))))

;;Real
(generate-typed-num-scal! real-typed-num-scal!
    (real-tensor dscal *real-l1-fcall-lb*))

(generate-typed-scal! real-typed-scal!
    (real-tensor descal *real-l1-fcall-lb*))

(generate-typed-div! real-typed-div!
    (real-tensor dediv *real-l1-fcall-lb*))

(generate-typed-num-div! real-typed-num-div!
    (real-tensor dediv *real-l1-fcall-lb*))

;;Complex
(definline zordscal (nele alpha x incx &optional hd-x)
  (if (zerop (imagpart alpha))
      (zdscal nele (realpart alpha) x incx hd-x)
      (zscal nele alpha x incx hd-x)))

(generate-typed-num-scal! complex-typed-num-scal!
    (complex-tensor zordscal *complex-l1-fcall-lb*))

(generate-typed-scal! complex-typed-scal!
    (complex-tensor zescal *complex-l1-fcall-lb*))

(generate-typed-div! complex-typed-div!
    (complex-tensor zediv *complex-l1-fcall-lb*))

(generate-typed-num-div! complex-typed-num-div!
    (complex-tensor zediv *complex-l1-fcall-lb*))

;;Symbolic
#+maxima
(progn
  (generate-typed-num-scal! symbolic-typed-num-scal!
      (symbolic-tensor nil 0))

  (generate-typed-scal! symbolic-typed-scal!
      (symbolic-tensor nil 0))

  (generate-typed-div! symbolic-typed-div!
      (symbolic-tensor nil 0))

  (generate-typed-num-div! symbolic-typed-num-div!
      (symbolic-tensor nil 0)))
;;---------------------------------------------------------------;;

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
    (assert (lvec-eq (dimensions x) (dimensions y) #'=) nil
	    'tensor-dimension-mismatch)))

(defmethod scal! ((alpha number) (x real-tensor))
  (real-typed-num-scal! (coerce-real alpha) x))

(defmethod scal! ((x real-tensor) (y real-tensor))
  (real-typed-scal! x y))

(defmethod scal! ((alpha number) (x complex-tensor))
  (complex-typed-num-scal! (coerce-complex alpha) x))

(defmethod scal! ((x complex-tensor) (y complex-tensor))
  (complex-typed-scal! x y))

(defmethod scal! ((x real-tensor) (y complex-tensor))
  (let ((tmp (tensor-realpart~ y)))
    (real-typed-scal! x tmp)
    ;;Move view to the imaginary part
    (incf (head tmp))
    (real-typed-scal! x tmp)))

;;
(defgeneric div! (alpha x)
  (:documentation "
  Syntax
  ======
  (div! alpha x)

  Purpose
  =======
  X <- alpha ./ X
")
  (:method :before ((x standard-tensor) (y standard-tensor))
    (assert (lvec-eq (dimensions x) (dimensions y) #'=) nil
	    'tensor-dimension-mismatch)))

(defmethod div! ((alpha number) (x real-tensor))
  (real-typed-num-div! (coerce-real alpha) x))

(defmethod div! ((x real-tensor) (y real-tensor))
  (real-typed-div! x y))

(defmethod div! ((alpha number) (x complex-tensor))
  (complex-typed-num-div! (coerce-complex alpha) x))

(defmethod div! ((x complex-tensor) (y complex-tensor))
  (complex-typed-div! x y))

(defmethod div! ((x real-tensor) (y complex-tensor))
  ;;The alternative is worse!
  (let ((tmp (copy! x (apply #'make-complex-tensor (lvec->list (dimensions x))))))
    (complex-typed-div! tmp y)))

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

"))

(defmethod scal ((alpha number) (x number))
  (* alpha x))

(defmethod scal ((x standard-tensor) (alpha number))
  (scal alpha x))

(defmethod scal ((alpha number) (x real-tensor))
  (let ((result (if (complexp alpha)
		    (copy! x (apply #'make-complex-tensor (lvec->list (dimensions x))))
		    (copy x))))
    (scal! alpha result)))

(defmethod scal ((x real-tensor) (y real-tensor))
  (scal! x (copy y)))

(defmethod scal ((x complex-tensor) (y real-tensor))
  (let ((result (copy! y (apply #'make-complex-tensor (lvec->list (dimensions x))))))
    (scal! x result)))

(defmethod scal ((alpha number) (x complex-tensor))
  (let ((result (copy x)))
    (scal! alpha result)))

(defmethod scal ((x real-tensor) (y complex-tensor))
  (let ((result (copy y)))
    (scal! x result)))

(defmethod scal ((x complex-tensor) (y complex-tensor))
  (let ((result (copy y)))
    (scal! x result)))

;;
(defgeneric div (x y)
  (:documentation "
  Syntax
  ======
  (div! alpha x)

  Purpose
  =======
  alpha ./ X

  Yes the calling order is twisted.
"))  

(defmethod div ((alpha number) (x number))
  (/ x alpha))

(defmethod div ((x standard-tensor) (y number))
  (let ((result (copy x)))
    (scal! (/ 1 y) result)))

(defmethod div ((x (eql nil)) (y standard-tensor))
  (let ((result (copy y)))
    (div! 1 result)))

(defmethod div ((x real-tensor) (y real-tensor))
  (div! x (copy y)))

(defmethod div ((alpha number) (x real-tensor))
  (let ((result (if (complexp alpha)
		    (copy! x (apply #'make-complex-tensor (lvec->list (dimensions x))))
		    (copy x))))
    (div! alpha result)))

(defmethod div ((x complex-tensor) (y real-tensor))
  (let ((result (copy! y (apply #'make-complex-tensor (lvec->list (dimensions x))))))
    (div! x result)))

(defmethod div ((alpha number) (x complex-tensor))
  (let ((result (copy x)))
    (div! alpha result)))

(defmethod div ((x real-tensor) (y complex-tensor))
  (let ((result (copy y)))
    (div! x result)))

(defmethod div ((x complex-tensor) (y complex-tensor))
  (let ((result (copy y)))
    (div! x result)))
