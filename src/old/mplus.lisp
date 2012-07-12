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

(defgeneric m+ (a b)
  (:documentation
   "
  Syntax
  ======
  (M+ a b)

  Purpose
  =======
  Create a new matrix which is the sum of A and B.
  A or B (but not both) may be a scalar, in which
  case the addition is element-wise.
")
  (:method ((a number) (b number))
    (+ a b))
  (:method ((a standard-tensor) (b standard-tensor))
    (axpy 1 a b))
  (:method (

(definline m.+ (a b)
  "
  Syntax
  ======
  (M.+ a b)

  Purpose
  =======
  Same as M+
"
  (m+ a b))

(defgeneric m+! (a b)
  (:documentation
   "
  Syntax
  ======
  (M+! a b)

  Purpose
  =======
  Desctructive version of M+:

       B <- A + B
")
  (:method ((a number) (b number))
    (+ a b)))

(definline m.+! (a b)
  "
  Syntax
  ======
  (M.+! a b)

  Purpose
  =======
  Same as M+!
"
  (m+! a b))

(defmethod m+ :before ((a standard-matrix) (b standard-matrix))
  (let ((n-a (nrows a))
	(m-a (ncols a))
	(n-b (nrows b))
	(m-b (ncols b)))
    (declare (type fixnum n-a m-a n-b m-b))

    (unless (and (= n-a n-b)
		 (= m-a m-b))
      (error "Cannot add a ~d x ~d matrix and a ~d x ~d matrix"
	     n-a m-a
	     n-b m-b))))


(defmethod m+ ((a standard-matrix) (b standard-matrix))
  (axpy 1.0d0 a b))

(let ((b-array (make-array 1 :element-type 'real-matrix-element-type)))
  (defmethod m+ ((a real-matrix) (b #+(or cmu sbcl) double-float #-(or cmu sbcl) float))
    (let ((nxm (number-of-elements a))
	  (result (copy a)))
      (declare (type fixnum nxm))

      (setf (aref b-array 0) b)
      (daxpy nxm 1.0d0 b-array 0 (store result) 1)
      result)))

(defmethod m+ ((a real-matrix) (b cl:real))
  (m+ a (coerce b 'real-matrix-element-type)))

(defmethod m+ ((a #+(or cmu sbcl) double-float #-(or cmu sbcl) float) (b real-matrix))
  (m+ b a))

(defmethod m+ ((a cl:real) (b real-matrix))
  (m+ b (coerce a 'real-matrix-element-type)))

(defmethod m+ ((a real-matrix) (b #+:cmu kernel::complex-double-float
                                  #+:sbcl sb-kernel::complex-double-float
				  #-(or cmu sbcl) complex))
  (let* ((n (nrows a))
	 (m (ncols a))
	 #-(or cmu sbcl) (b (complex-coerce b))
	 (result (make-complex-matrix-dim n m b)))
    (declare (type fixnum n m))

    (axpy! 1.0d0 a result)))

#+(or :cmu :sbcl)
(defmethod m+ ((a real-matrix) (b complex))
  (m+ a (complex-coerce b)))

(defmethod m+ ((a #+:cmu kernel::complex-double-float
                  #+:sbcl sb-kernel::complex-double-float
		  #-(or cmu sbcl) complex) (b real-matrix))
  (m+ b a))

#+(or :cmu :sbcl)
(defmethod m+ ((a complex) (b real-matrix))
  (m+ b (complex-coerce a)))

;;;
(let ((b-array (make-array 1 :element-type 'real-matrix-element-type)))
  (defmethod m+ ((a complex-matrix) (b #+(or cmu sbcl) double-float #-(or cmu sbcl) float))
    (let ((nxm (number-of-elements a))
	  (result (copy a)))
      (declare (type fixnum nxm))

      (setf (aref b-array 0) b)
      (daxpy nxm 1.0d0 b-array 0 (store result) 2)
      result)))

(defmethod m+ ((a complex-matrix) (b cl:real))
  (m+ a (coerce b 'complex-matrix-element-type)))

(defmethod m+ ((a #+(or cmu sbcl) double-float #-(or cmu sbcl) float) (b complex-matrix))
  (m+ b a))

(defmethod m+ ((a cl:real) (b complex-matrix))
  (m+ b (coerce a 'complex-matrix-element-type)))

(defmethod m+ ((a complex-matrix) (b #+:cmu kernel::complex-double-float
                                     #+:sbcl sb-kernel::complex-double-float
				     #-(or cmu sbcl) complex))
  (let* ((n (nrows a))
	 (m (ncols a))
	 #-(or cmu sbcl) (b (complex-coerce b))
	 (result (make-complex-matrix-dim n m b)))
    (declare (type fixnum n m))

    (axpy! 1.0d0 a result)))

#+(or :cmu :sbcl)
(defmethod m+ ((a complex-matrix) (b complex))
  (m+ a (complex-coerce b)))

(defmethod m+ ((a #+:cmu kernel::complex-double-float
                  #+:sbcl sb-kernel::complex-double-float
		  #-(or cmu sbcl) complex) (b complex-matrix))
  (m+ b a))

#+(or :cmu :sbcl)
(defmethod m+ ((a complex) (b complex-matrix))
  (m+ b (complex-coerce a)))


(defmethod m+! :before ((a standard-matrix) (b standard-matrix))
  (let ((n-a (nrows a))
	(m-a (ncols a))
	(n-b (nrows b))
	(m-b (ncols b)))
    (declare (type fixnum n-a m-a n-b m-b))
    (unless (and (= n-a n-b)
		 (= m-a m-b))
      (error "Cannot add a ~d x ~d matrix and a ~d x ~d matrix"
	     n-a m-a
	     n-b m-b))))

(defmethod m+! ((a standard-matrix) (b standard-matrix))
  (axpy! 1.0d0 a b))

(defmethod m+! ((a complex-matrix) (b real-matrix))
  (error "cannot M+! a COMPLEX-MATRIX A and a REAL-MATRIX B,
don't know how to coerce COMPLEX to REAL."))

;;;
(let ((b-array (make-array 1 :element-type 'real-matrix-element-type)))
  (defmethod m+! ((a real-matrix) (b #+(or cmu sbcl) double-float #-(or cmu sbcl) float))
    (let ((nxm (number-of-elements a)))
      (declare (type fixnum nxm))

      (setf (aref b-array 0) b)
      (daxpy nxm 1.0d0 b-array 0 (store a) 1)
      a)))

(defmethod m+! ((a real-matrix) (b cl:real))
  (m+! a (coerce b 'real-matrix-element-type)))

(defmethod m+! ((a #+(or cmu sbcl) double-float #-(or cmu sbcl) float) (b real-matrix))
  (m+! b a))

(defmethod m+! ((a cl:real) (b real-matrix))
  (m+! b (coerce a 'real-matrix-element-type)))

(defmethod m+! ((a real-matrix) (b complex))
  (error "cannon M+! a REAL-MATRIX and a COMPLEX,
don't know how to coerce COMPLEX to REAL"))

(defmethod m+! ((a complex) (b real-matrix))
  (error "cannon M+! a REAL-MATRIX and a COMPLEX,
don't know how to coerce COMPLEX to REAL"))

(let ((b-array (make-array 1 :element-type 'real-matrix-element-type)))
  (defmethod m+! ((a complex-matrix) (b #+(or cmu sbcl) double-float #-(or cmu sbcl) float))
    (let ((nxm (number-of-elements a)))
      (declare (type fixnum nxm))

      (setf (aref b-array 0) b)
      (daxpy nxm 1.0d0 b-array 0 (store a) 2)
      a)))

(defmethod m+! ((a complex-matrix) (b cl:real))
  (m+! a (coerce b 'complex-matrix-element-type)))

(defmethod m+! ((a #+(or cmu sbcl) double-float #-(or cmu sbcl) float) (b complex-matrix))
  (m+! b a))

(defmethod m+! ((a cl:real) (b complex-matrix))
  (m+! b (coerce a 'complex-matrix-element-type)))

#-:sbcl  ;; sbcl doesn't like constant arrays
(defconstant *complex-unity-as-array* 
  (make-array 2 :element-type 'complex-matrix-element-type
	      :initial-contents '(1.0d0 0.0d0)))

#+:sbcl
(defvar *complex-unity-as-array* 
  (make-array 2 :element-type 'complex-matrix-element-type
	      :initial-contents '(1.0d0 0.0d0)))

(defmethod m+! ((a complex-matrix) (b #+:cmu kernel::complex-double-float
                                      #+:sbcl sb-kernel::complex-double-float
				      #-(or cmu sbcl) complex))
  (let* ((nxm (number-of-elements a)))
    (declare (type fixnum nxm))

    #-(or cmu sbcl) (setq b (complex-coerce b))

    (setf (aref *1x1-complex-array* 0) (realpart b))
    (setf (aref *1x1-complex-array* 1) (imagpart b))
    (zaxpy nxm #c(1d0 0) b 0 (store a) 1)
    a))

#+(or :cmu :sbcl)
(defmethod m+! ((a complex-matrix) (b complex))
  (m+! a (complex-coerce b)))

(defmethod m+! ((a #+:cmu kernel::complex-double-float
                   #+:sbcl sb-kernel::complex-double-float
		   #-(or cmu sbcl) complex) (b complex-matrix))
  (m+! b a))

#+(or :cmu :sbcl)
(defmethod m+! ((a complex) (b complex-matrix))
  (m+! b (complex-coerce a)))

