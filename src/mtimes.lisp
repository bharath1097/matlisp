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
;;;
;;; $Id: mtimes.lisp,v 1.1 2000/04/14 00:11:12 simsek Exp $
;;;
;;; $Log: mtimes.lisp,v $
;;; Revision 1.1  2000/04/14 00:11:12  simsek
;;; o This file is adapted from obsolete files 'matrix-float.lisp'
;;;   'matrix-complex.lisp' and 'matrix-extra.lisp'
;;; o Initial revision.
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MATLISP")

(use-package "BLAS")
(use-package "LAPACK")
(use-package "FORTRAN-FFI-ACCESSORS")

(export '(m*
	  m*!
	  m.*
	  m.*!
	  set-m*!-swap-size))


(defgeneric m* (a b)
  (:documentation
   "
  Syntax
  ======
  (M* a b)

  Purpose
  =======
  Create a new matrix which is the product of A and B.
  A and/or B  may be scalars, in which
  case the multiplication is element-wise.
"))

(defgeneric m*! (a b)
  (:documentation
   "
  Syntax
  ======
  (M*! a b)

  Purpose
  =======
  Desctructive version of M*:

       B <- A * B
"))

(defgeneric m.* (a b)
  (:documentation
   "
  Syntax
  ======
  (M.* a b)

  Purpose
  =======
  Create a new matrix which is the element by
  element product of A and B.

  A and/or B may be scalars.

  If A,B are matrices they need not be of the
  same dimension, however, they must have the
  same number of elements.
"))

(defgeneric m.*! (a b)
  (:documentation
   "
  Syntax
  ======
  (M.*! a b)

  Purpose
  =======
  Desctructive version of M.*:

       B <- A .* B
"))

;;;;;

(defmethod m.* :before ((a standard-matrix) (b standard-matrix))
  (let ((nxm-a (nxm a))
	(nxm-b (nxm b)))
    (declare (type fixnum nxm-a nxm-b))
    (unless (= nxm-a nxm-b)
      (error "arguments A,B given to M.* are not the same size"))))

(defmethod m.*! :before ((a standard-matrix) (b standard-matrix))
  (let ((nxm-a (nxm a))
	(nxm-b (nxm b)))
    (declare (type fixnum nxm-a nxm-b))
    (unless (= nxm-a nxm-b)
      (error "arguments A,B given to M.* are not the same size"))))

  
(defmethod m.* ((a real-matrix) (b real-matrix))
  (let* ((n (n b))
	 (m (m b))
	 (nxm (nxm b))
	 (result (make-real-matrix-dim n m)))
    (declare (type fixnum n m nxm))

    (dotimes (k nxm result)
      (declare (type fixnum k))
      (let ((a-val (matrix-ref a k))
	    (b-val (matrix-ref b k)))
	(declare (type real-matrix-element-type a-val b-val))
	(setf (matrix-ref result k) (* a-val b-val))))))

(defmethod m.* ((a complex-matrix) (b complex-matrix))
  (let* ((n (n b))
	 (m (m b))
	 (nxm (nxm b))
	 (result (make-complex-matrix-dim n m)))
    (declare (type fixnum n m nxm))

    (dotimes (k nxm result)
      (declare (type fixnum k))
      (let ((a-val (matrix-ref a k))
	    (b-val (matrix-ref b k)))
	(declare (type (complex complex-matrix-element-type) a-val b-val))
	(setf (matrix-ref result k) (* a-val b-val))))))

(defmethod m.* ((a real-matrix) (b complex-matrix))
  (let* ((n (n b))
	 (m (m b))
	 (nxm (nxm b))
	 (result (make-complex-matrix-dim n m)))
    (declare (type fixnum n m nxm))

    (dotimes (k nxm result)
      (declare (type fixnum k))
      (let ((a-val (matrix-ref a k))
	    (b-val (matrix-ref b k)))
	(declare (type (complex complex-matrix-element-type)  b-val)
		 (type real-matrix-element-type a-val))
	(setf (matrix-ref result k) (* a-val b-val))))))

(defmethod m.* ((a complex-matrix) (b real-matrix))
  (m.* b a))

;;;;;

  
(defmethod m.*! ((a real-matrix) (b real-matrix))
  (let* ((nxm (nxm b)))
    (declare (type fixnum nxm))

    (dotimes (k nxm b)
      (declare (type fixnum k))
      (let ((a-val (matrix-ref a k))
	    (b-val (matrix-ref b k)))
	(declare (type real-matrix-element-type a-val b-val))
	(setf (matrix-ref b k) (* a-val b-val))))))

(defmethod m.*! ((a complex-matrix) (b complex-matrix))
  (let* ((nxm (nxm b)))
    (declare (type fixnum nxm))

    (dotimes (k nxm b)
      (declare (type fixnum k))
      (let ((a-val (matrix-ref a k))
	    (b-val (matrix-ref b k)))
	(declare (type (complex complex-matrix-element-type) a-val b-val))
	(setf (matrix-ref b k) (* a-val b-val))))))

(defmethod m.*! ((a real-matrix) (b complex-matrix))
  (let* ((nxm (nxm b)))
    (declare (type fixnum nxm))

    (dotimes (k nxm b)
      (declare (type fixnum k))
      (let ((a-val (matrix-ref a k))
	    (b-val (matrix-ref b k)))
	(declare (type (complex complex-matrix-element-type)  b-val)
		 (type real-matrix-element-type a-val))
	(setf (matrix-ref b k) (* a-val b-val))))))

(defmethod m.*! ((a complex-matrix) (b real-matrix))
  (error "cannot M.*! a COMPLEX-MATRIX into a REAL-MATRIX,
don't know how to coerce COMPLEX to REAL"))

(defmethod m.* ((a number) (b number))
  (* a b))

(defmethod m.*! ((a number) (b number))
  (* a b))

(defmethod m.* ((a standard-matrix) (b number))
  (scal b a))

(defmethod m.* ((a number) (b standard-matrix))
  (scal a b))


(defmethod m.*! ((a standard-matrix) (b number))
  (scal! b a))

(defmethod m.*! ((a number) (b standard-matrix))
  (scal! a b))

;;;;;

(defmethod m* ((a number) (b number))
  (* a b))

(defmethod m* ((a standard-matrix) (b number))
  (scal b a))

(defmethod m* ((a number) (b standard-matrix))
  (scal a b))

;;;;;

(defmethod m*! ((a number) (b number))
  (* a b))

(defmethod m*! ((a standard-matrix) (b number))
  (scal! b a))

(defmethod m*! ((a number) (b standard-matrix))
  (scal! a b))

(defmethod m*! ((a complex-matrix) (b real-matrix))
  (error "cannot M#! a COMPLEX-MATRIX into a REAL-MATRIX,
don't know how to coerce COMPLEX to REAL"))

(defmethod m* ((a real-matrix) (b real-matrix))
  (gemm! 1.0d0 a b 0.0d0 (make-real-matrix-dim (n a) (m b))))

(defmethod m* ((a standard-matrix) (b standard-matrix))
  (gemm! 1.0d0 a b 0.0d0 (make-complex-matrix-dim (n a) (m b))))

(defmethod m*! :before ((a standard-matrix) (b standard-matrix))
  (let ((n-a (n a))
	(m-a (m a))
	(n-b (n b)))
    (if (not (= n-a m-a n-b))
	(error "cannot M*! a ~dx~d matrix into a ~dx~d matrix"
	       n-a
	       m-a
	       n-b
	       (m b)))))

;; TODO: on installation, try GEMM and see if this swap space
;; is necessary.

;; Q: what exactly is a special variable?
(defparameter *auto-set-m*!-swap* t)
(defparameter *m*!-swap-size* (* 512 512))
(defparameter *m*!-swap* 
  (make-array (* 2 *m*!-swap-size*) :element-type 'complex-matrix-element-type))
(defparameter *m*!-complex-wrapper* 
  (make-instance 'complex-matrix :n *m*!-swap-size* :m 1 :store *m*!-swap*))
(defparameter *m*!-real-wrapper* 
  (make-instance 'real-matrix :n *m*!-swap-size* :m 1 :store *m*!-swap*))

(declaim (inline set-m*!-swap-size))
(defun set-m*!-swap-size (nxm)
  "
  Syntax
  ======
  (SET-M*!-SWAP-SIZE nxm)

  Purpose
  =======
  Resets the swap space allocated for M*!.
  By default, 

              NxM = (* 512 512)

  meaning that M*! can safely multiply a PxN matrix and
  an NxM matrix Y given by the expression:

              (M*! X Y)

  There is no restriction on P.

  The actual storage allocated is 2 * NxM * size of DOUBLE-FLOAT,
  or equivalently NxM * size of (COMPLEX DOUBLE-FLOAT).

  If *auto-set-m*!-swap* (default) then the swap will be adjusted 
  if argument Y given to M*! exceeds swap size.

  Returns NxM
"
  (declare (special *m*!-swap-size*
		    *m*!-swap*
		    *m*!-real-wrapper*
		    *m*!-complex-wrapper*))
  (if (<= nxm 0)
      (error "argument NxM given to SET-M*!-SWAP-SIZE should be bigger than 0"))

  (let ((swap (make-array (* 2 nxm) :element-type 'complex-matrix-element-type)))

    (setf *m*!-swap-size* nxm)
    (setf *m*!-swap* swap)
    (setf (store *m*!-complex-wrapper*) swap) 
    (setf (n *m*!-complex-wrapper*) nxm) 
    (setf (m *m*!-complex-wrapper*) 1)     
    (setf (nxm *m*!-complex-wrapper*) nxm) 
    (setf (store *m*!-real-wrapper*) swap) 
    (setf (n *m*!-real-wrapper*) nxm)
    (setf (m *m*!-real-wrapper*) 1)
    (setf (nxm *m*!-real-wrapper*) nxm) 
    
    nxm))
  

(defmethod m*! ((a real-matrix) (b real-matrix))
  (let ((n (n b))
	(m (m b)))
    (declare (type fixnum n m)
	     (special *m*!-real-wrapper*))

    (if (and *auto-set-m*!-swap*
	     (> (* n m) *m*!-swap-size*))
	(set-m*!-swap-size (* n m)))

    (copy! b *m*!-real-wrapper*)
    (setf (n *m*!-real-wrapper*) n)
    (setf (m *m*!-real-wrapper*) m)
    (setf (nxm *m*!-real-wrapper*) (* n m))
    (gemm! 1.0d0 a b 0.0d0 *m*!-real-wrapper*)
    (copy! *m*!-real-wrapper* b)
    (setf (n *m*!-real-wrapper*) *m*!-swap-size*)
    (setf (m *m*!-real-wrapper*) 1)
    (setf (nxm *m*!-real-wrapper*) *m*!-swap-size*)
    b))

(defmethod m*! ((a complex-matrix) (b real-matrix))
  (error "cannot M*! a COMPLEX-MATRIX A into a REAL-MATRIX B,
don't know how to coerce COMPLEX to REAL"))

(defmethod m*! ((a standard-matrix) (b complex-matrix))
  (let ((n (n b))
	(m (m b)))
    (declare (type fixnum n m)
	     (special *m*!-complex-wrapper*))

    (if (and *auto-set-m*!-swap*
	     (> (* n m) *m*!-swap-size*))
	(set-m*!-swap-size (* n m)))
    
    (copy! b *m*!-complex-wrapper*)
    (setf (n *m*!-complex-wrapper*) n)
    (setf (m *m*!-complex-wrapper*) m)
    (setf (nxm *m*!-complex-wrapper*) (* n m))
    (gemm! 1.0d0 a b 0.0d0 *m*!-complex-wrapper*)
    (copy! *m*!-complex-wrapper* b)
    (setf (n *m*!-complex-wrapper*) *m*!-swap-size*)
    (setf (m *m*!-complex-wrapper*) 1)
    (setf (nxm *m*!-complex-wrapper*) *m*!-swap-size*)
    b))




