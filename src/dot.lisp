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
;;; Originally written by Tunc Simsek, Univ. of California, Berkeley,
;;; 2000, simsek@eecs.berkeley.edu
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id: dot.lisp,v 1.6 2004/05/24 16:34:22 rtoy Exp $
;;;
;;; $Log: dot.lisp,v $
;;; Revision 1.6  2004/05/24 16:34:22  rtoy
;;; More SBCL support from Robert Sedgewick.  The previous SBCL support
;;; was incomplete.
;;;
;;; Revision 1.5  2002/07/28 14:59:31  rtoy
;;; Clean up method dot for complex matrices.
;;;
;;; Revision 1.4  2000/07/11 18:02:03  simsek
;;; o Added credits
;;;
;;; Revision 1.3  2000/07/11 02:11:56  simsek
;;; o Added support for Allegro CL
;;;
;;; Revision 1.2  2000/05/08 17:19:18  rtoy
;;; Changes to the STANDARD-MATRIX class:
;;; o The slots N, M, and NXM have changed names.
;;; o The accessors of these slots have changed:
;;;      NROWS, NCOLS, NUMBER-OF-ELEMENTS
;;;   The old names aren't available anymore.
;;; o The initargs of these slots have changed:
;;;      :nrows, :ncols, :nels
;;;
;;; Revision 1.1  2000/04/14 00:12:48  simsek
;;; Initial revision.
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :matlisp)

(defgeneric dot (x y &optional conjugate-p)
  (:documentation
"
  Sytnax
  ======
  (DOT x y [conjugate-p])

  Purpose
  =======
  Computes the inner product of X,Y.

  CONJUGATE-P       Computed Result
  ---------------------------------
                         H
  T (default)           X * Y
                         T
  NIL                   X * Y


  If X is real then CONJUGATE-P has no 
  effect since for real vectors:

                H   T
               X = X

  X,Y must be vectors but it doesn't matter
  if they are row or column vectors.

  If X and Y are both scalars then this is the same
  as (* (CONJUGATE X) Y) if CONJUAGTE-P and (* X Y)
  otherwise.
"))

(defmethod dot ((x number) (y number) &optional (conjugate-p t))
  (if conjugate-p
      (* (conjugate x) y)
      (* x y)))

(defmethod dot :before ((x standard-tensor) (y standard-tensor) &optional (conjugate-p t))
  (declare (ignore conjugate-p))
  (unless (and (vector-p x) (vector-p y))
    (error 'tensor-not-vector
	   :rank (cond
		   ((not (vector-p x))
		    (rank x))
		   ((not (vector-p y))
		    (rank y)))))
  (unless (idx= (dimensions x) (dimensions y))
    (error 'tensor-dimension-mismatch)))

(defmethod dot ((x real-tensor) (y real-tensor) &optional (conjugate-p t))
  (declare (ignore conjugate-p))
  (ddot (number-of-elements x)
	(store x) (aref (strides x) 0)
	(store y) (aref (strides y) 0)
	(head x) (head y)))

(defmethod dot ((x real-tensor) (y complex-tensor) &optional (conjugate-p t))
  (declare (ignore conjugate-p))
  (let ((nele (number-of-elements x))
	(std-x (aref (strides x) 0))
	(hd-x (head x))
	(std-y (aref (strides y) 0))
	(hd-y (head y)))
    (declare (type index-type nele std-x std-y hd-x hd-y))
    (let ((rpart (ddot nele (store x) std-x (store y) (* 2 std-y) hd-x (* 2 hd-y)))
	  (ipart (ddot nele (store x) std-x (store y) (* 2 std-y) hd-x (1+ (* 2 hd-y)))))
      (declare (type complex-base-type rpart ipart))
      (if (zerop ipart)
	  rpart
	  (complex rpart ipart)))))

(defmethod dot ((x complex-tensor) (y real-tensor) &optional (conjugate-p t))
  (let ((cres (dot y x)))
    (if conjugate-p
	(conjugate cres)
	cres)))

(defmethod dot ((x complex-tensor) (y complex-tensor) &optional (conjugate-p t))
  (let ((nele (number-of-elements x))
	(std-x (aref (strides x) 0))
	(hd-x (head x))
	(std-y (aref (strides y) 0))
	(hd-y (head y)))
    (declare (type index-type nele std-x hd-x std-y hd-y))
    (let ((ret (if conjugate-p
		   (zdotc nele
			  (store x) std-x
			  (store y) std-y
			  hd-x hd-y)
		   (zdotu nele
			  (store x) std-x
			  (store y) std-y
			  hd-x hd-y))))
      (if (zerop (imagpart ret))
	  (realpart ret)
	  ret))))
