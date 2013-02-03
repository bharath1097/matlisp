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
;;; Written by Knut Gjerden (analogous to getrf.lisp by R. Toy)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id: potrf.lisp,v 1.1 2009/08/19 16:01:36 rtoy Exp $
;;;
;;; $Log: potrf.lisp,v $
;;; Revision 1.1  2009/08/19 16:01:36  rtoy
;;; Add support for interfacing to potrf and potrs.  Submitted by Knut
;;; Gjerden.
;;;
;;; src/potrf.lisp:
;;; o New file for matlisp interface to potrf.  Modeled after getrf.
;;;
;;; src/potrs.lisp:
;;; o New file for matlisp interface to potrs.  Modeled after getrs.
;;;
;;; src/lapack.lisp:
;;; o Add Fortran interface to dpotrf, zpotrf, dpotrs, and zpotrs.
;;;
;;; matlisp.mk.in:
;;; o Add dpotrf.o, dpotf2.o dpotrs.o zpotrs.o to list of LAPACK files we
;;;   need to compile.
;;;
;;; packages.lisp:
;;; o Export DPOTRS, ZPOTRS, DPOTRF, and ZPOTRF
;;; o Export POTRF! and POTRS!.
;;;
;;; start.lisp:
;;; o Don't use verbose output from mk:oos.
;;;
;;; system.dcl:
;;; o Add potrf and potrs to system.
;;;
;;; Revision 1.1   06.08.2009 12:40:40 knutgj
;;; o Initial revision.
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MATLISP")

#+nil (use-package "BLAS")
#+nil (use-package "LAPACK")
#+nil (use-package "FORTRAN-FFI-ACCESSORS")

#+nil (export '(potrf!))
	  

(defgeneric potrf! (a &key uplo)
  (:documentation
"
  Syntax
  ======
  (POTRF a [:u :L])

  Purpose
  =======
  DPOTRF computes the Cholesky factorization of a real symmetric
  positive definite matrix A.

  The factorization has the form
     A = U**T * U,  if UPLO = 'U', or
     A = L  * L**T,  if UPLO = 'L',
  where U is an upper triangular matrix and L is lower triangular.

  This is the block version of the algorithm, calling Level 3 BLAS.

  Return Values
  =============
  [1] The factor U or L from the Cholesky
          factorization A = U**T*U or A = L*L**T.
  [2] INFO = T: successful
             i:  U(i,i) is exactly zero. 
"))





(defmethod potrf! ((a real-matrix) &key uplo)
  (let* ((n (nrows a))
	 (m (ncols a)))

    (declare (type fixnum n m))
    (multiple-value-bind (new-a info)
	(dpotrf (case uplo
                  (:L "L")
                  (:U "U")
                  (t "U")) ;; UPLO
		m          ;; N
		(store a)  ;; A
		n          ;; LDA
		0)         ;; INFO
      (declare (ignore new-a))
      (values a (if (zerop info)
			 t
		       info)))))

(defmethod potrf! ((a complex-matrix) &key uplo)
  (let* ((n (nrows a))
	 (m (ncols a)))

    (declare (type fixnum n m))
    (multiple-value-bind (new-a info)
	(zpotrf  (case uplo
                  (:L "L")
                  (:U "U")
                  (t "U")) ;; UPLO
		m          ;; N
		(store a)  ;; A
		n          ;; LDA
		0)         ;; INFO
      (declare (ignore new-a))
      (values a (if (zerop info)
			 t
		       info)))))
