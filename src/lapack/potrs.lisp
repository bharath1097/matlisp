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
;;; Written by Knut Gjerden (analogous to getrs.lisp by R. Toy)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id: potrs.lisp,v 1.1 2009/08/19 16:01:36 rtoy Exp $
;;;
;;; $Log: potrs.lisp,v $
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
;;; Revision 1.1   06.08.2009 12:19:27 knutgj
;;; o Initial revision.
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MATLISP")

#+nil (use-package "BLAS")
#+nil (use-package "LAPACK")
#+nil (use-package "FORTRAN-FFI-ACCESSORS")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function definitions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric potrs! (a b &key uplo)
  (:documentation
   "
  Syntax
  ======
  (POTRS! a b [:U :L])

  Purpose
  =======
  Solves a system of linear equations
      A * X = B  or  A' * X = B
  with a general N-by-N matrix A using the Cholesky LU factorization computed
  by POTRF.  A and are the results from POTRF, UPLO specifies
  the form of the system of equations: 
           = 'U':   A = U**T*U 
           = 'L':   A = L*L**T

  Return Values
  =============
  [1] The NxM matrix X. (overwriting B)
  [4] INFO = T: successful
             i:  U(i,i) is exactly zero.  The LU factorization
                 used in the computation has been completed, 
                 but the factor U is exactly singular.
                 Solution could not be computed.
"))

(defgeneric potrs (a b &key uplo)
  (:documentation
 "
  Syntax
  ======
  (POTRS a b [:U :L])

  Purpose
  =======
  Same as POTRS! except that B is not overwritten.
"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Method definitions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod potrs! :before ((a standard-matrix) (b standard-matrix) &key uplo)
  (declare (ignore uplo))
  (let ((n-a (nrows a))
        (m-a (ncols a))
        (n-b (nrows b)))
    (if (not (= n-a m-a n-b))
        (error "Dimensions of A,B given to POTRS! do not match"))))

(defmethod potrs! ((a real-matrix) (b real-matrix) &key uplo)
  (let* ((n (nrows a))
         (m (ncols b)))

    (declare (type fixnum n m))

    (multiple-value-bind (x info)
        (dpotrs (case uplo
                  (:L "L")
                  (:U "U")
                  (t "U"))  ;;UPLO
                n           ;;N
                m           ;;NRHS
                (store a)   ;;A
                n           ;;LDA
                (store b)   ;;B
                n           ;;LDB
                0)          ;;INFO
        
        (values 
         (make-instance 'real-matrix :nrows n :ncols m :store x)
         (if (zerop info)
             t
           info)))))

(defmethod potrs! ((a complex-matrix) (b complex-matrix) &key uplo)

  (let* ((n (nrows a))
         (m (ncols b)))

    (declare (type fixnum n m))

    (multiple-value-bind (x info)
        (zpotrs (case uplo
                  (:L "L")
                  (:U "U")
                  (t "U"))
                n
                m
                (store a)
                n
                (store b)
                n
                0)
        
        (values 
         (make-instance 'complex-matrix :nrows n :ncols m :store x) 
         (if (zerop info)
             t
           info)))))

(defmethod potrs! ((a standard-matrix) (b standard-matrix) &key uplo)
  (let ((a (typecase a
             (real-matrix (copy! a (make-complex-matrix-dim (nrows a) (ncols a))))
             (complex-matrix a)
             (t (error "argument A given to POTRS! is not a REAL-MATRIX or COMPLEX-MATRIX"))))
        (b (typecase b
             (real-matrix (copy! b (make-complex-matrix-dim (nrows b) (ncols b))))
             (complex-matrix b)
             (t (error "argument B given to POTRS! is not a REAL-MATRIX or COMPLEX-MATRIX")))))
    (potrs! a b :uplo uplo)))

(defmethod potrs :before ((a standard-matrix) (b standard-matrix) &key uplo)
  (declare (ignore uplo))
  (if (not (= (nrows a) (ncols a) (nrows b)))
      (error "dimensions of A,B given to POTRS do not match")))

(defmethod potrs ((a real-matrix) (b real-matrix) &key uplo)
  (potrs! a (copy b) :uplo uplo))

(defmethod potrs ((a complex-matrix) (b complex-matrix) &key uplo)
  (potrs! a (copy b) :uplo uplo))

(defmethod potrs ((a standard-matrix) (b standard-matrix) &key uplo)
  (let ((a (typecase a
             (real-matrix (copy! a (make-complex-matrix-dim (nrows a) (ncols a))))
             (complex-matrix (copy a))
             (t (error "argument A given to POTRS is not a REAL-MATRIX or COMPLEX-MATRIX"))))
        (b (typecase b
             (real-matrix (copy! b (make-complex-matrix-dim (nrows b) (ncols b))))
             (complex-matrix (copy b))
             (t (error "argument B given to POTRS is not a REAL-MATRIX or COMPLEX-MATRIX")))))
    (potrs! a b :uplo uplo)))

