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

(deft/generic (t/lapack-gesvd-func #'subfieldp) sym ())
(deft/method t/lapack-gesvd-func (sym real-tensor) ()
  'matlisp-lapack:dgesvd)

(deft/method t/lapack-gesvd-func (sym complex-tensor) ()
  'mzgesvd)
(definline mzgesvd (jobu jobvt m n a lda s u ldu vt ldvt work lwork info &optional (head-a 0) (head-u 0) (head-vt 0))
  (matlisp-lapack:zgesvd jobu jobvt m n a lda s u ldu vt ldvt work lwork (t/store-allocator (t/realified-type complex-tensor) (* 5 (min m n))) info head-a head-u head-vt))
;;
(deft/generic (t/lapack-gesvd! #'subtypep) sym (A lda u ldu v ldv s))
(deft/method t/lapack-gesvd! (sym blas-numeric-tensor) (A lda u ldu v ldv s)
  (using-gensyms (decl (A lda u ldu v ldv s) (lwork xxx))
    `(let (,@decl
	   (,lwork -1))
       (declare (type ,sym ,A)
		(type ,(realified-type sym) ,s)
		(type index-type ,lda ,lwork))
       (let-typed ((,xxx (t/store-allocator ,sym 1) :type ,(store-type sym)))
	 (,(macroexpand-1 `(t/lapack-gesvd-func ,sym))
	   (if ,u #\A #\N) (if ,v #\A #\N)
	   (nrows ,A) (ncols ,A)
	   ,xxx ,lda
	   ,xxx	   
	   ,xxx (if ,u ,ldu 1)
	   ,xxx (if ,v ,ldv 1)
	   ,xxx -1
	   0)
	 (setq ,lwork (ceiling (t/frealpart ,(field-type sym) (t/store-ref ,sym ,xxx 0)))))
       (,(macroexpand-1 `(t/lapack-gesvd-func ,sym))
	 (if ,u #\A #\N) (if ,v #\A #\N)
	 (nrows ,A) (ncols ,A)
	 (the ,(store-type sym) (store ,A)) ,lda
	 (the ,(store-type (realified-type sym)) (store ,s))
	 (if ,u (the ,(store-type sym) (store ,u)) (cffi:null-pointer)) (if ,u ,ldu 1)
	 (if ,v (the ,(store-type sym) (store ,v)) (cffi:null-pointer)) (if ,v ,ldv 1)
	 (t/store-allocator ,sym ,lwork) ,lwork
	 0
	 (the index-type (head ,A)) (if ,u (the index-type (head ,u)) 0) (if ,v (the index-type (head ,v)) 0)))))
;;
(defgeneric svd (a &optional job)
  (:documentation
  "
  Syntax
  ======
  (SVD a [job])

  Purpose
  =======
  Computes the singular value decomposition (SVD) of the 
  NxM matrix A. The SVD of A is given by:

                 A = U * SIGMA * V'
  
  where, taking p = min(n,m):

          U = [u1 u2 ... un] an NxN othogonal matrix
          
               [s1  0  0  ... 0]
      SIGMA =  [0  s2  0  ... 0]  if N < M
               [:   :  \\      :]
               [0   0  sp ... 0]
               
               [s1  0  0 ...  0]         
            =  [0  s2  0 ...  0]  if M > N
               [:   :  \\ ...  0]
               [:   :    \\    0]
               [0   0  0 ... sp]
               [0   0  0 ...  0]
               [:   :  :      :]
               [0   0  0 ...  0]

              [v1']
          V = [v2'] an MxM orthogonal matrix
              [ : ]
              [vm']

   The diagonal elements of SIGMA are the singular values of A.
   s1,...,sp are real, non-negative and arranged so that s1 >= s2 >= ... >= sp
   The first p columns of U are the left singular vectors of A.
   The first p rows of V' are the right singular vectors of A.

  Return Values
  =============

  JOB              Return Value
  -------------------------------------------------
  :NN (default)   SIGMA                The p diagonal elements of SIGMA as a vector.
  :UN             SIGMA, U
  :NV             SIGMA, V
  :UV             SIGMA, U, V
  ")
  (:method :before ((a base-tensor) &optional (job :nn))
	   (assert (member job '(:nn :un :nv :uv)) nil 'invalid-arguments)))

(define-tensor-method svd ((a blas-numeric-tensor :input) &optional (job :nn))
  `(destructuring-bind (ujob vjob) (split-job job)
     (let ((u (when (char= ujob #\U) (with-colm (zeros (list (nrows a) (nrows a)) ',(cl a)))))
	   (v (when (char= vjob #\V) (with-colm (zeros (list (ncols a) (ncols a)) ',(cl a)))))
	   (s (zeros (lvec-min (dimensions a)) ',(realified-type (cl a)))))
       (multiple-value-bind (ao so uo vo wo info) (t/lapack-gesvd! ,(cl a) (with-colm (copy a)) (nrows a) u (and u (nrows u)) v (and v (nrows v)) s)
	 (declare (ignore ao so uo vo wo))
	 (unless (= info 0)
	   (if (< info 0)
	       (error "GESVD: Illegal value in the ~:r argument." (- info))
	       (error "GESVD: DBDSQR did not converge. ~a superdiagonals of an intermediate bidiagonal form B did not converge to zero. See the description of WORK in the LAPACK documentation." info))))
       (let ((ret nil))
	 (when v (push (with-colm (transpose v)) ret))
	 (when u (push u ret))
	 (values-list (list* s ret))))))
