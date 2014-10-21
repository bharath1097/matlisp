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

;;
(deft/generic (t/lapack-gesvd! #'subtypep) sym (A lda u ldu v ldv s))
(deft/method t/lapack-gesvd! (sym blas-numeric-tensor) (A lda u ldu v ldv s)
  (let* ((ftype (field-type sym)) (rtype (field-type (realified-type sym)))
	 (complex? (subtypep ftype 'cl:complex)))
    (using-gensyms (decl (A lda u ldu v ldv s) (lwork xxx xxr))
      `(let (,@decl)
	 (declare (type ,sym ,A)
		  (type ,(realified-type sym) ,s)
		  (type index-type ,lda))
	 (with-field-elements ,(realified-type sym) (,@(when complex? `((,xxr (t/fid+ ,rtype) (* 5 (lvec-min (dimensions ,A)))))))
	   (with-lapack-query ,sym (,xxx ,lwork)
	     (ffuncall ,(blas-func "gesvd" ftype)
	       (:& :character) (if ,u #\A #\N) (:& :character) (if ,v #\A #\N)
	       (:& :integer) (dimensions ,A 0) (:& :integer) (dimensions ,A 1)
	       (:* ,(lisp->ffc ftype) :+ (head ,A)) (the ,(store-type sym) (store ,A)) (:& :integer) ,lda
	       (:* ,(lisp->ffc rtype) :+ (head ,s)) (the ,(store-type (realified-type sym)) (store ,s))
	       (:* ,(lisp->ffc ftype) :+ (if ,u (head ,u) 0)) (if ,u (the ,(store-type sym) (store ,u)) (cffi:null-pointer)) (:& :integer) (if ,u ,ldu 1)
	       (:* ,(lisp->ffc ftype) :+ (if ,v (head ,v) 0)) (if ,v (the ,(store-type sym) (store ,v)) (cffi:null-pointer)) (:& :integer) (if ,v ,ldv 1)
	       (:* ,(lisp->ffc ftype)) (the ,(store-type sym) ,xxx) (:& :integer) ,lwork
	       ,@(when complex? `((:* ,(lisp->ffc rtype)) ,xxr))
	       (:& :integer :output) 0)))))))
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
       (let ((info (t/lapack-gesvd! ,(cl a) (with-colm (copy a)) (nrows a) u (and u (nrows u)) v (and v (nrows v)) s)))
	 (unless (= info 0)
	   (if (< info 0)
	       (error "GESVD: Illegal value in the ~:r argument." (- info))
	       (error "GESVD: DBDSQR did not converge. ~a superdiagonals of an intermediate bidiagonal form B did not converge to zero. See the description of WORK in the LAPACK documentation." info))))
       (let ((ret nil))
	 (when v (push (with-colm (transpose v)) ret))
	 (when u (push u ret))
	 (values-list (list* s ret))))))

;; (letv* ((a (randn '(10 10)))
;; 	(s u v (svd a :uv)))
;;   (norm #i(a - u * diag(s, 2) * v')))
