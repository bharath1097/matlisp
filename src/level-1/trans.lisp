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

(definline transpose! (A &optional permutation)
  "
  Syntax
  ======
  (TRANSPOSE! a [permutation])

  Purpose
  =======
  Exchange the arguments of the tensor in place. The default
  is to swap the first and last arguments of the tensor.

  Settable
  ========
  (setf (TRANSPOSE! tensor permutation) value)

  is basically the same as
  (copy! value (TRANSPOSE! tensor permutation)).

  NOTE: This will have side-effects even if copy! doesn't succeed."
  (declare (type standard-tensor a))
  (if permutation
      (progn
	(permute! (strides A) permutation)
	(permute! (dimensions A) permutation))
      (let-typed ((rnk (rank A) :type index-type)
		  (dim-A (dimensions A) :type index-store-vector)
		  (strd-A (strides A) :type index-store-vector))
		 (rotatef (aref dim-A (1- rnk)) (aref dim-A 0))
		 (rotatef (aref strd-A (1- rnk)) (aref strd-A 0))))
  A)

(definline (setf transpose!) (value A &optional permutation)
  (copy! value (transpose! A permutation)))

(definline transpose~ (A &optional permutation)
  "
  Syntax
  ======
  (TRANSPOSE~ a permutation)

  Purpose
  =======
  Like TRANSPOSE!, but the permuted strides and dimensions are part of
  a new tensor object instead, the store being shared with the given
  tensor.

  Settable
  ========
  (setf (TRANSPOSE~ tensor permutation) value)

  is basically the same as
  (copy! value (TRANSPOSE~ tensor permutation))"
  (declare (type standard-tensor A))
  (let ((displaced (make-instance (class-of A) :store (store A)
				  :store-size (store-size A)
				  :dimensions (copy-seq (dimensions A))
				  :strides (copy-seq (strides A))
				  :parent-tensor A)))
    (transpose! displaced permutation)))

(definline (setf transpose~) (value A &optional permutation)
  (declare (type standard-tensor A))
  (copy! value (transpose~ A permutation)))

(definline transpose (A &optional permutation)
  "
  Syntax
  ======
  (TRANSPOSE~ a permutation)

  Purpose
  =======
  Like TRANSPOSE!, but the permutation is applied on a copy of
  the given tensor.

  Settable
  ========
  (setf (TRANSPOSE tensor permutation) value)

  is the same as (setf (transpose~ ..) ..)"
  (declare (type standard-tensor A))
  (copy (transpose~ A permutation)))

(definline (setf transpose) (value A &optional permutation)
  (declare (type standard-tensor A))
  (copy! value (transpose~ A permutation)))

;;This is a bit more complicated, now that we are no longer in S_2
;;Computing the inverse permutation is trivial in the cyclic representation,
;;but probably not worth the trouble for this ugly macro.
#+nil
(defmacro with-transpose! (matlst &rest body)
  `(progn
     ,@(mapcar #'(lambda (mat) `(transpose! ,mat)) matlst)
     ,@body
     ,@(mapcar #'(lambda (mat) `(transpose! ,mat)) matlst)))


;;
(definline mconjugate! (A)
  "
  Syntax
  ======
  (mconjugate! A)

  Purpose
  =======
  Destructively modifies A into its complex conjugate (not hermitian conjugate).

  (tensor-imagpart~ A) <- (- (tensor-imagpart~ A)) "
  (etypecase A
    (real-tensor A)
    (complex-tensor
     (real-typed-num-scal! -1d0 (tensor-imagpart~ A))
     A)
    (number (conjugate A))))

(definline mconjugate (A)
  "
  Syntax
  ======
  (mconjugate A)

  Purpose
  =======
  Like mconjugate!, but non-destructive."
  (etypecase A
    (standard-tensor (mconjugate! (copy A)))
    (number (cl:conjugate A))))

;;
(defun htranspose! (A &optional permutation)
  "
   Syntax
   ======
   (HTRANSPOSE! A [permutation])

   Purpose
   =======
   Hermitian transpose of A (destructive).
"
  (declare (type standard-tensor A))
  (transpose! A permutation)
  (when (typep A 'complex-tensor)
    (mconjugate! A))
  A)

(definline ctranspose! (A &optional permutation)
  "
   Syntax
   ======
   (CTRANSPOSE! A [permutation])

   Purpose
   =======
   Conjugate transpose of A (destructive).
"
  (htranspose! A permutation))

(definline htranspose (A &optional permutation)
  "
  Syntax
  ======
  (HTRANSPOSE A [permutation])

  Purpose
  =======
  Like HTRANSPOSE!, but non-destructive."
  (declare (type standard-tensor A))
  (let ((result (copy A)))
    (htranspose! result permutation)))

(definline ctranspose (A &optional permutation)
  "
  Syntax
  ======
  (CTRANSPOSE A [permutation])

  Purpose
  =======
  Like CTRANSPOSE!, but non-destructive."
  (htranspose A permutation))
