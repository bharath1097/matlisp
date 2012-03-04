;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: :matlisp; Base: 10 -*-
;;;

(defclass foreign-real-matrix (real-matrix)
  ((store
    :type foreign-pointer))
  (:documentation "A class of matrices with real elements."))

(defclass foreign-complex-matrix (complex-matrix)
  ((store
    :type foreign-pointer))
  (:documentation "A class of matrices with complex elements."))


(defun make-foreign-real-matrix (n m store)
  "
  Syntax
  ======
  (MAKE-REAL-MATRIX-DIM n m [fill-element])

  Purpose
  =======
  Creates an NxM REAL-MATRIX with initial contents FILL-ELEMENT,
  the default 0.0d0

  See MAKE-REAL-MATRIX.
"
  (declare (type fixnum n m))
  
  (make-instance 'foreign-real-matrix :nrows n :ncols m
		 :store store))