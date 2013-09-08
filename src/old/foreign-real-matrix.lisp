;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: :matlisp; Base: 10 -*-
;;;

(defclass foreign-real-matrix (real-matrix)
  ((store
    :type cffi:foreign-pointer))
  (:documentation "A class of matrices with real elements."))

;;
(defmethod matrix-ref-1d ((matrix foreign-real-matrix) (idx fixnum))
  (let ((store (store matrix)))
    (declare (type cffi:foreign-pointer store))
    (cffi:mem-aref store :double idx)))

(defmethod (setf matrix-ref-1d) ((value cl:real) (matrix foreign-real-matrix) (idx fixnum))
  (let ((store (store matrix)))
    (declare (type cffi:foreign-pointer store))
    (setf (cffi:mem-aref store :double idx) (coerce value 'double-float))))


(defun make-foreign-real-matrix (n m store store-size)
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
