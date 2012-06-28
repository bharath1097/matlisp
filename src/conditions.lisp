;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: :matlisp; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id: conditions.lisp,v 1.1 2003/06/01 15:21:41 rtoy Exp $
;;;
;;; $Log: conditions.lisp,v $
;;; Revision 1.1  2003/06/01 15:21:41  rtoy
;;; Some conditions for matlisp matrix errors.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Error conditions for matlisp

(in-package :matlisp)

;;---------------------------------------------------------------;;
(define-condition generic-error (error)
  ((message :reader message :initarg :message :initform "")))

(defmethod print-object ((c generic-error) stream)
  (format stream (message c)))

;;---------------------------------------------------------------;;
(define-condition invalid-type (generic-error)
  ((given-type :reader given :initarg :given)
   (expected-type :reader expected :initarg :expected))
  (:documentation "Given an unexpected type."))

(defmethod print-object ((c invalid-type) stream)
  (format stream "Given object of type ~A, expected ~A.~%" (given c) (expected c))
  (call-next-method))

;;---------------------------------------------------------------;;
(define-condition invalid-value (generic-error)
  ((given-value :reader given :initarg :given)
   (expected-value :reader expected :initarg :expected))
  (:documentation "Given an unexpected value."))

(defmethod print-object ((c invalid-value) stream)
  (format stream "Given object ~A, expected ~A.~%" (given c) (expected c))
  (call-next-method))
;;---------------------------------------------------------------;;
(define-condition unknown-token (generic-error)
  ((token :reader token :initarg :token))
  (:documentation "Given an unknown token."))

(defmethod print-object ((c unknown-token) stream)
  (format stream "Given unknown token: ~A.~%" (token c))
  (call-next-method))

;;---------------------------------------------------------------;;
(define-condition matlisp-error (error)
  ;;Optional argument for error-handling.
  ((tensor :reader tensor :initarg :tensor)))

(define-condition store-index-out-of-bounds (matlisp-error)
  ((index :reader index :initarg :index)
   (store-size :reader store-size :initarg :store-size))
  (:documentation "An out of bounds index error for the one-dimensional store.")
  (:report (lambda (c stream)
	     (format stream "Requested index ~A, but store is only of size ~A." (index c) (store-size c)))))

(define-condition tensor-not-matrix (matlisp-error)
  ((tensor-rank :reader rank :initarg :rank))
  (:documentation "Given tensor is not a matrix.")
  (:report (lambda (c stream)
	     (format stream "Given tensor with rank ~A, is not a matrix." (rank c)))))

(define-condition insufficient-store (matlisp-error)
  ((store-size :reader store-size :initarg :store-size)
   (max-idx :reader max-idx :initarg :max-idx))
  (:documentation "Store is too small for the tensor with given dimensions.")
  (:report (lambda (c stream)
	     (format stream "Store size is ~A, but maximum possible index is ~A." (store-size c) (max-idx c)))))

(define-condition tensor-index-out-of-bounds (matlisp-error)
  ((argument :reader argument :initarg :argument)
   (index :reader index :initarg :index)
   (argument-space-dimension :reader dimension :initarg :dimension))
  (:documentation "An out of bounds index error")
  (:report (lambda (c stream)
	     (format stream "~&Out of bounds for argument ~A: requested ~A, but dimension is only ~A." (argument c) (index c) (dimension c)))))

(define-condition tensor-index-rank-mismatch (matlisp-error)
  ((index-rank :reader index-rank :initarg :index-rank)
   (rank :reader rank :initarg :rank))
  (:documentation "Incorrect number of subscripts for the tensor.")
  (:report (lambda (c stream)
	     (format stream "Index is of size ~A, whereas the tensor is of rank ~A." (index-rank c) (rank c)))))

(define-condition tensor-invalid-head-value (matlisp-error)
  ((head :reader head :initarg :head))
  (:documentation "Incorrect value for the head of the tensor storage.")
  (:report (lambda (c stream)
	     (format stream "Head of the store must be >= 0, initialized with ~A." (head c)))))

(define-condition tensor-invalid-dimension-value (matlisp-error)
  ((argument :reader argument :initarg :argument)
   (argument-dimension :reader dimension :initarg :dimension))
  (:documentation "Incorrect value for one of the dimensions of the tensor.")
  (:report (lambda (c stream)
	     (format stream "Dimension of argument ~A must be > 0, initialized with ~A." (argument c) (dimension c)))))

(define-condition tensor-invalid-stride-value (matlisp-error)  
  ((argument :reader argument :initarg :argument)
   (argument-stride :reader stride :initarg :stride))
  (:documentation "Incorrect value for one of the strides of the tensor storage.")
  (:report (lambda (c stream)
	     (format stream "Stride of argument ~A must be >= 0, initialized with ~A." (argument c) (stride c)))))

(define-condition tensor-cannot-find-sub-class (matlisp-error)
  ((tensor-class :reader tensor-class :initarg :tensor-class))
  (:documentation "Cannot find sub-class of the given tensor class")
  (:report (lambda (c stream)
	     (format stream "Cannot find sub-class of the given tensor class: ~a." (tensor-class c)))))

(define-condition tensor-cannot-find-optimization (matlisp-error)
  ((tensor-class :reader tensor-class :initarg :tensor-class))
  (:documentation "Cannot find optimization information for the given tensor class")
  (:report (lambda (c stream)
	     (format stream "Cannot find optimization information for the given tensor class: ~a." (tensor-class c)))))
