;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: :matlisp; Base: 10 -*-
(in-package #:matlisp-conditions)

(defmacro defcondition (name (&rest parent-types) (&rest slot-specs) &body options)
  "Like define-condition except that you can define
   methods inside the condition definition with:
   (:method {generic-function-name} {args*} &rest code*)
"
  (labels ((get-methods (opts mth rst)
	     (if (null opts) (values (reverse mth) (reverse rst))
		 (if (and (consp (car opts)) (eq (caar opts) :method))
		     (get-methods (cdr opts) (cons (car opts) mth) rst)
		     (get-methods (cdr opts) mth (cons (car opts) rst))))))
    (multiple-value-bind (methods rest) (get-methods options nil nil)
      `(progn
	 (define-condition ,name ,parent-types
	   ,slot-specs
	   ,@rest)
	 ,@(loop for mth in methods
	      collect `(defmethod ,@(cdr mth)))))))
;;Generic conditions---------------------------------------------;;
(defcondition generic-error (error)
  ((message :reader message :initarg :message :initform ""))
  (:method print-object ((c generic-error) stream)
	   (format stream (message c))))

(defcondition invalid-type (generic-error)
  ((given-type :reader given :initarg :given)
   (expected-type :reader expected :initarg :expected))
  (:documentation "Given an unexpected type.")
  (:method print-object ((c invalid-type) stream)
	   (format stream "Given object of type ~A, expected ~A.~%" (given c) (expected c))
	   (call-next-method)))

(defcondition invalid-value (generic-error)
  ((given-value :reader given :initarg :given)
   (expected-value :reader expected :initarg :expected))
  (:documentation "Given an unexpected value.")
  (:method print-object ((c invalid-value) stream)
	   (format stream "Given object ~A, expected ~A.~%" (given c) (expected c))
	   (call-next-method)))

(defcondition unknown-token (generic-error)
  ((token :reader token :initarg :token))
  (:documentation "Given an unknown token.")
  (:method print-object ((c unknown-token) stream)
	   (format stream "Given unknown token: ~A.~%" (token c))
	   (call-next-method)))

(defcondition parser-error (generic-error)
  ()
  (:documentation "Macro reader encountered an error while parsing the stream."))

(defcondition coercion-error (generic-error)
  ((from :reader from :initarg :from)
   (to :reader to :initarg :to))
  (:documentation "Cannot coerce one type into another.")
  (:method print-object ((c coercion-error) stream)
	   (format stream "Cannot coerce ~a into ~a.~%" (from c) (to c))
	   (call-next-method)))

(defcondition out-of-bounds-error (generic-error)
  ((requested :reader requested :initarg :requested)
   (bound :reader bound :initarg :bound))
  (:documentation "General out-of-bounds error")
  (:method print-object ((c out-of-bounds-error) stream)
	   (format stream "Out-of-bounds error, requested index : ~a, bound : ~a.~%" (requested c) (bound c))
	   (call-next-method)))

(defcondition non-uniform-bounds-error (generic-error)
  ((assumed :reader assumed :initarg :assumed)
   (found :reader found :initarg :found))
  (:documentation "Bounds are not uniform")
  (:method print-object ((c non-uniform-bounds-error) stream)
	   (format stream "The bounds are not uniform, assumed bound : ~a, now found to be : ~a.~%" (assumed c) (found c))
	   (call-next-method)))

;;Permutation conditions-----------------------------------------;;
(define-condition permutation-error (error)
  ((permutation :reader permutation :initarg :permutation)))

(define-condition permutation-invalid-error (permutation-error)
  ()
  (:documentation "Object is not a permutation.")
  (:report (lambda (c stream)
	     (declare (ignore c))
	     (format stream "Object is not a permutation."))))

(define-condition permutation-permute-error (permutation-error)
  ((sequence-length :reader seq-len :initarg :seq-len)
   (group-rank :reader group-rank :initarg :group-rank))
  (:documentation "Cannot permute sequence.")
  (:report (lambda (c stream)
	     (format stream "Cannot permute sequence.
sequence-length : ~a
group-rank: ~a" (seq-len c) (group-rank c)))))

;;Tensor conditions----------------------------------------------;;
(define-condition tensor-error (error)
  ;;Optional argument for error-handling.
  ((tensor :reader tensor :initarg :tensor)))

(define-condition tensor-store-index-out-of-bounds (tensor-error)
  ((index :reader index :initarg :index)
   (store-size :reader store-size :initarg :store-size))
  (:documentation "An out of bounds index error for the one-dimensional store.")
  (:report (lambda (c stream)
	     (format stream "Requested index ~A, but store is only of size ~A." (index c) (store-size c)))))

(define-condition tensor-insufficient-store (tensor-error)
  ((store-size :reader store-size :initarg :store-size)
   (max-idx :reader max-idx :initarg :max-idx))
  (:documentation "Store is too small for the tensor with given dimensions.")
  (:report (lambda (c stream)
	     (format stream "Store size is ~A, but maximum possible index is ~A." (store-size c) (max-idx c)))))

(define-condition tensor-not-matrix (tensor-error)
  ((tensor-rank :reader rank :initarg :rank))
  (:documentation "Given tensor is not a matrix.")
  (:report (lambda (c stream)
	     (format stream "Given tensor with rank ~A, is not a matrix." (rank c)))))

(define-condition tensor-not-vector (tensor-error)
  ((tensor-rank :reader rank :initarg :rank))
  (:documentation "Given tensor is not a vector.")
  (:report (lambda (c stream)
	     (format stream "Given tensor with rank ~A, is not a vector." (rank c)))))

(define-condition tensor-index-out-of-bounds (tensor-error)
  ((argument :reader argument :initarg :argument)
   (index :reader index :initarg :index)
   (argument-space-dimension :reader dimension :initarg :dimension))
  (:documentation "An out of bounds index error")
  (:report (lambda (c stream)
	     (format stream "~&Out of bounds for argument ~A: requested ~A, but dimension is only ~A." (argument c) (index c) (dimension c)))))

(define-condition tensor-index-rank-mismatch (tensor-error)
  ((index-rank :reader index-rank :initarg :index-rank)
   (rank :reader rank :initarg :rank))
  (:documentation "Incorrect number of subscripts for the tensor.")
  (:report (lambda (c stream)
	     (format stream "Index is of size ~A, whereas the tensor is of rank ~A." (index-rank c) (rank c)))))

(define-condition tensor-invalid-head-value (tensor-error)
  ((head :reader head :initarg :head))
  (:documentation "Incorrect value for the head of the tensor storage.")
  (:report (lambda (c stream)
	     (format stream "Head of the store must be >= 0, initialized with ~A." (head c)))))

(define-condition tensor-invalid-dimension-value (tensor-error)
  ((argument :reader argument :initarg :argument)
   (argument-dimension :reader dimension :initarg :dimension))
  (:documentation "Incorrect value for one of the dimensions of the tensor.")
  (:report (lambda (c stream)
	     (format stream "Dimension of argument ~A must be > 0, initialized with ~A." (argument c) (dimension c)))))

(define-condition tensor-invalid-stride-value (tensor-error)  
  ((argument :reader argument :initarg :argument)
   (argument-stride :reader stride :initarg :stride))
  (:documentation "Incorrect value for one of the strides of the tensor storage.")
  (:report (lambda (c stream)
	     (format stream "Stride of argument ~A must be >= 0, initialized with ~A." (argument c) (stride c)))))

(define-condition tensor-cannot-find-counter-class (tensor-error)
  ((tensor-class :reader tensor-class :initarg :tensor-class))
  (:documentation "Cannot find the counter-class list of the given tensor class")
  (:report (lambda (c stream)
	     (format stream "Cannot find the counter-class list of the given tensor class: ~a." (tensor-class c)))))

(define-condition tensor-cannot-find-optimization (tensor-error)
  ((tensor-class :reader tensor-class :initarg :tensor-class))
  (:documentation "Cannot find optimization information for the given tensor class")
  (:report (lambda (c stream)
	     (format stream "Cannot find optimization information for the given tensor class: ~a." (tensor-class c)))))

(define-condition tensor-dimension-mismatch (tensor-error)
  ()
  (:documentation "The dimensions of the given tensors are not suitable for continuing with the operation.")
  (:report (lambda (c stream)
	     (declare (ignore c))
	     (format stream "The dimensions of the given tensors are not suitable for continuing with the operation."))))
