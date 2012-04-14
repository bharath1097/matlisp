(in-package :tensor)

(eval-when (load eval compile)
  (deftype real-type ()
    "The type of the elements stored in a REAL-MATRIX"
    'double-float)
  
  (deftype real-array (size)
    "The type of the storage structure for a REAL-MATRIX"
    `(simple-array real-type (,size)))
  )
;;
(declaim (inline allocate-real-store))
(defun allocate-real-store (size &optional (initial-element 0d0))
  (make-array size :element-type 'real-type
	      :initial-element (coerce initial-element 'real-type)))

(declaim (inline coerce-real))
(defun coerce-real (x)
  (coerce x 'real-type))

;;
(defclass real-tensor (standard-tensor)
  ((store
    :initform nil
    :type (real-array *)))
  (:documentation "Tensor class with real elements."))

(defclass sub-real-tensor (real-tensor)
  ((parent-tensor
    :initarg :parent-tensor
    :accessor parent-tensor))
  (:documentation "Sub-tensor class with real elements."))

;;
(defmethod initialize-instance ((tensor real-tensor) &rest initargs)
  (setf (store-size tensor) (length (get-arg :store initargs)))
  (call-next-method))
;;

(defmethod tensor-store-ref ((tensor real-tensor) (idx fixnum))
  (aref (store tensor) idx))

(defmethod (setf tensor-store-ref) ((value number) (tensor real-tensor) (idx fixnum))
  (setf (aref (store tensor) idx) (coerce-real value)))

;;

(defun make-real-tensor (&rest subs)
  (let* ((dims (apply #'make-index-store subs))
	 (ss (reduce #'* dims))
	 (store (allocate-real-store ss)))
    (make-instance 'real-tensor :store store :dimensions dims)))