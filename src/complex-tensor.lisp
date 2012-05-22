(in-package :tensor)

(eval-when (load eval compile)
  (deftype complex-base-type ()
    "The type of the elements stored in a COMPLEX-MATRIX"
    'double-float)
  
  (deftype complex-base-array (size)
    "The type of the storage structure for a COMPLEX-MATRIX"
    `(simple-array real-type (,size)))
 
  (deftype complex-type ()
    "Complex number with Re, Im parts in complex-base-type."
    '(cl:complex (complex-base-type * *)))
  )
;;
(declaim (inline allocate-complex-store))
(defun allocate-complex-store (size)
  (make-array (* 2 size) :element-type 'complex-base-type
	      :initial-element (coerce 0 'complex-base-type)))

(declaim (inline coerce-complex))
(defun coerce-complex (x)
  (coerce x 'complex-type))

;;
(defclass complex-tensor (standard-tensor)
  ((store
    :initform nil
    :type (complex-base-array *)))
  (:documentation "Tensor class with complex elements."))

(defclass complex-sub-tensor (complex-tensor sub-tensor)
  ()
  (:documentation "Sub-tensor class with complex elements."))

;;
(defmethod initialize-instance ((tensor complex-tensor) &rest initargs)
  (mlet* ((2*ss (length (get-arg :store initargs)) :type index-type))
	 (unless (evenp 2*ss)
	   (error "Store is not of even length.
Cannot hold complex numbers."))
	 ;;Hold the effective Store-size.
	 ;;Might have to have conversions when converting complex-tensors to real ones.
	 (setf (store-size tensor) (/ 2*ss 2)))
  (call-next-method))
;;

(defmethod tensor-store-ref ((tensor complex-tensor) (idx fixnum))
  (complex (aref (store tensor) (* 2 idx))
	   (aref (store tensor) (+ (* 2 idx) 1))))

(defmethod (setf tensor-store-ref) ((value number) (tensor complex-tensor) (idx fixnum))
  (setf (aref (store tensor) (* 2 idx)) (coerce (realpart value) 'complex-base-type)
	(aref (store tensor) (+ (* 2 idx) 1)) (coerce (imagpart value) 'complex-base-type)))

;;

(defun make-complex-tensor (&rest subs)
  (let* ((dims (make-index-store subs))
	 (ss (reduce #'* dims))
	 (store (allocate-complex-store ss)))
    (make-instance 'complex-tensor :store store :dimensions dims)))

