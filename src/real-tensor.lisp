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

(defclass real-sub-tensor (real-tensor sub-tensor)
  ()
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
  (let* ((dims (make-index-store subs))
	 (ss (reduce #'* dims))
	 (store (allocate-real-store ss)))
    (make-instance 'real-tensor :store store :dimensions dims)))

;;

(defun collapse~ (tensor &rest subs)
  (declare (type standard-tensor tensor))
  (mlet* (((hd rank dims stds sto) (slot-values tensor '(head rank dimensions strides store))
	   :type (index-type index-type (index-array *) (index-array *) (real-array *))))
	 (labels ((parse-sub (rsubs i ndims nstds hd)
		    (let ((val (car rsubs)))
		      (cond
			((< i 0)
			 (unless (null val)
			   (error "Too many subscripts for a tensor of rank ~A" rank))
			 (values ndims nstds hd))
			;;
			((null val)
			 (error "Too few subscripts for a tensor of rank ~A" rank))
			;;
			((eq val t)
			 (parse-sub (cdr rsubs) (1- i) (cons (aref dims i) ndims) (cons (aref stds i) nstds) hd))
			;;
			(t
			 (unless (< -1 val (aref dims i))
			   (error "Requested index ~A for argument ~A is out of bounds.
Tensor only has dimension ~A for the ~A argument." val i (aref dims i) i))
			 (parse-sub (cdr rsubs) (1- i) ndims nstds (+ hd (* val (aref stds i)))))))))
	   (multiple-value-bind (ndims nstds nhd) (parse-sub (reverse subs) (- rank 1) nil nil hd)
	     (if (null ndims)
		 (apply #'tensor-ref (cons tensor subs))
		 (make-instance (typecase tensor
				  (real-tensor 'real-sub-tensor)
				  (complex-tensor 'complex-sub-tensor))
				:store sto :dimensions (make-index-store ndims)
				:strides (make-index-store nstds) :head nhd
				:parent-tensor tensor))))))