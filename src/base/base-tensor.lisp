(in-package #:matlisp)

;;Alias for fixnum.
(deftype index-type ()
  'fixnum)

(deftype index-store-vector (&optional (size '*))
  `(simple-array index-type (,size)))

(make-array-allocator allocate-index-store 'index-type 0
"
  Syntax
  ======
  (ALLOCATE-INDEX-STORE SIZE [INITIAL-ELEMENT 0])

  Purpose
  ======
  Allocates index storage.")

(definline make-index-store (contents)
  "
  Syntax
  ======
  (MAKE-INDEX-STORE &rest CONTENTS)

  Purpose
  =======
  Allocates index storage with initial elements from the list CONTENTS."
  (make-array (length contents) :element-type 'index-type
	      :initial-contents contents))

(definline idxv (&rest contents)
  (make-index-store contents))
;;
(defclass base-tensor ()
  ((dimensions :reader dimensions :initarg :dimensions :type index-store-vector
   :documentation "Dimensions of the vector spaces in which the tensor's arguments reside.")   
   (parent-tensor :reader parent-tensor :initform nil :initarg :parent-tensor :type (or null base-tensor)
    :documentation "If the tensor is a view of another tensor, then this slot is bound.")
   (store :initarg :store :reader store
    :documentation "The actual storage for the tensor.")
   (attributes :initarg :attributes :initform nil
    :documentation "Place for computable attributes of an object instance."))
  (:documentation "Basic tensor class."))

(declaim (ftype (function (base-tensor) index-store-vector) dimensions)
	 (ftype (function (base-tensor) hash-table) attributes))

(defgeneric print-element (tensor
			   element stream)
  (:documentation "
  Syntax
  ======
  (PRINT-ELEMENT tensor element stream)

  Purpose
  =======
  This generic function is specialized to TENSOR to
  print ELEMENT to STREAM.  Called by PRINT-TENSOR/MATRIX
  to format a tensor into the STREAM.")
  (:method ((tensor base-tensor) element stream)
    (format stream "~a" element)))

;;Create hash-table only when necessary
(definline attributes (x)
  (declare (type base-tensor x))
  (or (slot-value x 'attributes)
      (setf (slot-value x 'attributes) (make-hash-table))))

(defmacro memoizing ((tensor name) &rest body)
  (declare (type symbol name))
  (with-gensyms (tens)
    `(let-typed ((,tens ,tensor :type base-tensor))
       (multiple-value-bind (value present?) (gethash ',name (attributes ,tens))
	 (values-list 
	  (if present?
	      value
	      (setf (gethash ',name (attributes ,tens))
		    (multiple-value-list (progn ,@body)))))))))

;;I have no idea what this does, or why we want it (inherited from standard-matrix.lisp)
(defmethod make-load-form ((tensor base-tensor) &optional env)
  "
  MAKE-LOAD-FORM allows us to determine a load time value for
  tensor, for example #.(make-tensors ...)"
  (make-load-form-saving-slots tensor :environment env))

;;These should ideally be memoised (or not)
;;We use order (against cl convention) so as not to cause confusion with matrix rank.
(definline order (tensor)
  (declare (type base-tensor tensor))
  (length (the index-store-vector (dimensions tensor))))
;; (definline tensor-rank (tensor) (order tensor))

;;
(definline size (tensor)
  (declare (type base-tensor tensor))
  (lvec-foldr #'* (the index-store-vector (dimensions tensor))))

(definline dims (tensor &optional idx)
  (declare (type base-tensor tensor))
  (if idx (aref (dimensions tensor) idx)
      (memoizing (tensor dims)
		 (lvec->list (the index-store-vector (dimensions tensor))))))
;;
(defmethod initialize-instance :after ((tensor base-tensor) &rest initargs)
  (declare (ignore initargs))
  (when *check-after-initializing?*
    (let-typed ((dims (dimensions tensor) :type index-store-vector))
      (very-quickly (loop :for i :from 0 :below (length dims)
		       :do (assert (> (aref dims i) 0) nil 'tensor-invalid-dimension-value :argument i :dimension (aref dims i) :tensor tensor))))))

;;
(defclass sparse-tensor (base-tensor) ())				       
(defclass dense-tensor (base-tensor) ())

(defgeneric ref (tensor &rest subscripts)
  (:documentation "
  Syntax
  ======
  (ref store subscripts)

  Purpose
  =======
  Return the element corresponding to subscripts.
"))

(defgeneric (setf ref) (value tensor &rest subscripts))

;;This is a silly hack. The plan is to get rid of this part using MOP.
(defvar *tensor-type-leaves* nil "
  This is used to keep track of classes that are not meant to be
  abstract classes. This prevents less specialized methods from
  clobbering the generation of more sophisticated (read faster)
  methods.")

(defmacro defleaf (name direct-superclasses direct-slots &rest options)
  `(progn
     (defclass ,name ,direct-superclasses ,direct-slots ,@options)
     (setf *tensor-type-leaves* (setadd *tensor-type-leaves* ',name))))
