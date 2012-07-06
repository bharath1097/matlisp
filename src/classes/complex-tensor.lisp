(in-package #:matlisp)

(deftype complex-base-type ()
  "The type of the elements stored in a COMPLEX-MATRIX"
  'double-float)

(deftype complex-base-array (size)
  "The type of the storage structure for a COMPLEX-MATRIX"
  `(simple-array complex-base-type (,size)))

(deftype complex-type ()
  "Complex number with Re, Im parts in complex-base-type."
  '(cl:complex complex-base-type))
;;

(definline allocate-complex-store (size)
  "
  (allocate-complex-store size)
  Allocates real storage of size (* SIZE 2).
  Default initial-element = 0d0.
"
  (make-array (* 2 size) :element-type 'complex-base-type
	      :initial-element (coerce 0 'complex-base-type)))

(definline coerce-complex (x)
  (coerce x 'complex-type))

(definline coerce-complex-base (x)
  (coerce x 'complex-base-type))

;;
(defclass complex-tensor (standard-tensor)
  ((store
    :initform nil
    :type (complex-base-array *)))
  (:documentation "Tensor class with complex elements."))

(defclass complex-sub-tensor (complex-tensor standard-sub-tensor)
  ()
  (:documentation "Sub-tensor class with complex elements."))

;;Push the counter sub-class name into a hash-table so that we can
;;refer to it later from class-ignorant functions.
(setf (gethash 'complex-tensor *sub-tensor-counterclass*) 'complex-sub-tensor
      (gethash 'complex-sub-tensor *sub-tensor-counterclass*) 'complex-sub-tensor)

;;
(defmethod initialize-instance ((tensor complex-tensor) &rest initargs)
  (mlet* ((2*ss (length (getf initargs :store)) :type index-type))
	 (unless (evenp 2*ss)
	   (error "Store is not of even length.
Cannot hold complex numbers."))
	 ;;Hold the effective Store-size.
	 ;;Might have to have conversions when converting complex-tensors to real ones.
	 (setf (store-size tensor) (/ 2*ss 2)))
  (call-next-method))
;;

(tensor-store-defs (complex-tensor complex-type complex-base-type)
  :store-allocator allocate-complex-store
  :coercer coerce-complex  
  :reader
  (lambda (tstore idx)
    (complex (aref tstore (* 2 idx))
	     (aref tstore (1+ (* 2 idx)))))
  :value-writer
  (lambda (value store idx)
    (setf (aref store (* 2 idx)) (realpart value)
	  (aref store (1+ (* 2 idx))) (imagpart value)))
  :reader-writer
  (lambda (fstore fidx tstore tidx)
    (setf (aref tstore (* 2 tidx)) (aref fstore (* 2 fidx))
	  (aref tstore (1+ (* 2 tidx))) (aref fstore (1+ (* 2 fidx)))))
  :swapper
  (lambda (fstore fidx tstore tidx)
    (progn
      (rotatef (aref tstore (* 2 tidx)) (aref fstore (* 2 fidx)))
      (rotatef (aref tstore (1+ (* 2 tidx))) (aref fstore (1+ (* 2 fidx)))))))

(setf (gethash 'complex-sub-tensor *tensor-class-optimizations*) 'complex-tensor)

(defmethod (setf tensor-ref) ((value number) (tensor complex-tensor) subscripts)
  (let ((sto-idx (store-indexing subscripts tensor)))
    (setf (tensor-store-ref tensor sto-idx) (coerce-complex value))))

;;
(defmethod print-element ((tensor complex-tensor)
			  element stream)
  (let ((realpart (realpart element))
	(imagpart (imagpart element)))
    (format stream (if (zerop imagpart)
		       "~11,5,,,,,'Eg"
		       "#C(~11,4,,,,,'Ee ~11,4,,,,,'Ee)")
	    realpart imagpart)))
