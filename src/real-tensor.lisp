(in-package :matlisp)

(deftype real-type ()
  "The type of the elements stored in a REAL-MATRIX"
  'double-float)

(deftype real-array (size)
  "The type of the storage structure for a REAL-MATRIX"
  `(simple-array real-type (,size)))
;;

(make-array-allocator allocate-real-store 'real-type 0d0
"(allocate-real-store size [initial-element])
Allocates real storage.  Default initial-element = 0d0.")

(definline coerce-real (x)
  (coerce x 'real-type))

;;
(defclass real-tensor (standard-tensor)
  ((store
    :initform nil
    :type (real-array *)))
  (:documentation "Tensor class with real elements."))

(defclass real-sub-tensor (real-tensor standard-sub-tensor)
  ()
  (:documentation "Sub-tensor class with real elements."))

;;Push the counter sub-class name into a hash-table so that we can
;;refer to it later from class-ignorant functions.
(setf (gethash 'real-tensor *sub-tensor-counterclass*) 'real-sub-tensor
      (gethash 'real-sub-tensor *sub-tensor-counterclass*) 'real-sub-tensor)

;;
(defmethod initialize-instance ((tensor real-tensor) &rest initargs)
  (setf (store-size tensor) (length (getf initargs :store)))
  (call-next-method))
;;

(tensor-store-defs (real-tensor real-type real-type)
  :store-allocator allocate-real-store
  :coercer coerce-real
  :reader
  (lambda (tstore idx)
    (aref tstore idx))
  :value-writer
  (lambda (value store idx)
    (setf (aref store idx) value))
  :reader-writer
  (lambda (fstore fidx tstore tidx)
    (setf (aref tstore tidx) (aref fstore fidx))))

(setf (gethash 'real-sub-tensor *tensor-class-optimizations*) 'real-tensor)

(defmethod (setf tensor-ref) ((value number) (tensor real-tensor) subscripts)
  (let ((sto-idx (store-indexing subscripts tensor)))
    (setf (tensor-store-ref tensor sto-idx) (coerce-real value))))

;;
(defmethod print-element ((tensor real-tensor)
			  element stream)
  (format stream "~11,5,,,,,'Eg" element))


