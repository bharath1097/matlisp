(in-package #:matlisp)

;;Complex-base-type must always equal real-type.
(deftype complex-base-type ()
  "The type of the elements stored in a COMPLEX-MATRIX"
  'double-float)

(deftype complex-store-vector (&optional (size '*))
  "The type of the storage structure for a COMPLEX-MATRIX"
  `(simple-array complex-base-type (,size)))

(deftype complex-type ()
  "Complex number with Re, Im parts in complex-base-type."
  '(cl:complex complex-base-type))

;;Field operations
(definline complex-type.f+ (a b)
  (declare (type complex-type a b))
  (+ a b))

(definline complex-type.f- (a b)
  (declare (type complex-type a b))
  (- a b))

(definline complex-type.finv+ (a)
  (declare (type complex-type a))
  (- a))

(definline complex-type.fid+ ()
  #c(0.0d0 0.0d0))

(definline complex-type.f* (a b)
  (declare (type complex-type a b))
  (* a b))

(definline complex-type.f/ (a b)
  (declare (type complex-type a b))
  (/ a b))

(definline complex-type.finv* (a)
  (declare (type complex-type a))
  (/ a))

(definline complex-type.fid* ()
  #c(1.0d0 0.0d0))

;;Store operations
(definline allocate-complex-store (size)
  "
  (allocate-complex-store size)
  Allocates real storage of size (* SIZE 2).
  Default initial-element = 0d0.
"
  (make-array (* 2 size) :element-type 'complex-base-type
	      :initial-element (coerce 0 'complex-base-type)))

(definline coerce-complex-unforgiving (x)
  (coerce x 'complex-type))

(defun coerce-complex (x)
  (restart-case (coerce-complex-unforgiving x)
    (use-value (value) (coerce-complex value))))

(definline coerce-complex-base-unforgiving (x)
  (coerce x 'complex-base-type))

(defun coerce-complex-base (x)
  (restart-case (coerce-complex-base-unforgiving x)
    (use-value (value) (coerce-complex-base value))))
;;
(definline complex-type.reader (tstore idx)
  (declare (type complex-store-vector tstore)
	   (type index-type idx))
  (complex (aref tstore (* 2 idx))
	   (aref tstore (1+ (* 2 idx)))))

(definline complex-type.value-writer (value store idx)
  (declare (type complex-store-vector store)
	   (type index-type idx)
	   (type complex-type value))
  (setf (aref store (* 2 idx)) (realpart value)
	(aref store (1+ (* 2 idx))) (imagpart value)))

(definline complex-type.reader-writer (fstore fidx tstore tidx)
  (declare (type complex-store-vector fstore tstore)
	   (type index-type fidx tidx))
  (setf (aref tstore (* 2 tidx)) (aref fstore (* 2 fidx))
	(aref tstore (1+ (* 2 tidx))) (aref fstore (1+ (* 2 fidx)))))

(definline complex-type.swapper (fstore fidx tstore tidx)
  (declare (type complex-store-vector fstore tstore)
	   (type index-type fidx tidx))
  (rotatef (aref tstore (* 2 tidx)) (aref fstore (* 2 fidx)))
  (rotatef (aref tstore (1+ (* 2 tidx))) (aref fstore (1+ (* 2 fidx)))))
;;

(define-tensor (complex-tensor complex-type complex-base-type complex-store-vector
                (:documentation "Tensor class with complex elements."))
  :matrix complex-matrix :vector complex-vector
  ;;
  :f+ complex-type.f+
  :f- complex-type.f-
  :finv+ complex-type.finv+
  :fid+ complex-type.fid+
  :f* complex-type.f*
  :f/ complex-type.f/
  :finv* complex-type.finv*
  :fid* complex-type.fid*
  ;;
  :store-allocator allocate-complex-store
  :coercer coerce-complex
  :coercer-unforgiving coerce-complex-unforgiving
  ;;
  :matrix complex-matrix :vector complex-vector
  ;;
  :reader complex-type.reader
  :value-writer complex-type.value-writer
  :reader-writer complex-type.reader-writer
  :swapper complex-type.swapper)

;;
(defmethod initialize-instance ((tensor complex-tensor) &rest initargs)
  (if (getf initargs :store)
      ;;Two values count for one complex value.
      (setf (store-size tensor) (floor (length (getf initargs :store)) 2))
      (let ((size (reduce #'* (getf initargs :dimensions))))
	(setf (store tensor) (allocate-complex-store size)
	      (store-size tensor) size)))
  (call-next-method))

;;
(defmethod print-element ((tensor complex-tensor)
			  element stream)
  (let ((realpart (realpart element))
	(imagpart (imagpart element)))
    (format stream (if (zerop imagpart)
		       "~11,5,,,,,'Eg"
		       "#C(~11,4,,,,,'Ee ~11,4,,,,,'Ee)")
	    realpart imagpart)))

(defmethod (setf tensor-ref) ((value number) (tensor complex-tensor) subscripts)
  (let ((sto-idx (store-indexing subscripts tensor)))
    (setf (tensor-store-ref tensor sto-idx) (coerce-complex value))))
