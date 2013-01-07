(in-package #:matlisp)

(deftype real-type ()
  "The type of the elements stored in a REAL-MATRIX"
  'double-float)

(deftype real-store-vector (&optional (size '*))
  "The type of the storage structure for a REAL-MATRIX"
  `(simple-array real-type (,size)))

;;Field definitions
(definline real-type.f+ (a b)
  (declare (type real-type a b))
  (+ a b))

(definline real-type.f- (a b)
  (declare (type real-type a b))
  (- a b))

(definline real-type.finv+ (a)
  (declare (type real-type a))
  (- a))

(definline real-type.fid+ ()
  0.0d0)

(definline real-type.f* (a b)
  (declare (type real-type a b))
  (* a b))

(definline real-type.f/ (a b)
  (declare (type real-type a b))
  (/ a b))

(definline real-type.finv* (a)
  (declare (type real-type a))
  (/ a))

(definline real-type.fid* ()
  1.0d0)

(definline real-type.fid= (a b)
  (declare (type real-type a b))
  (= a b))

;;Store definitions
(definline real-type.reader (tstore idx)
  (declare (type index-type idx)
	   (type real-store-vector tstore))
  (aref tstore idx))

(definline real-type.value-writer (value store idx)
  (declare (type index-type idx)
	   (type real-store-vector store)
	   (type real-type value))
  (setf (aref store idx) value))

(definline real-type.reader-writer (fstore fidx tstore tidx)
  (declare (type index-type fidx tidx)
	   (type real-store-vector fstore tstore))
  (setf (aref tstore tidx) (aref fstore fidx)))

(definline real-type.swapper (fstore fidx tstore tidx)
  (declare (type index-type fidx tidx)
	   (type real-store-vector fstore tstore))
  (rotatef (aref tstore tidx) (aref fstore fidx)))

;;
(make-array-allocator allocate-real-store 'real-type 0d0
"(allocate-real-store size [initial-element])
Allocates real storage.  Default initial-element = 0d0.")

(definline coerce-real-unforgiving (x)
  (coerce x 'real-type))

(defun coerce-real (x)
  (restart-case (coerce-real-unforgiving x)
    (use-value (value) (coerce-real value))))

;;
(define-tensor (real-tensor real-type real-type real-store-vector
		(:documentation "Tensor class with real double elements."))
  :matrix real-matrix :vector real-vector
  ;;
  :f+ real-type.f+
  :f- real-type.f-
  :finv+ real-type.finv+
  :fid+ real-type.fid+
  :f* real-type.f*
  :f/ real-type.f/
  :finv* real-type.finv*
  :fid* real-type.fid*
  :f= real-type.fid=
  :fconj nil
  ;;
  :store-allocator allocate-real-store
  :coercer coerce-real
  :coercer-unforgiving coerce-real-unforgiving
  ;;
  :reader real-type.reader
  :value-writer real-type.value-writer
  :reader-writer real-type.reader-writer
  :swapper real-type.swapper)

;;
(defmethod initialize-instance ((tensor real-tensor) &rest initargs)
  (if (getf initargs :store)
      (setf (slot-value tensor 'store-size) (length (getf initargs :store)))
      (let ((size (reduce #'* (getf initargs :dimensions))))
	(setf (slot-value tensor 'store) (allocate-real-store size)
	      (slot-value tensor 'store-size) size)))
  (call-next-method))

;;
(defmethod (setf tensor-ref) ((value number) (tensor real-tensor) subscripts)
  (let ((sto-idx (store-indexing subscripts tensor)))
    (setf (tensor-store-ref tensor sto-idx) (coerce-real value))))

(defmethod print-element ((tensor real-tensor)
			  element stream)
  (format stream "~11,5,,,,,'Eg" element))
