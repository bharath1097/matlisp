(in-package #:matlisp)

(deftype real-type ()
  "The type of the elements stored in a REAL-MATRIX"
  'double-float)

(deftype real-store-vector (&optional (size '*))
  "The type of the storage structure for a REAL-MATRIX"
  `(simple-array real-type (,size)))

;;Field definitions
(definline real-typed.f+ (a b)
  (declare (type real-type a b))
  (+ a b))

(definline real-typed.f- (a b)
  (declare (type real-type a b))
  (- a b))

(definline real-typed.finv+ (a)
  (declare (type real-type a))
  (- a))

(definline real-typed.fid+ ()
  0.0d0)

(definline real-typed.f* (a b)
  (declare (type real-type a b))
  (* a b))

(definline real-typed.f/ (a b)
  (declare (type real-type a b))
  (/ a b))

(definline real-typed.finv* (a)
  (declare (type real-type a))
  (/ a))

(definline real-typed.fid* ()
  1.0d0)

(definline real-typed.fid= (a b)
  (declare (type real-type a b))
  (= a b))

;;Store definitions
(definline real-typed.reader (tstore idx)
  (declare (type index-type idx)
	   (type real-store-vector tstore))
  (aref tstore idx))

(definline real-typed.value-writer (value store idx)
  (declare (type index-type idx)
	   (type real-store-vector store)
	   (type real-type value))
  (setf (aref store idx) value))

(definline real-typed.value-incfer (value store idx)
  (declare (type index-type idx)
	   (type real-store-vector store)
	   (type real-type value))
  (incf (aref store idx) value))

(definline real-typed.reader-writer (fstore fidx tstore tidx)
  (declare (type index-type fidx tidx)
	   (type real-store-vector fstore tstore))
  (setf (aref tstore tidx) (aref fstore fidx)))

(definline real-typed.swapper (fstore fidx tstore tidx)
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
  :f+ real-typed.f+
  :f- real-typed.f-
  :finv+ real-typed.finv+
  :fid+ real-typed.fid+
  :f* real-typed.f*
  :f/ real-typed.f/
  :finv* real-typed.finv*
  :fid* real-typed.fid*
  :f= real-typed.fid=
  :fconj nil
  ;;
  :store-allocator allocate-real-store
  :coercer coerce-real
  :coercer-unforgiving coerce-real-unforgiving
  ;;
  :reader real-typed.reader
  :value-writer real-typed.value-writer
  :value-incfer real-typed.value-incfer
  :reader-writer real-typed.reader-writer
  :swapper real-typed.swapper)

(defmethod print-element ((tensor real-tensor)
			  element stream)
  (format stream "~11,5,,,,,'Eg" element))
