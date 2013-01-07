(in-package #:matlisp)

(deftype symbolic-type ()
  "Symbolic type associated with Maxima"
  '(or number symbol list))

(deftype symbolic-store-vector (&optional (size '*))
  "The type of the storage structure for a REAL-MATRIX"
  `(simple-array symbolic-type (,size)))

;;Field definitions
(definline symbolic-type.f+ (a b)
  (declare (type symbolic-type a b))
  (maxima::add a b))

(definline symbolic-type.f- (a b)
  (declare (type symbolic-type a b))
  (maxima::sub a b))

(definline symbolic-type.finv+ (a)
  (declare (type symbolic-type a))
  (maxima::mul -1 a))

(definline symbolic-type.fid+ ()
  0)

(definline symbolic-type.f* (a b)
  (declare (type symbolic-type a b))
  (maxima::mul a b))

(definline symbolic-type.f/ (a b)
  (declare (type symbolic-type a b))
  (maxima::div a b))

(definline symbolic-type.finv* (a)
  (declare (type symbolic-type a))
  (maxima::div 1 a))

(definline symbolic-type.fid* ()
  1)

(definline symbolic-type.f= (a b)
  (declare (type symbolic-type a b))
  (maxima::equal a b))

(definline symbolic-type.fconj (a)
  (maxima::meval `((maxima::$conjugate maxima::simp) ,a)))

(definline symbolic-type.diff (a x)
  (etypecase a
    (symbolic-type
     (maxima::$diff a x))
    (symbolic-tensor
     (make-instance 'symbolic-tensor
		    :dimensions (copy-seq (dimensions a))
		    :store (map 'symbolic-store-vector #'(lambda (f) (maxima::$diff f x)) (store a))))))
;;
;;Store definitions
(definline symbolic-type.reader (tstore idx)
  (declare (type index-type idx)
	   (type symbolic-store-vector tstore))
  (aref tstore idx))

(definline symbolic-type.value-writer (value store idx)
  (declare (type index-type idx)
	   (type symbolic-store-vector store)
	   (type symbolic-type value))
  (setf (aref store idx) value))

(definline symbolic-type.reader-writer (fstore fidx tstore tidx)
  (declare (type index-type fidx tidx)
	   (type symbolic-store-vector fstore tstore))
  (setf (aref tstore tidx) (aref fstore fidx)))

(definline symbolic-type.swapper (fstore fidx tstore tidx)
  (declare (type index-type fidx tidx)
	   (type symbolic-store-vector fstore tstore))
  (rotatef (aref tstore tidx) (aref fstore fidx)))

;;
(make-array-allocator allocate-symbolic-store 'symbolic-type 0
"(allocate-symbolic-store size [initial-element])
Allocates symbolic storage.  Default initial-element = 0.")

(definline coerce-symbolic-unforgiving (x)
  (coerce x 'symbolic-type))

(defun coerce-symbolic (x)
  (restart-case (coerce-symbolic-unforgiving x)
    (use-value (value) (coerce-symbolic value))))

(define-tensor (symbolic-tensor symbolic-type symbolic-type symbolic-store-vector
		(:documentation "Tensor class with symbolic double elements."))
  :matrix symbolic-matrix :vector symbolic-vector
  ;;
  :f+ symbolic-type.f+
  :f- symbolic-type.f-
  :finv+ symbolic-type.finv+
  :fid+ symbolic-type.fid+
  :f* symbolic-type.f*
  :f/ symbolic-type.f/
  :finv* symbolic-type.finv*
  :fid* symbolic-type.fid*
  :f= symbolic-type.f=
  :fconj symbolic-type.fconj
  ;;
  :store-allocator allocate-symbolic-store
  :coercer coerce-symbolic
  :coercer-unforgiving coerce-symbolic-unforgiving
  ;;
  :reader symbolic-type.reader
  :value-writer symbolic-type.value-writer
  :reader-writer symbolic-type.reader-writer
  :swapper symbolic-type.swapper)

(defmethod initialize-instance ((tensor symbolic-tensor) &rest initargs)
  (if (getf initargs :store)
      (setf (slot-value tensor 'store-size) (length (getf initargs :store)))
      (let ((size (reduce #'* (getf initargs :dimensions))))
	(setf (slot-value tensor 'store) (allocate-symbolic-store size)
	      (slot-value tensor 'store-size) size)))
  (call-next-method))

;;
(defmethod (setf tensor-ref) ((value number) (tensor symbolic-tensor) subscripts)
  (let ((sto-idx (store-indexing subscripts tensor)))
    (setf (tensor-store-ref tensor sto-idx) (coerce-symbolic value))))

(defmethod print-element ((tensor symbolic-tensor)
			  element stream)
  (format stream "~a" element))
