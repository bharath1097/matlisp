(in-package #:matlisp)

(defclass linear-store ()
  ((head :initarg :head :initform 0 :reader head :type index-type
    :documentation "Head for the store's accessor.")
   (strides :initarg :strides :type index-store-vector
    :documentation "Strides for accesing elements of the tensor.")
   (store :initarg :store :reader store :type vector
    :documentation "The actual storage for the tensor.")))

(declaim (ftype (function (base-tensor &optional index-type) (or index-type index-store-vector)) strides)
	 (ftype (function (base-tensor) index-type) head))

(definline strides (x &optional idx)
  (declare (type base-tensor x))
  (if idx
      (aref (the index-store-vector (slot-value x 'strides)) (modproj idx (order x) nil 0))
      (the index-store-vector (slot-value x 'strides))))

;;
(defun store-indexing-vec (idx hd strides dims)
"
  Syntax
  ======
  (STORE-INDEXING-VEC IDX HD STRIDES DIMS)

  Purpose
  =======
  Does error checking to make sure IDX is not out of bounds.
  Returns the sum:

    length(STRIDES)
       __
  HD + \  STRIDE  * IDX
       /_        i      i
     i = 0
"
  (declare (type index-type hd)
	   (type index-store-vector idx strides dims))
  (let-typed ((rank (length strides) :type index-type))
    (assert (= rank (length idx) (length dims)) nil 'tensor-index-rank-mismatch :index-rank (length idx) :rank rank)
    (very-quickly
      (loop
	 :for i :of-type index-type :from 0 :below rank
	 :for cidx :across idx
	 :for d :across dims
	 :for s :across strides
	 :with sto-idx :of-type index-type := hd
	 :do (progn
	       (assert (< (1- (- d)) cidx d) nil 'tensor-index-out-of-bounds :argument i :index cidx :dimension d)
	       (incf sto-idx (the index-type (* s (if (< cidx 0) (mod cidx d) cidx)))))
	 :finally (return sto-idx)))))

(defun store-indexing-lst (idx hd strides dims)
"
  Syntax
  ======
  (STORE-INDEXING-LST IDX HD STRIDES DIMS)

  Purpose
  =======
  Does error checking to make sure idx is not out of bounds.
  Returns the sum:

    length(STRIDES)
       __
  HD + \  STRIDE  * IDX
       /_        i      i
     i = 0
"
  (declare (type index-type hd)
	   (type index-store-vector strides dims)
	   (type cons idx))
  (let-typed ((rank (length strides) :type index-type))
    (very-quickly
      (loop :for cidx :of-type index-type :in idx
	 :for i :of-type index-type := 0 :then (1+ i)
	 :for d :across dims
	 :for s :across strides
	 :with sto-idx :of-type index-type := hd
	 :do (progn
	       (assert (< (1- (- d)) cidx d) nil 'tensor-index-out-of-bounds :argument i :index cidx :dimension d)
	       (incf sto-idx (the index-type (* s (if (< cidx 0) (mod cidx d) cidx)))))
	 :finally (progn
		    (assert (= (1+ i) rank) nil 'tensor-index-rank-mismatch :index-rank (1+ i) :rank rank)
		    (return sto-idx))))))

(definline store-indexing (idx tensor)
"
  Syntax
  ======
  (STORE-INDEXING IDX TENSOR)

  Purpose
  =======
  Returns the linear index of the element pointed by IDX.
  Does error checking to make sure idx is not out of bounds.
  Returns the sum:

    length(STRIDES)
       __
  HD + \  STRIDES  * IDX
       /_        i      i
     i = 0
"
  (etypecase idx
    (cons (store-indexing-lst idx (head tensor) (strides tensor) (dimensions tensor)))
    (vector (store-indexing-vec idx (head tensor) (strides tensor) (dimensions tensor)))))

;;Stride makers.
(macrolet ((defstride (fname col?)
	     `(definline ,fname (dims)
		(declare (type index-store-vector dims))
		(let-typed ((stds (allocate-index-store (length dims)) :type index-store-vector))
		  (very-quickly
		    (iter
		      ,(if col?
			   `(for i from 0 below (length dims))
			   `(for i from (1- (length dims)) downto 0))
		      (declare (type index-type i))
		      (with st = 1) (declare (type index-type st))
		      (let-typed ((d (aref dims i) :type index-type))
			(assert (> d 0) nil 'tensor-invalid-dimension-value :argument i :dimension d)
			(setf (aref stds i) st
			      st (* st d)))
		      (finally (return (values stds st)))))))))
  (defstride make-stride-cmj t)
  (defstride make-stride-rmj nil)
  (definline make-stride (dims)
    (ecase *default-stride-ordering* (:row-major (make-stride-rmj dims)) (:col-major (make-stride-cmj dims)))))

;;Is it a tensor, is a store ? It is both!
(defclass standard-tensor (dense-tensor linear-store) ())

(defmethod initialize-instance :after ((tensor standard-tensor) &rest initargs)
  (declare (ignore initargs))
  (when *check-after-initializing?*
    (let-typed ((dims (dimensions tensor) :type index-store-vector))
      (assert (>= (head tensor) 0) nil 'tensor-invalid-head-value :head (head tensor) :tensor tensor)
      (if (not (slot-boundp tensor 'strides))
	  (multiple-value-bind (stds size) (make-stride dims)
	    (declare (type index-store-vector stds)
		     (type index-type size))
	    (setf (slot-value tensor 'strides) stds)
	    (assert (<= (+ (head tensor) size) (store-size tensor)) nil 'tensor-insufficient-store :store-size (store-size tensor) :max-idx (+ (head tensor) (1- (size tensor))) :tensor tensor))
	  (very-quickly
	    (let-typed ((stds (strides tensor) :type index-store-vector))
	      (loop :for i :of-type index-type :from 0 :below (order tensor)
		 :for sz :of-type index-type := (aref dims 0) :then (the index-type (* sz (aref dims i)))
		 :summing (the index-type (the index-type (* (aref stds i) (1- (aref dims i))))) :into lidx :of-type index-type 
		 :do (assert (> (aref dims i) 0) nil 'tensor-invalid-dimension-value :argument i :dimension (aref dims i) :tensor tensor)
		 :finally (assert (>= (the index-type (store-size tensor)) (the index-type (+ (the index-type (head tensor)) lidx)) 0) nil 'tensor-insufficient-store :store-size (store-size tensor) :max-idx (the index-type (+ (head tensor) lidx)) :tensor tensor))))))))

(defmethod ref ((tensor standard-tensor) &rest subscripts)
  (let ((clname (class-name (class-of tensor))))
    (assert (member clname *tensor-type-leaves*) nil 'tensor-abstract-class :tensor-class clname)
    (compile-and-eval
     `(defmethod ref ((tensor ,clname) &rest subscripts)
	(let ((subs (if (numberp (car subscripts)) subscripts (car subscripts))))
	  (t/store-ref ,clname (store tensor) (store-indexing subs tensor)))))
    (apply #'ref (cons tensor subscripts))))

(defmethod (setf ref) (value (tensor standard-tensor) &rest subscripts)
  (let ((clname (class-name (class-of tensor))))
    (assert (member clname *tensor-type-leaves*) nil 'tensor-abstract-class :tensor-class clname)
    (compile-and-eval
     `(defmethod (setf ref) (value (tensor ,clname) &rest subscripts)
	(let* ((subs (if (numberp (car subscripts)) subscripts (car subscripts)))
	       (idx (store-indexing subs tensor))
	       (sto (store tensor)))
	  (t/store-set ,clname (t/coerce ,(field-type clname) value) sto idx)
	  (t/store-ref ,clname sto idx))))
    (setf (ref tensor (if (numberp (car subscripts)) subscripts (car subscripts))) value)))

;;
(defmethod subtensor~ ((tensor standard-tensor) (subscripts list))
  (multiple-value-bind (hd dims stds) (parse-slice-for-strides subscripts (dimensions tensor) (strides tensor))
    (cond
      ((not hd) nil)
      ((not dims) (store-ref tensor hd))
      (t (with-no-init-checks
	     (make-instance (class-of tensor)
			    :head (+ hd (head tensor))
			    :dimensions (make-index-store dims)
			    :strides (make-index-store stds)
			    :store (store tensor)
			    :parent-tensor tensor))))))

(defmethod suptensor~ ((ten standard-tensor) ord &optional (start 0))
  (declare (type index-type ord start))
  (if (= (order ten) ord) ten
      (let* ((tord (order ten)))
	(with-no-init-checks
	    (make-instance (class-of ten)
			   :dimensions (make-index-store
					(nconc (make-list start :initial-element 1)
					       (lvec->list (dimensions ten))
					       (make-list (- ord tord start) :initial-element 1)))
			   :strides (make-index-store
				     (nconc (make-list start :initial-element (size ten))
					    (lvec->list (strides ten))
					    (make-list (- ord tord start) :initial-element (size ten))))
			   :head (head ten)
			   :store (store ten)
			   :parent-tensor ten)))))
