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
(defvar *tensor-type-leaves* nil "
  This is used to keep track of classes that are not meant to be
  abstract classes. This prevents less specialized methods from
  clobbering the generation of more sophisticated (read faster)
  methods.")

(defmacro defleaf (name direct-superclasses direct-slots &rest options)
  `(progn
     (defclass ,name ,direct-superclasses ,direct-slots ,@options)
     (setf *tensor-type-leaves* (setadd *tensor-type-leaves* ',name))))

(defclass standard-tensor ()
  ((dimensions :reader dimensions :initarg :dimensions :type index-store-vector
    :documentation "Dimensions of the vector spaces in which the tensor's arguments reside.")
   ;;
   (parent-tensor :reader parent-tensor :initform nil :initarg :parent-tensor :type (or null standard-tensor)
    :documentation "If the tensor is a view of another tensor, then this slot is bound.")
   ;;
   (head :initarg :head :initform 0 :reader head :type index-type
    :documentation "Head for the store's accessor.")
   (strides :initarg :strides :reader strides :type index-store-vector
    :documentation "Strides for accesing elements of the tensor.")
   (store :initarg :store :reader store
    :documentation "The actual storage for the tensor.")
   ;;
   (attributes :initarg :attributes :initform nil
    :documentation "Place for computable attributes of an object instance."))
  (:documentation "Basic tensor class."))

;;Create hash-table only when necessary
(definline attributes (x)
  (declare (type standard-tensor x))
  (or (slot-value x 'attributes)
      (let ((htbl (make-hash-table)))
	(setf (slot-value x 'attributes) htbl)
	htbl)))

(declaim (ftype (function (standard-tensor) index-store-vector) strides dimensions)
	 (ftype (function (standard-tensor) index-type) head)
	 (ftype (function (standard-tensor) hash-table) attributes))

(defmacro memoizing ((tensor name) &rest body)
  (declare (type symbol name))
  (with-gensyms (tens)
    `(let* ((,tens ,tensor))
       (declare (type standard-tensor ,tens))
       (multiple-value-bind (value present?) (gethash ',name (attributes ,tens))
	 (values-list 
	  (if present?
	      value
	      (setf (gethash ',name (attributes ,tens))
		    (multiple-value-list (progn ,@body)))))))))
		      
;;I have no idea what this does, or why we want it (inherited from standard-matrix.lisp)
(defmethod make-load-form ((tensor standard-tensor) &optional env)
  "
  MAKE-LOAD-FORM allows us to determine a load time value for
  tensor, for example #.(make-tensors ...)"
  (make-load-form-saving-slots tensor :environment env))

;;These should ideally be memoised (or not)
(definline rank (tensor)
  (declare (type standard-tensor tensor))
  (length (the index-store-vector (dimensions tensor))))

(definline size (tensor)
  (declare (type standard-tensor tensor))
  (lvec-foldr #'* (the index-store-vector (dimensions tensor))))

(definline dims (tensor)
  (declare (type standard-tensor tensor))
  (memoizing (tensor dims)
	     (lvec->list (the index-store-vector (dimensions tensor)))))

;;
(defgeneric store-size (tensor)
  (:documentation "
  Syntax
  ======
  (store-size tensor)

  Purpose
  =======
  Returns the number of elements the store of the tensor can hold
  (which is not necessarily equal to its vector length).")
  (:method ((tensor standard-tensor))
    (let ((clname (class-name (class-of tensor))))
      (assert (member clname *tensor-type-leaves*) nil 'tensor-abstract-class :tensor-class clname)
      (compile-and-eval
       `(defmethod store-size ((tensor ,clname))
	  (t/store-size ,clname (store tensor))))
      (store-size tensor))))

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
  (:method ((tensor standard-tensor) element stream)
    (format stream "~a" element)))

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
	 :with sto-idx :of-type index-type := hd
	 :do (progn
	       (assert (< -1 cidx (aref dims i)) nil 'tensor-index-out-of-bounds :argument i :index cidx :dimension (aref dims i))
	       (incf sto-idx (the index-type (* (aref strides i) cidx))))
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
    (assert (= rank (length dims)) nil 'tensor-dimension-mismatch)
    (very-quickly
      (loop :for cidx :of-type index-type :in idx
	 :for i :of-type index-type := 0 :then (1+ i)
	 :with sto-idx :of-type index-type := hd
	 :do (progn
	       (assert (< -1 cidx (aref dims i)) nil 'tensor-index-out-of-bounds :argument i :index cidx :dimension (aref dims i))
	       (incf sto-idx (the index-type (* (aref strides i) cidx))))
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
;;

(defmacro with-order (order &rest code)
  `(let ((*default-stride-ordering* ,order))
     ,@code))

;;
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
	      (loop :for i :of-type index-type :from 0 :below (rank tensor)
		 :for sz :of-type index-type := (aref dims 0) :then (the index-type (* sz (aref dims i)))
		 :for lidx :of-type index-type := (the index-type (* (aref stds 0) (1- (aref dims 0)))) :then (the index-type (+ lidx (the index-type (* (aref stds i) (1- (aref dims i))))))
		 :do (progn
		       (assert (> (aref stds i) 0) nil 'tensor-invalid-stride-value :argument i :stride (aref stds i) :tensor tensor)
		       (assert (> (aref dims i) 0) nil 'tensor-invalid-dimension-value :argument i :dimension (aref dims i) :tensor tensor))
		 :finally (assert (>= (the index-type (store-size tensor)) (the index-type (+ (the index-type (head tensor)) lidx))) nil 'tensor-insufficient-store :store-size (store-size tensor) :max-idx lidx :tensor tensor))))))))

;;
(defgeneric ref (tensor &rest subscripts)
  (:documentation "
  Syntax
  ======
  (ref store subscripts)

  Purpose
  =======
  Return the element:

    (rank - 1)
       __
  hd + \   stride  * sub
       /_        i      i
      i = 0

  of the store.")
  (:method ((tensor standard-tensor) &rest subscripts)
    (let ((clname (class-name (class-of tensor))))
      (assert (member clname *tensor-type-leaves*) nil 'tensor-abstract-class :tensor-class clname)
      (compile-and-eval
       `(defmethod ref ((tensor ,clname) &rest subscripts)
	  (let ((subs (if (numberp (car subscripts)) subscripts (car subscripts))))
	    (t/store-ref ,clname (store tensor) (store-indexing subs tensor)))))
      (apply #'ref (cons tensor subscripts)))))

(defgeneric (setf ref) (value tensor &rest subscripts)
  (:method (value (tensor standard-tensor) &rest subscripts)
    (let ((clname (class-name (class-of tensor))))
      (assert (member clname *tensor-type-leaves*) nil 'tensor-abstract-class :tensor-class clname)
      (compile-and-eval
       `(defmethod (setf ref) (value (tensor ,clname) &rest subscripts)
	  (let* ((subs (if (numberp (car subscripts)) subscripts (car subscripts)))
		 (idx (store-indexing subs tensor)))
	    (t/store-set ,clname value (store tensor) idx)
	    (t/store-ref ,clname (store tensor) idx))))
    (setf (apply #'ref (cons tensor subscripts)) value))))

(defgeneric store-ref (tensor idx)
  (:documentation  "Generic serial read access to the store.")
  (:method ((tensor standard-tensor) idx)
    (let ((clname (class-name (class-of tensor))))
      (assert (member clname *tensor-type-leaves*) nil 'tensor-abstract-class :tensor-class clname)
      (compile-and-eval
       `(defmethod store-ref ((tensor ,clname) idx)
	  (t/store-ref ,clname (store tensor) idx))))
    (store-ref tensor idx)))

(defgeneric (setf store-ref) (value tensor idx)
  (:method (value (tensor standard-tensor) idx)
    (let ((clname (class-name (class-of tensor))))
      (assert (member clname *tensor-type-leaves*) nil 'tensor-abstract-class :tensor-class clname)
      (compile-and-eval
       `(defmethod (setf store-ref) (value (tensor ,clname) idx)
	  (t/store-set ,clname value (store tensor) idx)
	  (t/store-ref ,clname (store tensor) idx))))
    (setf (store-ref tensor idx) value)))

;;
(defun tensor-typep (tensor subs)
  "
  Syntax
  ======
  (tensor-typep tensor subscripts)

  Purpose
  =======
  Check if the given tensor is of a particular size in particular
  arguments.

  Examples
  ========
  Checking for a vector:
  > (tensor-typep ten '(class-name *))

  Checking for a matrix with 2 columns:
  > (tensor-typep ten '(real-tensor (* 2)))

  "
  (declare (type standard-tensor tensor))
  (destructuring-bind (cls &optional subscripts) (ensure-list subs)
    (and (typep tensor cls)
	 (if subscripts
	     (let-typed ((rank (rank tensor) :type index-type)
			 (dims (dimensions tensor) :type index-store-vector))
			(very-quickly 
			  (loop :for val :in subscripts
			     :for i :of-type index-type := 0 :then (1+ i)
			     :do (unless (or (eq val '*) (eq val (aref dims i)))
				   (return nil))
			     :finally (return (when (= (1+ i) rank) t)))))
	     t))))

(definline tensor-matrixp (ten)
  (declare (type standard-tensor ten))
  (= (rank ten) 2))

(definline tensor-vectorp (ten)
  (declare (type standard-tensor ten))
  (= (rank ten) 1))

(definline tensor-squarep (tensor)
  (declare (type standard-tensor tensor))
  (let-typed ((dims (dimensions tensor) :type index-store-vector))
	     (loop :for i :from 1 :below (length dims)
		:do (unless (= (aref dims i) (aref dims 0))
		      (return nil))
		:finally (return t))))

;;
(defun subtensor~ (tensor subscripts &optional (preserve-rank nil))
  "
  Syntax
  ======
  (SUBTENSOR~ TENSOR SUBSCRIPTS)

  Purpose
  =======
  Creates a new tensor data structure, sharing store with
  TENSOR but with different strides and dimensions, as defined
  in the subscript-list SUBSCRIPTS.

  Examples
  ========
  > (defvar X (make-real-tensor 10 10 10))
  X

  ;; Get (:, 0, 0)
  > (subtensor~ X '((* * *) (0 * 1) (0 * 1)))

  ;; Get (:, 2:5, :)
  > (subtensor~ X '((* * *) (2 * 5)))

  ;; Get (:, :, 0:2:10) (0:10:2 = [i : 0 <= i < 10, i % 2 = 0])
  > (subtensor~ X '((* * *) (* * *) (0 2 10)))

  Commentary
  ==========
  Sadly in our parentheses filled world, this function has to be necessarily
  verbose (unlike MATLAB, Python). However, this function has been designed with the
  express purpose of using it with a Lisp reader macro. The slicing semantics is
  essentially the same as MATLAB except for the zero-based indexing.
"
  (declare (type standard-tensor tensor)
	   (type list subscripts)
	   (type boolean preserve-rank))
  (if (null subscripts)
      (let ((*check-after-initializing?* nil))
	(make-instance (class-of tensor)
		       :head (head tensor)
		       :dimensions (copy-seq (dimensions tensor))
		       :strides (copy-seq (strides tensor))
		       :store (store tensor)
		       :parent-tensor tensor))		       
      (let-typed ((dims (dimensions tensor) :type index-store-vector)
		  (stds (strides tensor) :type index-store-vector)
		  (rank (rank tensor) :type index-type))
		 (loop :for (start step end) :in subscripts
		    :for i :of-type index-type := 0 :then (1+ i)
		    :with ndims :of-type index-store-vector := (allocate-index-store rank)
		    :with nstds :of-type index-store-vector := (allocate-index-store rank)
		    :with nrank :of-type index-type := 0
		    :with nhd :of-type index-type := (head tensor)
		    :do (assert (< i rank) nil 'tensor-index-rank-mismatch :index-rank (1+ i) :rank rank)
		    :do (let* ((start (if (eq start '*) 0
					  (progn
					    (assert (and (typep start 'index-type) (< -1 start (aref dims i))) nil 'tensor-index-out-of-bounds :argument i :index start :dimension (aref dims i))
					    start)))
			       (step (if (eq step '*) 1
					 (progn
					   (assert (and (typep step 'index-type) (< 0 step)) nil 'invalid-value :given step :expected '(< 0 step) :message "STEP cannot be <= 0.")
					   step)))
			       (end (if (eq end '*) (aref dims i)
					(progn
					  (assert (and (typep end 'index-type) (<= 0 end (aref dims i))) nil 'tensor-index-out-of-bounds :argument i :index start :dimension (aref dims i))
					  end))))
			  (declare (type index-type start step end))
			  ;;
			  (let-typed ((dim (ceiling (the index-type (- end start)) step) :type index-type))
				     (unless (and (= dim 1) (not preserve-rank))
				       (setf (aref ndims nrank) dim
					     (aref nstds nrank) (* step (aref stds i)))
				       (incf nrank))
				     (when (/= start 0)
				       (incf nhd (the index-type (* start (aref stds i)))))))
		    :finally (return
			       (if (= nrank 0) (store-ref tensor nhd)
				   (let ((*check-after-initializing?* nil))
				     (make-instance (class-of tensor)
						    :head nhd
						    :dimensions (very-quickly (vectorify (the index-store-vector ndims) nrank 'index-type))
						    :strides (very-quickly (vectorify (the index-store-vector nstds) nrank 'index-type))
						    :store (store tensor)
						    :parent-tensor tensor))))))))

(definline slice~ (x axis &optional (idx 0))
  (let ((slst (make-list (rank x) :initial-element '(* * *))))
    (rplaca (nthcdr axis slst) (list idx '* (1+ idx)))
    (subtensor~ x slst nil)))

(definline row-slice~ (x idx)
  (slice~ x 0 idx))

(definline col-slice~ (x idx)
  (slice~ x 1 idx))
