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
  (the index-store-vector (make-array (length contents) :element-type 'index-type
				      :initial-contents contents)))

(definline idxv (&rest contents)
  (make-index-store contents))

;;This is a silly hack. The plan is to get rid of this part using MOP.
(defvar *tensor-type-leaves* nil "
  This is used to keep track of classes that are not meant to be
  abstract classes. This prevents less specialized methods from
  clobbering the generation of more sophisticated (read faster)
  methods.")

(defmacro defleaf (name direct-superclasses direct-slots &rest options)
  `(eval-every
     (defclass ,name ,direct-superclasses ,direct-slots ,@options)
     (setf *tensor-type-leaves* (setadd *tensor-type-leaves* ',name))))
;;
(defclass base-tensor ()
  ((dimensions :initarg :dimensions :type index-store-vector
   :documentation "Dimensions of the vector spaces in which the tensor's arguments reside.")
   (parent-tensor :reader parent-tensor :initform nil :initarg :parent-tensor :type (or null base-tensor)
    :documentation "If the tensor is a view of another tensor, then this slot is bound.")
   (store :initarg :store :reader store
    :documentation "The actual storage for the tensor.")
   (attributes :initarg :attributes :initform nil
    :documentation "Place for computable attributes of an object instance."))
  (:documentation "Basic tensor class."))

(declaim (ftype (function (base-tensor &optional index-type) (or index-type index-store-vector)) dimensions)
	 (ftype (function (base-tensor) hash-table) attributes))

(definline orphanize (x)
  (setf (slot-value x 'parent-tensor) nil)
  x)

(definline dimensions (x &optional idx)
  (declare (type base-tensor x))
  (if idx
      (aref (the index-store-vector (slot-value x 'dimensions)) (modproj idx (order x) nil 0))
      (the index-store-vector (slot-value x 'dimensions))))

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
  (length (the index-store-vector (slot-value tensor 'dimensions))))
;; (definline tensor-rank (tensor) (order tensor))

;;
(defgeneric size (obj)
  (:method ((tensor base-tensor))
    (lvec-foldr #'(lambda (x y) (declare (type index-type x y)) (the index-type (* x y))) (the index-store-vector (dimensions tensor))))
  (:method ((obj sequence))
    (length obj))
  (:method ((arr array))
    (reduce #'* (array-dimensions arr))))

(definline dims (tensor &optional idx)
  (declare (type base-tensor tensor))
  (if idx (aref (dimensions tensor) (modproj idx (order tensor) nil 0))
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

(declaim (ftype (function (base-tensor &rest t) t) ref))
;;
(defgeneric store-ref (tensor idx)
  (:documentation  "Generic serial read access to the store.")
  (:method ((tensor base-tensor) idx)
    (let ((clname (class-name (class-of tensor))))
      (assert (member clname *tensor-type-leaves*) nil 'tensor-abstract-class :tensor-class clname)
      (compile-and-eval
       `(defmethod store-ref ((tensor ,clname) idx)
	  (t/store-ref ,clname (store tensor) idx))))
    (store-ref tensor idx)))

(defgeneric (setf store-ref) (value tensor idx)
  (:method (value (tensor base-tensor) idx)
    (let ((clname (class-name (class-of tensor))))
      (assert (member clname *tensor-type-leaves*) nil 'tensor-abstract-class :tensor-class clname)
      (compile-and-eval
       `(defmethod (setf store-ref) (value (tensor ,clname) idx)
	  (t/store-set ,clname value (store tensor) idx)
	  (t/store-ref ,clname (store tensor) idx))))
    (setf (store-ref tensor idx) value)))

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
  (:method ((tensor base-tensor))
    (let ((clname (class-name (class-of tensor))))
      (assert (member clname *tensor-type-leaves*) nil 'tensor-abstract-class :tensor-class clname)
      (compile-and-eval
       `(defmethod store-size ((tensor ,clname))
	  (t/store-size ,clname (store tensor))))
      (store-size tensor))))
;;
(defgeneric subtensor~ (tensor subscripts)
  (:documentation "
  Syntax
  ======
  (SUBTENSOR~ TENSOR SUBSCRIPTS &optional PRESERVE-RANK REF-SINGLE-ELEMENT?)

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
  > (subtensor~ X '((nil nil . nil) (0 1 . nil) (0 1 . nil)))

  ;; Get (:, 2:5, :)
  > (subtensor~ X '((nil nil . nil) (2 5 . nil)))

  ;; Get (:, :, 0:2:10) (0:10:2 = [i : 0 <= i < 10, i % 2 = 0])
  > (subtensor~ X '((nil nil . nil) (nil nil . nil) (0 10 . 2)))

  Commentary
  ==========
  Sadly in our parentheses filled world, this function has to be necessarily
  verbose (unlike MATLAB, Python). However, this function has been designed with the
  express purpose of using it with a Lisp reader macro. The slicing semantics is
  essentially the same as MATLAB except for the zero-based indexing.
"))

(defun (setf subtensor~) (value tensor subscripts)
  (copy! value (subtensor~ tensor subscripts)))

;;Helper functions
(definline modproj (i d &optional open? def)
  (cond
    ((not i) def)
    ((not d) i)
    (t (assert (if open? (<= (- (1+ d)) i d) (< (- (1+ d)) i d)) nil 'invalid-value)
       (if (< i 0) (if (and open? (= i (- (1+ d)))) -1 (mod i d)) i))))

(definline parse-slice (subs dimensions)
  (declare (type index-store-vector dimensions))
  (iter (for sub.i in subs)
	(for d in-vector dimensions) (declare (type index-type d))
	(if (not (consp sub.i))
	    (let ((idx (modproj (the (or index-type null) sub.i) d nil 0)))
	      (collect 1 into dims)
	      (collect idx into psubs))
	    (destructuring-bind (start end . inc) sub.i
	      (declare ((or index-type null) start end inc))
	      (let* ((inc (modproj inc nil nil 1))
		     (start (modproj start d nil (if (> inc 0) 0 (1- d))))
		     (end (modproj end d t (if (> inc 0) d -1)))
		     (nd (ceiling (- end start) inc)))
		(declare (type index-type start end inc nd))
		(when (<= nd 0) (return nil))
		(collect nd into dims)
		(collect (list* start end inc) into psubs))))
	(finally (return (values psubs dims)))))

(definline parse-slice-for-strides (subscripts dimensions strides)
  (declare (type index-store-vector dimensions strides)
	   (type list subscripts))
  (iter (for sub.i in subscripts)
	(for d in-vector dimensions)
	(for s in-vector strides)
	(with (the index-type hd) = 0)
	(if (not (consp sub.i))
	    (let ((idx (modproj (the (or index-type null) sub.i) d nil 0)))
	      (incf hd (* s idx)))
	    (destructuring-bind (start end . inc) sub.i
	      (declare ((or index-type null) start end inc))
	      (let* ((inc (modproj inc nil nil 1))
		     (start (modproj start d nil (if (> inc 0) 0 (1- d))))
		     (end (modproj end d t (if (> inc 0) d -1)))
		     (nd (ceiling (- end start) inc)))
		(declare (type index-type start end inc nd))
		(when (<= nd 0) (return nil))
		(incf hd (* s start))
		(collect nd into dims)
		(collect (* inc s) into stds))))
	(finally (return (values hd dims stds)))))

(definline slice~ (x axis &optional (idx 0) (preserve-rank? (when (= (order x) 1) t)))
  (let* ((axis (modproj axis (order x) nil 0))
	 (subs (iter (for i from 0 below (order x)) (collect (cond ((/= i axis) '(nil nil))
								   (preserve-rank? (list idx (1+ idx)))
								   (t idx))))))
    (subtensor~ x subs)))

(definline row-slice~ (x idx)
  (slice~ x 0 idx))

(definline col-slice~ (x idx)
  (slice~ x 1 idx))
;;
(defgeneric suptensor~ (tensor ord &optional start)
  (:method :before ((tensor base-tensor) ord &optional (start 0))
	   (declare (type index-type start))
	   (let ((tord (order tensor)))
	     (assert (and (< -1 start) (<= tord (order tensor)) (<= 0 start (- ord tord))) nil 'invalid-arguments))))

(definline matrixify~ (vec &optional (col-vector? t))
  (if (tensor-matrixp vec) vec (suptensor~ vec 2 (if col-vector? 0 1))))
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
  (declare (type base-tensor tensor))
  (destructuring-bind (cls &optional subscripts) (ensure-list subs)
    (and (typep tensor cls)
	 (if subscripts
	     (let-typed ((rank (order tensor) :type index-type)
			 (dims (dimensions tensor) :type index-store-vector))
			(very-quickly
			  (loop :for val :in subscripts
			     :for i :of-type index-type := 0 :then (1+ i)
			     :do (unless (or (eq val '*) (eq val (aref dims i)))
				   (return nil))
			     :finally (return (when (= (1+ i) rank) t)))))
	     t))))

(definline tensor-matrixp (ten)
  (declare (type base-tensor ten))
  (= (order ten) 2))

(definline tensor-vectorp (ten)
  (declare (type base-tensor ten))
  (= (order ten) 1))

(deftype base-square-matrix ()
  `(and base-tensor (satisfies tensor-square-matrixp)))

(deftype base-matrix ()
  `(and base-tensor (satisfies tensor-matrixp)))

(deftype base-vector ()
  `(and base-tensor (satisfies tensor-vectorp)))

(definline tensor-squarep (tensor)
  (declare (type base-tensor tensor))
  (let-typed ((dims (dimensions tensor) :type index-store-vector))
	     (loop :for i :from 1 :below (length dims)
		:do (unless (= (aref dims i) (aref dims 0))
		      (return nil))
		:finally (return t))))
;;
(defun tensor-append (axis tensor &rest more-tensors)
  (if (>= axis (order tensor))
      (apply #'tensor-append axis (mapcar #'(lambda (x) (suptensor~ x (1+ axis))) (cons tensor more-tensors)))
      (let ((dims (copy-seq (dimensions tensor))))
	(iter (for ele in more-tensors) (incf (aref dims axis) (aref (dimensions ele) axis)))
	(let* ((ret (zeros dims (class-of tensor)))
	       (view (slice~ ret axis 0 t)))
	  (iter (for ele in (cons tensor more-tensors))
		(with head = 0)
		(setf (slot-value view 'head) head
		      (aref (dimensions view) axis) (aref (dimensions ele) axis))
		(copy! ele view)
		(incf head (* (aref (strides ret) axis) (aref (dimensions ele) axis))))
	  ret))))
