(in-package #:matlisp)

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
		 :for lidx :of-type index-type := (the index-type (* (aref stds 0) (1- (aref dims 0)))) :then (the index-type (+ lidx (the index-type (* (aref stds i) (1- (aref dims i))))))
		 :do (progn
		       (assert (> (aref stds i) 0) nil 'tensor-invalid-stride-value :argument i :stride (aref stds i) :tensor tensor)
		       (assert (> (aref dims i) 0) nil 'tensor-invalid-dimension-value :argument i :dimension (aref dims i) :tensor tensor))
		 :finally (assert (>= (the index-type (store-size tensor)) (the index-type (+ (the index-type (head tensor)) lidx))) nil 'tensor-insufficient-store :store-size (store-size tensor) :max-idx lidx :tensor tensor))))))))

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

(definline tensor-squarep (tensor)
  (declare (type base-tensor tensor))
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
  (declare (type base-tensor tensor)
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
		  (rank (order tensor) :type index-type))
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
  (let ((slst (make-list (order x) :initial-element '(* * *))))
    (rplaca (nthcdr axis slst) (list idx '* (1+ idx)))
    (subtensor~ x slst nil)))

(definline row-slice~ (x idx)
  (slice~ x 0 idx))

(definline col-slice~ (x idx)
  (slice~ x 1 idx))
