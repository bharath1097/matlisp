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
(defmethod subtensor~ ((tensor standard-tensor) (subscripts list) &optional (preserve-rank nil) (ref-single-element? t))
  (iter (for (start end . inc) in subscripts)
	(for d in-vector (dimensions tensor))
	(for s in-vector (strides tensor))
	(with hd = (head tensor))
	(with empty? = nil)
	(let ((start (or (and start (if (>= start 0) start (mod start d))) 0))
	      (end (or (and end (if (>= end 0) end (mod end d))) d))
	      (inc (or inc 1)))
	  (declare (type index-type start end inc))
	  (incf hd (* start s))
	  (let ((nd (ceiling (- end start) inc)))
	    (when (< inc 0) (setq nd (- nd)))
	    (when (< nd 0) (setq empty? t))
	    (when (or preserve-rank (> nd 1))
	      (collect nd into dims)
	      (collect (* inc s) into stds))))
	(finally (return
		   (let ((*check-after-initializing?*))
		     (print empty?)
		     (if (and ref-single-element? (null dims)) (store-ref tensor hd)
			 (make-instance (class-of tensor)
					:head hd
					:dimensions (or (and dims (make-index-store dims)) (make-index-store (list 1)))
					:strides (or (and stds (make-index-store stds)) (make-index-store (list 1)))
					:store (store tensor)
					:parent-tensor tensor)))))))

(defun tensor-append (axis tensor &rest more-tensors)
  (let ((dims (copy-seq (dimensions tensor))))
    (loop :for ele :in more-tensors
       :do (incf (aref dims axis) (aref (dimensions ele) axis)))
    (let* ((ret (zeros dims))
	   (view (slice~ ret axis 0 t)))
      (loop :for ele :in (cons tensor more-tensors)
	 :and head := 0 :then (+ head (* (aref (strides ret) axis) (aref (dimensions ele) axis)))
	 :do (progn
	       (setf (slot-value view 'head) head
		     (aref (dimensions view) axis) (aref (dimensions ele) axis))
	       (copy! ele view)))
      ret)))
