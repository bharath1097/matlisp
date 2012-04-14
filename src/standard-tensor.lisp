(defpackage :tensor
  (:use :cl :utilities))

(in-package :tensor)

;;
(declaim (inline allocate-integer4-store))

(eval-when (load eval compile)
  (deftype int32-type ()
    '(signed-byte 32))
  (deftype int32-array (size)
    `(simple-array int32-type (,size)))

  ;;
  (deftype index-type ()
    'fixnum)
  (deftype index-array (size)
    `(simple-array index-type (,size)))
  )

(defun allocate-int32-store (size &optional (initial-element 0))
  "(ALLOCATE-INTEGER-STORE SIZE [INITIAL-ELEMENT]).  Allocates
integer storage.  Default INITIAL-ELEMENT = 0."
  (make-array size
	      :element-type 'int32-type
	      :initial-element initial-element))

(defun allocate-index-store (size &optional (initial-element 0))
  (make-array size :element-type 'index-type
	      :initial-element initial-element))

(defun make-index-store (&rest contents)
  (let ((size (length contents)))
    (make-array size :element-type 'index-type
		:initial-contents contents)))

;;
(defun store-indexing-internal (idx strides &optional (hd (the index-type 0)))
  "No explicit error checking, meant to be used internally.
   Returns

     length(strides)
       __
  hd + \   stride  * idx
       /_        i      i
      i = 0

  "
  (declare (type index-type hd)
	   (type (index-array *) idx strides))
  (let ((rank (length strides)))
    (declare (type index-type rank))
    (the index-type (+ hd
		       (do ((i 0 (+ i 1))
			    (sto-idx (the index-type 0) (the index-type
							  (+ sto-idx
							     (the index-type
							       (* (the index-type
								    (aref idx i))
								  (the index-type
								    (aref strides i))))))))
			   ((= i rank) sto-idx))))))


(defun store-indexing-vec (idx strides dims &optional (hd (the index-type 0)))
  "
   Returns

     length(strides)
       __
  hd + \   stride  * idx
       /_        i      i
      i = 0

  "
  (declare (type index-type hd)
	   (type (index-array *) idx strides))
  (let ((rank (length strides)))
    (declare (type index-type rank))
    (unless (= rank (length idx))
      (error "Wrong number of subscripts for a array of rank ~A" rank))
    (the index-type (+ hd
		       (do ((i 0 (+ i 1))
			    (sto-idx (the index-type 0) (the index-type
							  (+ sto-idx
							     (the index-type
							       (* (the index-type
								    (aref strides i))
								  ;;
								  (the index-type
								    (let ((cidx (aref idx i)))
								      (unless (< -1 cidx (aref dims i))
									(error "Requested index ~A for argument ~A is out of bounds.
Tensor only has dimension ~A for the ~A argument." cidx i (aref dims i) i))
								      cidx))))))))
			   ((= i rank) sto-idx))))))


(defun store-indexing-lst (idx strides dims &optional (hd (the index-type 0)))
  "
   Returns

     length(strides)
       __
  hd + \   stride  * idx
       /_        i      i
      i = 0

  "  
  (declare (type index-type hd)
	   (type (index-array *) strides)
	   (type cons idx))
  (let ((rank (length strides)))
    (declare (type index-type rank))
    (the index-type (+ hd
		       (let ((idx-sum (the index-type 0)))
			 (do ((i 0 (+ i 1))
			      (ilst idx (cdr ilst)))
			     ((= i rank) (if (null ilst)
					     idx-sum
					     (error "Too many subscripts for a tensor of rank ~A" rank)))
			   (let ((cidx (car ilst)))
			     (when (null cidx)
			       (error "Too few subscripts for a tensor of rank ~A" rank))
			     (unless (< -1 cidx (aref dims i))
			       (error "Requested index ~A for argument ~A is out of bounds.
Tensor only has dimension ~A for the ~A argument." cidx i (aref dims i) i))
			     ;;
			     (setf idx-sum (the index-type (+ idx-sum
							      (the index-type
								(*
								 (the index-type cidx)
								 (the index-type (aref strides i))))))))))))))

;;
(defclass standard-tensor ()
  ((rank
    :accessor rank
    :type index-type
    :documentation "Rank of the tensor: number of arguments for the tensor")
   (dimensions
    :accessor dimensions
    :initarg :dimensions
    :type (index-array *)
    :documentation "Dimensions of the vector spaces in which the tensor's arguments reside.")
   (number-of-elements
    :accessor number-of-elements
    :type index-type
    :documentation "Total number of elements in the tensor.")
   ;;
   (head
    :initarg :head
    :initform 0
    :accessor head
    :type index-type
    :documentation "Head for the store's accessor.")
   (strides
    :initarg :strides
    :accessor strides
    :type (index-array *)
    :documentation "Strides for accesing elements of the tensor.")
   (store-size
    :accessor store-size
    :type index-type
    :documentation "Size of the store.")
   (store
    :initarg :store
    :accessor store
    :documentation "The actual storage for the tensor."))
  (:documentation "Basic tensor class."))

;;
(defun store-indexing (idx tensor)
  (declare (type standard-tensor tensor)
	   (type (or cons (index-array *)) idx))  
  (typecase idx
    (cons (store-indexing-lst idx (strides tensor) (dimensions tensor) (head tensor)))
    (vector (store-indexing-lst idx (strides tensor) (dimensions tensor) (head tensor)))))

;;
(defmethod initialize-instance :after ((tensor standard-tensor) &rest initargs)
  (declare (ignore initargs))
  (mlet*
   (((dims hd ss) (slot-values tensor '(dimensions head store-size))
     :type ((index-array *) index-type index-type))
    (rank (length dims) :type index-type))
   ;;Let the object be consistent.
   (setf (rank tensor) rank)
   ;;Row-ordered by default.
   (unless (and (slot-boundp tensor 'strides)
		(= (length (strides tensor)) rank))
     (mlet* ((stds (allocate-index-store rank)
		   :type (index-array *)))
	    (setf (strides tensor) stds)
	    (do ((i (1- rank) (1- i))
		 (st 1 (* st (aref dims i))))
		((< i 0))
	      (setf (aref stds i) st))))
   ;;
   (mlet* ((stds (strides tensor) :type (index-array *))
	   (L-idx (store-indexing-vec (map `(index-array *) #'1- dims) stds dims hd) :type index-type))
	  ;;Error checking is good if we use foreign-pointers as store types.
	  (cond
	    ((< hd 0) (error "Head of the store must be >= 0. Initialized with ~A." hd))
	    ((<= ss L-idx) (error "Store is not large enough to hold the matrix.
Initialized with ~A, but the largest possible index is ~A." ss L-idx)))
	  ;;
	  (dotimes (i rank)
	    (let ((ns (aref dims i))
		  (st (aref stds i)))
	      (cond
		((<= ns 0) (error "Dimension ~A must be > 0.
Initialized with ~A." i ns))
		((< st 0) (error "Stride of dimension ~A must be >= 0.
Initialized with ~A." i st))))))
   ;;
   (setf (number-of-elements tensor) (reduce #'* dims))))

;;
(defgeneric tensor-store-ref (tensor store-idx)
  (:documentation "
  Syntax
  ======
  (tensor-ref-1d store store-idx)

  Purpose
  =======
  Return the element store-idx of the tensor store."))

(defmethod tensor-store-ref :before ((tensor standard-tensor) (idx fixnum))
  (unless (< -1 idx (store-size tensor))
    (error "Requested index ~A is out of bounds.
Tensor-store only has ~A elements." idx (store-size tensor))))

;;
(defgeneric (setf tensor-store-ref) (value matrix idx))

(defmethod (setf tensor-store-ref) :before ((value t) (tensor standard-tensor) (idx fixnum))
  (unless (< -1 idx (store-size tensor))
    (error "Requested index ~A is out of bounds.
Tensor-store only has ~A elements." idx (store-size tensor))))

;;
(defgeneric tensor-ref (tensor &rest subscripts)
  (:documentation "
  Syntax
  ======
  (tensor-ref store &rest subscripts)

  Purpose
  =======
  Return the element:

     (rank - 1)
       __
  hd + \   stride  * sub
       /_        i      i
      i = 0

  of the store "))

(defmethod tensor-ref ((tensor standard-tensor) &rest subscripts)
  (let ((sto-idx (store-indexing subscripts tensor)))
    (tensor-store-ref tensor sto-idx)))

;;
(defgeneric (setf tensor-ref) (value tensor &rest subscripts))

(defmethod (setf tensor-ref) ((value t) (tensor standard-tensor) &rest subscripts)
  (let ((sto-idx (store-indexing subscripts tensor)))
    (setf (tensor-store-ref tensor sto-idx) value)))

;;
;; TODO: Pretty-ify by borrowing from src/print.lisp
(defmethod print-object ((tensor standard-tensor) stream)
  (let ((rank (rank tensor))
	(dims (dimensions tensor)))
    (labels ((rec-print (tensor idx subs)
	       (if (> idx 1)
		   (dotimes (i (aref dims idx))
		     (rec-print tensor (1- idx) (cons i subs)))
		   (progn
		     (format stream "~A~%" (append (list '\: '\:) subs))
		     (dotimes (i (aref dims 0))
		       (dotimes (j (aref dims 1))
			 (format stream "~A~,4T" (apply #'tensor-ref (cons tensor (append (list i j) subs)))))
		     (format stream "~%~%"))))))
      (format stream "<TENSOR(~A) ~A>~%" rank dims)
      (if (= rank 1)
	  (progn
	    (dotimes (i (aref dims 0))
	      (format stream "~A~,4T" (tensor-ref tensor i)))
	    (format stream "~%"))
	  (rec-print tensor (- rank 1) nil)))))
;;
(defun tensor-type-p (tensor &rest subscripts)
  "
  Syntax
  ======
  (tensor-ref tensor &rest subscripts)

  Purpose
  =======
  Check if the given tensor is of particular sizes in particular
  arguments.

  Checking if the tensor is a vector would then be:
  (tensor-type-p ten t)

  Checking if it is a matrix with 2 columns would be:
  (tensor-type-p ten t 2)
  "
  (declare (type standard-tensor tensor))
  (mlet* (((rank dims) (slot-values tensor '(rank dimensions))
	   :type (index-type (index-array *))))
	 (let ((syms->val (make-hash-table)))
	   (labels ((parse-sub (lst i)
		      (let ((val (car lst)))
			(cond
			  ((= i rank) t)
			  ((null val) nil)
			  ((eq val t) (parse-sub (cdr lst) (1+ i)))
			  (t (progn
			       (when (symbolp val)
				 (multiple-value-bind (hash-val existp) (gethash val syms->val)
				   (if existp
				       (setq val hash-val)
				       (setf (gethash val syms->val) (aref dims i)
					     val (aref dims i)))))
			       (if (= val (aref dims i))
				   (parse-sub (cdr lst) (1+ i))
				   nil)))))))
	     (parse-sub subscripts 0)))))

(defun vector-p (tensor)
  (declare (type standard-tensor tensor))
  (tensor-type-p tensor t))

(defun matrix-p (tensor)
  (declare (type standard-tensor tensor))
  (tensor-type-p tensor t t))

(defun square-p (tensor)
  (let* ((rank (rank tensor))
	 (sym (gensym))
	 (lst (make-list rank :initial-element sym)))
    (apply #'tensor-type-p (cons tensor lst))))