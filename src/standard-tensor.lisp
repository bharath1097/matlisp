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

(make-array-allocator allocate-int32-store 'int32-type 0
"(allocate-int32-store size [initial-element])
Allocates integer-32 storage.  Default initial-element = 0.")

(make-array-allocator allocate-index-store 'index-type 0
"(allocate-index-store size [initial-element])
Allocates index storage.  Default initial-element = 0.")


(defun make-index-store (contents)
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
  (declare (optimize (safety 0) (speed 3))
	   (type index-type hd)
	   (type (index-array *) idx strides))
  (let ((rank (length strides)))
    (declare (type index-type rank))
    (the index-type 
      (do ((i 0 (+ i 1))
	   (sto-idx (the index-type hd) (the index-type
					  (+ sto-idx
					     (the index-type
					       (* (the index-type
						    (aref idx i))
						  (the index-type
						    (aref strides i))))))))
	  ((= i rank) sto-idx)))))

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
    (if (not (= rank (length idx)))
	(error 'tensor-index-rank-mismatch :index-rank (length idx) :rank rank)
	(the index-type
	  (do ((i 0 (+ i 1))
	       (sto-idx (the index-type hd)
			(the index-type
			  (+ sto-idx
			     (the index-type
			       (* (the index-type
				    (aref strides i))
				  ;;
				  (the index-type
				    (let ((cidx (aref idx i)))
				      (declare (type index-type cidx))
				      (if (< -1 cidx (aref dims i))
					  cidx
					  (error 'tensor-index-out-of-bounds :argument i :index cidx :dimension (aref dims i)))))))))))
	      ((= i rank) sto-idx))))))

(defun store-indexing-lst (idx strides dims &optional (hd (the index-type 0)))
  "
   Returns

     length(strides)
       __
  hd + \   stride  * idx
       /_        i      i
      i = 0

  idx here is a list.
  "  
  (declare (type index-type hd)
	   (type (index-array *) strides)
	   (type cons idx))
  (let ((rank (length strides)))
    (declare (type index-type rank))
    (labels ((rec-sum (sum i lst)
	       (cond
		 ((and (null lst) (= i rank)) (the index-type sum))
		 ((or (null lst) (= i rank)) (error 'tensor-index-rank-mismatch :index-rank (length idx) :rank rank))
		 (t
		  (let ((cidx (car lst)))
		    (declare (type index-type cidx))
		    (rec-sum (the index-type (+ sum
						(* (aref strides i)
						   (if (< -1 cidx (aref dims i))
						       cidx
						       (error 'tensor-index-out-of-bounds :argument i :index cidx :dimension (aref dims i))))))
			      (+ i 1) (cdr lst)))))))
      (rec-sum (the index-type hd) 0 idx))))

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

(defclass sub-tensor (standard-tensor)
  ((parent-tensor
    :initarg :parent-tensor
    :accessor parent-tensor))
  (:documentation "Basic sub-tensor class."))

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
	    ((< hd 0) (error 'tensor-invalid-head-value :head hd :tensor tensor))
	    ((<= ss L-idx) (error 'insufficient-store :store-size ss :max-idx L-idx :tensor tensor)))
	  ;;
	  ;;--*TODO: Add checks to see if there is index-collision.*--
	  (dotimes (i rank)
	    (let ((ns (aref dims i))
		  (st (aref stds i)))
	      (cond
		((<= ns 0) (error 'tensor-invalid-dimension-value :argument i :dimension ns :tensor tensor))
		((< st 0) (error 'tensor-invalid-stride-value :argument i :stride st :tensor tensor))))))
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
    (error 'store-index-out-of-bounds :index idx :store-size (store-size tensor) :tensor tensor)))

;;
(defgeneric (setf tensor-store-ref) (value matrix idx))

(defmethod (setf tensor-store-ref) :before ((value t) (tensor standard-tensor) (idx fixnum))
  (unless (< -1 idx (store-size tensor))
    (error 'store-index-out-of-bounds :index idx :store-size (store-size tensor) :tensor tensor)))

;;
(defgeneric tensor-ref (tensor subscripts)
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

(defmethod tensor-ref ((tensor standard-tensor) subscripts)
  (let ((sto-idx (store-indexing subscripts tensor)))
    (tensor-store-ref tensor sto-idx)))

;;
(defgeneric (setf tensor-ref) (value tensor subscripts))

(defmethod (setf tensor-ref) ((value t) (tensor standard-tensor) subscripts)
  (let ((sto-idx (store-indexing subscripts tensor)))
    (setf (tensor-store-ref tensor sto-idx) value)))

;;
;; TODO: Pretty-ify by borrowing from src/print.lisp
(defmethod print-object ((tensor standard-tensor) stream)
  (print-unreadable-object (tensor stream :type t)    
    (let ((rank (rank tensor))
	  (dims (dimensions tensor)))
      (labels ((two-print (tensor subs)
		 (dotimes (i (aref dims 0))
		   (dotimes (j (aref dims 1))
		     (format stream "~A~,4T" (apply #'tensor-ref (cons tensor (append (list i j) subs)))))
		   (format stream "~%")))
	       (rec-print (tensor idx subs)
		 (if (> idx 1)
		     (dotimes (i (aref dims idx))
		       (rec-print tensor (1- idx) (cons i subs)))
		     (progn
		       (format stream "~A~%" (append (list '\: '\:) subs))
		       (two-print tensor subs)
		       (format stream "~%")))))
	(format stream "~A ~A~%" rank dims)
	(case rank
	  (1
	   (dotimes (i (aref dims 0))
	     (format stream "~A~,4T" (tensor-ref tensor i)))
	   (format stream "~%"))
	  (2
	   (two-print tensor nil))
	  (t
	   (rec-print tensor (- rank 1) nil)))))))

;;
(defun tensor-type-p (tensor subscripts)
  "
  Syntax
  ======
  (tensor-ref tensor subscripts)

  Purpose
  =======
  Check if the given tensor is of particular sizes in particular
  arguments.

  Checking if the tensor is a vector would then be:
  (tensor-type-p ten '(t))

  Checking if it is a matrix with 2 columns would be:
  (tensor-type-p ten '(t 2))
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
  (tensor-type-p tensor '(t)))

(defun matrix-p (tensor)
  (declare (type standard-tensor tensor))
  (tensor-type-p tensor '(t t)))

(defun square-p (tensor)
  (let* ((rank (rank tensor))
	 (sym (gensym))
	 (lst (make-list rank :initial-element sym)))
    (apply #'tensor-type-p (list tensor lst))))