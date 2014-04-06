(in-package :matlisp)

;;One may to do better than a Hash-table for this.
(defclass coordinate-sparse-tensor (sparse-tensor)
  ((head :initarg :head :initform 0 :reader head :type index-type
	 :documentation "Head for the store's accessor.")
   (strides :initarg :strides :reader strides :type index-store-vector
	    :documentation "Strides for accesing elements of the tensor.")))

(defmethod initialize-instance :after ((tensor coordinate-sparse-tensor) &rest initargs)
  (declare (ignore initargs))
  (when *check-after-initializing?*
    (let-typed ((dims (dimensions tensor) :type index-store-vector))
      (assert (>= (head tensor) 0) nil 'tensor-invalid-head-value :head (head tensor) :tensor tensor)
      (if (not (slot-boundp tensor 'strides))
	  (setf (slot-value tensor 'strides) (make-stride-cmj dims))
	  (very-quickly
	    (let-typed ((stds (strides tensor) :type index-store-vector))
	      (loop :for i :of-type index-type :from 0 :below (order tensor)
		 :for sz :of-type index-type := (aref dims 0) :then (the index-type (* sz (aref dims i)))
		 :for lidx :of-type index-type := (the index-type (* (aref stds 0) (1- (aref dims 0)))) :then (the index-type (+ lidx (the index-type (* (aref stds i) (1- (aref dims i))))))
		 :do (progn
		       (assert (> (aref stds i) 0) nil 'tensor-invalid-stride-value :argument i :stride (aref stds i) :tensor tensor)
		       (assert (> (aref dims i) 0) nil 'tensor-invalid-dimension-value :argument i :dimension (aref dims i) :tensor tensor))
		 :finally (assert (>= (the index-type (store-size tensor)) (the index-type (+ (the index-type (head tensor)) lidx))) nil 'tensor-insufficient-store :store-size (store-size tensor) :max-idx lidx :tensor tensor))))))))

(deft/generic (t/sparse-fill #'subtypep) sym ())
(deft/method t/sparse-fill (sym sparse-tensor) ()
 `(t/fid+ (t/field-type ,sym)))

(deft/method t/store-allocator (sym coordinate-sparse-tensor) (size &optional nz)
  (with-gensyms (size-sym)
    `(let ((,size-sym (or ,nz (min (max 16 (ceiling (/ ,size *default-sparsity*))) *max-sparse-size*))))
       (make-hash-table :size ,size-sym))))

(deft/method t/store-ref (sym coordinate-sparse-tensor) (store &rest idx)
   (assert (null (cdr idx)) nil "given more than one index for hashtable.")
  `(the ,(field-type sym) (gethash ,(car idx) ,store (t/sparse-fill ,sym))))

(deft/method t/store-set (sym coordinate-sparse-tensor) (value store &rest idx)
   (assert (null (cdr idx)) nil "given more than one index for hashtable.")
   (with-gensyms (val)
     `(let-typed ((,val ,value :type ,(field-type sym)))
	(unless (t/f= ,(field-type sym) ,val (t/fid+ ,(field-type sym)))
	  (setf (gethash ,(car idx) ,store) (the ,(field-type sym) ,value))))))

(deft/method t/store-type (sym coordinate-sparse-tensor) (&optional (size '*))
  'hash-table)

(deft/method t/store-size (sym coordinate-sparse-tensor) (ele)
  `(hash-table-count ,ele))

(deft/method t/store-type (sym coordinate-sparse-tensor) (&optional (size '*))
  'hash-table)
;;
(defmethod ref ((tensor coordinate-sparse-tensor) &rest subscripts)
  (let ((clname (class-name (class-of tensor))))
    (assert (member clname *tensor-type-leaves*) nil 'tensor-abstract-class :tensor-class clname)
    (compile-and-eval
     `(defmethod ref ((tensor ,clname) &rest subscripts)
	(let ((subs (if (numberp (car subscripts)) subscripts (car subscripts))))
	  (t/store-ref ,clname (store tensor) (store-indexing subs tensor)))))
    (apply #'ref (cons tensor subscripts))))

(defmethod (setf ref) (value (tensor coordinate-sparse-tensor) &rest subscripts)
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
(defmethod subtensor~ ((tensor coordinate-sparse-tensor) (subscripts list) &optional (preserve-rank nil) (ref-single-element? t))
  (multiple-value-bind (hd dims stds) (parse-slice-for-strides (dimensions tensor) (strides tensor) subscripts preserve-rank ref-single-element?)
    (incf hd (head tensor))
    (if dims
	(let ((*check-after-initializing?* nil))
	  (make-instance (class-of tensor)
			 :head hd
			 :dimensions (make-index-store dims)
			 :strides (make-index-store stds)
			 :store (store tensor)
			 :parent-tensor tensor))
	(store-ref tensor hd))))
