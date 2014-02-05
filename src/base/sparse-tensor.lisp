(in-package :matlisp)

;;One may to do better than a Hash-table for this.
(defparameter *default-sparsity* 1/1000)
(defparameter *max-size* 10000)

(defclass coordinate-sparse-tensor (sparse-tensor)
  ((strides :initarg :strides :reader strides :type index-store-vector
	    :documentation "Strides for accesing elements of the tensor.")))

(deft/generic (t/sparse-fill #'subtypep) sym ())
(deft/method t/sparse-fill (sym sparse-tensor) ()
 `(t/fid+ (t/field-type ,sym)))

(deft/method t/store-allocator (sym coordinate-sparse-tensor) (size &optional initial-element)
  (with-gensyms (size-sym)
    `(let ((,size-sym (t/compute-store-size ,sym ,size)))
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

(deft/method t/compute-store-size (sym coordinate-sparse-tensor) (size)
 `(max (min sb-impl::+min-hash-table-size+ (ceiling (/ ,size *default-sparsity*))) *max-sparse-size*))

(defmethod head ((tensor coordinate-sparse-tensor))
  0)
;;
(defleaf real-sparse-tensor (coordinate-sparse-tensor) ())

(deft/method t/field-type (sym real-sparse-tensor) ()
  'double-float)
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
(defclass compressed-sparse-matrix (sparse-tensor)
  ((index-position :initarg :strides :reader index-position :type index-store-vector
		   :documentation "Strides for accesing elements of the tensor.")
   (indices :initarg :strides :reader indices :type index-store-vector
	    :documentation "Strides for accesing elements of the tensor.")))

(defclass ccs-matrix (compressed-sparse-matrix) ())
(defclass crs-matrix (compressed-sparse-matrix) ())


(deft/method t/store-allocator (sym compressed-sparse-matrix) (size &optional initial-element)
  (using-gensyms (decl (size))
    `(let (,@decl)
       (destructuring-bind (ni nz) (t/compute-store-size ,sym ,size)
	 (list
	  (allocate-index-store ni)
	  (allocate-index-store nz)
	  (make-array nz :element-type ,(store-element-type sym) :initial-element ,(if (subtypep type 'number) `(t/fid+ ,type) nil)))))))

(deft/method t/compute-store-size (sym ccs-matrix) (size)
  `(destructuring-bind (nr nc &optional nz) size
     (unless nz (setq nz (min (ceiling (* nr nc *default-sparsity*)) *max-sparse-size*)))
     (list nr nz nil)))

(deft/method t/compute-store-size (sym ccs-matrix) (size)
  (using-gensyms (decl (size))
    `(let (,@decl)
       (destructuring-bind (nr nc &optional nz) size
	 (unless nz (setq nz (min (ceiling (* nr nc *default-sparsity*)) *max-sparse-size*)))
	 (list nc nz nil)))))

(t/compute-store-size ccs-matrix '(10 10))
(deft/method t/compute-store-size (sym ccr-matrix) (size)
  `(append ,size t))


(deft/method t/compute-store-size (sym ccs-matrix) (size)
  `(max (min sb-impl::+min-hash-table-size+ (ceiling (/ ,size *default-sparsity*))) *max-size*))



(defun coordinate->ccs (tensor)
  (assert (eql (nth-value 2 (blas-matrix-compatiblep tensor #\n)) :col-major) nil "nooo!")
  (labels ((rref (idx)
	     (multiple-value-list (floor idx (col-stride tensor))))
	   (convert-sto ()
	     (let ((sto (store tensor))
		   (nsto (make-hash-table)))
	       (maphash #'(lambda (k v)			    
			    (destructuring-bind (r c) (rref k)
			      (unless (nth-value 1 (gethash c nsto))
				(setf (gethash c nsto) (cons nil nil)))
			      (push r (car (gethash c nsto)))
			      (push v (cdr (gethash c nsto)))))
			sto)
	       (maphash #'(lambda (k v)
			    (setf (car v) (make-index-store (car v))
				  (cdr v) (make-array (length (cdr v)) :initial-contents (cdr v)))) nsto)
	       nsto)))
    (convert-sto)))

(defclass ccs-sparse-matrix (sparse-tensor) ())
