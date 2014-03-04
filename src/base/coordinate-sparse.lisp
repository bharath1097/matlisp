(in-package :matlisp)

;;One may to do better than a Hash-table for this.
(defparameter *default-sparsity* 1/1000)
(defparameter *max-sparse-size* 10000)

(defclass coordinate-sparse-tensor (sparse-tensor)
  ((strides :initarg :strides :reader strides :type index-store-vector
	    :documentation "Strides for accesing elements of the tensor.")))

(deft/generic (t/sparse-fill #'subtypep) sym ())
(deft/method t/sparse-fill (sym sparse-tensor) ()
 `(t/fid+ (t/field-type ,sym)))

(deft/method t/store-allocator (sym coordinate-sparse-tensor) (size &optional nz)
  (with-gensyms (size-sym)
    `(let ((,size-sym (or ,nz (min (max sb-impl::+min-hash-table-size+ (ceiling (/ ,size *default-sparsity*))) *max-sparse-size*))))
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

(defmethod head ((tensor coordinate-sparse-tensor))
  0)
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
