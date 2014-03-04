(in-package #:matlisp)

(deft/generic (t/zeros #'subtypep) sym (dims &optional initial-element))
(deft/method t/zeros (class standard-tensor) (dims &optional initial-element)
  (with-gensyms (astrs adims sizs)
    `(let* ((,adims (make-index-store ,dims)))
       (declare (type index-store-vector ,adims))
       (multiple-value-bind (,astrs ,sizs) (make-stride ,adims)
	 (declare (type index-store-vector ,astrs))
	 (make-instance ',class
			:dimensions ,adims
			:head 0
			:strides ,astrs
			:store (t/store-allocator ,class ,sizs ,@(when initial-element `((t/coerce ,(field-type class) ,initial-element)))))))))

(deft/method t/zeros (class coordinate-sparse-tensor) (dims &optional nz)
  (with-gensyms (astrs adims sizs)
    `(let* ((,adims (make-index-store ,dims)))
       (declare (type index-store-vector ,adims))
       (multiple-value-bind (,astrs ,sizs) (make-stride-cmj ,adims)
	 (declare (type index-store-vector ,astrs))
	 (make-instance ',class
			:dimensions ,adims
			:strides ,astrs
			:store (t/store-allocator ,class ,sizs ,nz))))))

(deft/method t/zeros (class compressed-sparse-matrix) (dims &optional nz)
  (with-gensyms (dsym)
    `(let ((,dsym ,dims))
       (destructuring-bind (vr vd) (t/store-allocator ,class ,dsym ,nz)
	 (make-instance ',class
			:dimensions (make-index-store ,dims)
			:neighbour-start (allocate-index-store (1+ (second ,dsym)))
			:neighbour-id vr
			:store vd)))))

;;
(defgeneric zeros-generic (dims dtype &optional initial-element)
  (:documentation "
    A generic version of @func{zeros}.
")
  (:method ((dims cons) (dtype t) &optional initial-element)
    ;; (assert (member dtype *tensor-type-leaves*) nil 'tensor-abstract-class :tensor-class dtype)
    (compile-and-eval
     `(defmethod zeros-generic ((dims cons) (dtype (eql ',dtype)) &optional initial-element)
	(if initial-element
	  (t/zeros ,dtype dims initial-element)
	  (t/zeros ,dtype dims))))
    (zeros-generic dims dtype initial-element)))

(definline zeros (dims &optional (type *default-tensor-type*) initial-element)
"
    Create a tensor with dimensions @arg{dims} of class @arg{dtype}.
    The optional argument @arg{initial-element} is used in two completely
    incompatible ways.

    If @arg{dtype} is a dense tensor, then @arg{initial-element}, is used to
    initialize all the elements. If @arg{dtype} is however, a sparse tensor,
    it is used for computing the number of nonzeros slots in the store.

    Example:
    > (zeros 3)
    #<REAL-TENSOR #(3)
      0.0000      0.0000      0.0000     
    >

    > (zeros 3 'complex-tensor 2)
    #<COMPLEX-TENSOR #(3)
      2.0000      2.0000      2.0000     
    >

    > (zeros '(10000 10000) 'real-compressed-sparse-matrix 10000)
    #<REAL-COMPRESSED-SPARSE-MATRIX #(10000 10000), store-size: 10000>
"
  (let ((*check-after-initializing?* nil))
    (let ((type (etypecase type (standard-class (class-name type)) (symbol type))))
      (etypecase dims
	(vector
	 (zeros-generic (lvec->list dims) type initial-element))
	(cons
	 (zeros-generic dims type initial-element))
	(fixnum
	 (zeros-generic (list dims) type initial-element))))))
