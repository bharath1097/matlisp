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
			:store (t/store-allocator ,class ,sizs ,@(when initial-element `(,initial-element))))))))

(deft/method t/zeros (class coordinate-sparse-tensor) (dims &optional initial-element)
  (with-gensyms (astrs adims sizs)
    `(let* ((,adims (make-index-store ,dims)))
       (declare (type index-store-vector ,adims))
       (multiple-value-bind (,astrs ,sizs) (make-stride ,adims)
	 (declare (type index-store-vector ,astrs))
	 (make-instance ',class
			:dimensions ,adims
			:strides ,astrs
			:store (t/store-allocator ,class ,sizs))))))

;;
(defgeneric zeros-generic (dims dtype)
  (:documentation "Create a tensor with dimensions @arg{dims} of class @arg{dtype}.")
  (:method ((dims cons) (dtype t))
    ;; (assert (member dtype *tensor-type-leaves*) nil 'tensor-abstract-class :tensor-class dtype)
    (compile-and-eval
     `(defmethod zeros-generic ((dims cons) (dtype (eql ',dtype)))
	(t/zeros ,dtype dims)))
    (zeros-generic dims dtype)))

(definline zeros (dims &optional (type *default-tensor-type*))
  (let ((*check-after-initializing?* nil))
    (let ((type (etypecase type (standard-class (class-name type)) (symbol type))))
      (etypecase dims
	(vector
	 (zeros-generic (lvec->list dims) type))
	(cons
	 (zeros-generic dims type))
	(fixnum
	 (zeros-generic (list dims) type))))))
