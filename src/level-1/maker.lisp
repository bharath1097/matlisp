(in-package #:matlisp)

(deft/generic (t/zeros #'subtypep) sym (dims &optional initial-element))
(deft/method t/zeros (class standard-tensor) (dims &optional initial-element)
  (with-gensyms (astrs adims sizs)
    `(let* ((,adims (make-index-store ,dims)))
       (multiple-value-bind (,astrs ,sizs) (make-stride ,adims)
	 (make-instance ',class
			:dimensions ,adims			  
			:head 0
			:strides ,astrs
			:store (t/store-allocator ,class ,sizs ,@(when initial-element `(,initial-element))))))))

(defgeneric zeros-generic (dims dtype)
  (:documentation "Create a tensor with dimensions @arg{dims} of class @arg{dtype}.")
  (:method ((dims cons) (dtype t))
    (assert (member dtype *tensor-type-leaves*) nil 'tensor-abstract-class :tensor-class dtype)
    (compile-and-eval
     `(defmethod zeros-generic ((dims cons) (dtype (eql ',dtype)))
	(t/zeros ,dtype dims)))
    (zeros-generic dims dtype)))

(definline zeros (dims &optional (type 'real-tensor))
  (let ((*check-after-initializing?* nil))
    (etypecase dims
      (vector
       (zeros-generic (lvec->list dims) type))
      (cons
       (zeros-generic dims type))
      (fixnum
       (zeros-generic (list dims) type)))))
