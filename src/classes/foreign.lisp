(in-package #:matlisp)

(defclass foreign-numeric-tensor (blas-numeric-tensor) ())

(deft/method t/store-allocator (sym foreign-numeric-tensor) (size &optional initial-element)
  (error "cannot allocate store for ~a" sym))

(deft/method t/store-type (sym foreign-numeric-tensor) (&optional size)
  'foreign-vector)
(deft/method t/store-size (sym foreign-numeric-tensor) (vec)
  `(fv-size ,vec))
(deft/method t/store-ref (sym foreign-numeric-tensor) (store idx)
  `(the ,(field-type sym) (fv-ref ,store ,idx)))
(deft/method t/store-set (sym foreign-numeric-tensor) (value store idx)
  `(setf (fv-ref ,store ,idx) (the ,(field-type sym) ,value)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric cl->cffi-type (type)
    (:method (type)
      (ecase type
	(character :char)
	(single-float :float)
	(double-float :double)
	(string :string)
	(t (error 'unknown-token :token type
		  :message "Don't know how to convert type to CFFI."))))))

(deft/method with-field-element (sym foreign-numeric-tensor) (decl &rest body)
  (destructuring-bind (var val &optional (count 1)) decl
    (with-gensyms (idx size point)
      (let ((type (cl->cffi-type (store-element-type sym))))
	`(let ((,size (t/compute-store-size ,sym ,count)))
	   (cffi:with-foreign-object (,point ,type ,size)
	     (let ((,var (make-foreign-vector :pointer ,point :type ,type :size ,size)))
	       ,@(when val
		   ;;No point rushing through this loop.
		   `((loop :for ,idx :from 0 :below ,size
			:do (t/store-set ,sym ,val ,var ,idx))))
	       (locally
		   ,@body))))))))
;;
(defclass foreign-real-numeric-tensor (foreign-numeric-tensor real-numeric-tensor) ())
(deft/method t/field-type (sym foreign-real-numeric-tensor) ()
  'real)

(defleaf foreign-real-tensor (foreign-real-numeric-tensor) ())
(deft/method t/field-type (sym foreign-real-tensor) ()
  'double-float)

(defun make-foreign-real-tensor (dims pointer)
  (let ((dims (make-index-store (etypecase dims
				  (vector (lvec->list dims))
				  (cons dims)
				  (fixnum (list dims))))))
    (make-instance 'foreign-real-tensor
		   :dimensions dims
		   :store pointer
		   :strides (make-stride dims))))

(with-field-element foreign-real-tensor (fv 0d0 10)
  (let ((tens (make-foreign-real-tensor (idxv 2 2) fv)))
    (axpy! 1 nil tens)
    (copy tens 'real-tensor)))

;;
#+nil
(progn
(defclass foreign-complex-numeric-tensor (foreign-numeric-tensor complex-numeric-tensor) ())
(deft/method t/field-type (sym foreign-complex-numeric-tensor) ()
  'complex)

(t/store-type foreign-real-tensor)
)
