(in-package #:matlisp)

(defclass foreign-numeric-tensor (blas-numeric-tensor) ())

(deft/method t/store-allocator (sym foreign-numeric-tensor) (size &optional initial-element)
  (error "cannot allocate store for ~a" sym))

(deft/method t/store-type (sym foreign-numeric-tensor) (&optional size)
  'foreign-vector)
(deft/method t/store-size (sym foreign-numeric-tensor) (vec)
  `(fv-size ,vec))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (definline cl->cffi-type (type)
    (ecase type
      (character :char)
      (single-float :float)
      (double-float :double)
      (string :string)
      (t (error 'unknown-token :token type
		:message "Don't know how to convert type to CFFI.")))))

(deft/method t/store-ref (sym foreign-numeric-tensor) (store &rest idx)
   (assert (null (cdr idx)) nil "given more than one index for linear-store")
   (with-gensyms (sto idx0)
     `(let ((,idx0 ,(car idx))
	    (,sto ,store))
	(declare (type index-type ,idx0)
		 (type foreign-vector ,sto))
	(assert (eql (fv-type ,sto) ',(cl->cffi-type (store-element-type sym))) nil 'invalid-type)
	(assert (< -1 ,idx0 (fv-size ,sto)) nil 'out-of-bounds-error :requested ,idx0 :bound (fv-size ,sto))
	(the ,(store-element-type sym) (cffi:mem-aref (fv-pointer ,sto) ',(cl->cffi-type (store-element-type sym)) ,idx0)))))

(deft/method t/store-set (sym foreign-numeric-tensor) (value store &rest idx)
   (assert (null (cdr idx)) nil "given more than one index for linear-store")
   (with-gensyms (sto idx0)
     `(let ((,idx0 ,(car idx))
	    (,sto ,store))
	(declare (type index-type ,idx0)
		 (type foreign-vector ,sto))
	(assert (eql (fv-type ,sto) ',(cl->cffi-type (store-element-type sym))) nil 'invalid-type)
	(assert (< -1 ,idx0 (fv-size ,sto)) nil 'out-of-bounds-error :requested ,idx0 :bound (fv-size ,sto))
	(setf (cffi:mem-aref (fv-pointer ,sto) ',(cl->cffi-type (store-element-type sym)) ,idx0)
	      (the ,(store-element-type sym) ,value)))))

;;
(deft/method t/zeros (class foreign-numeric-tensor) (dims &optional data-pointer)
  (using-gensyms (decl (dims data-pointer) (idims size strd idx sto ptr))
    `(let (,@decl)
       (let ((,idims (make-index-store ,dims)))
	 (multiple-value-bind (,strd ,size) (make-stride ,idims)
	   (let ((,sto (etypecase ,data-pointer
			 (cffi:foreign-pointer
			  (assert (not (cffi:null-pointer-p ,data-pointer)) nil 'invalid-arguments "NULL pointer given for data store.")
			  (make-foreign-vector :pointer ,data-pointer :size ,size :type ',(cl->cffi-type (store-element-type class))))
			 (matlisp-ffi:foreign-vector
			  (assert (<= ,size (fv-size ,data-pointer)) nil 'tensor-insufficient-store :store-size (fv-size ,data-pointer) :max-idx (1- ,size))
			  ,data-pointer))))
	     (let ((,ptr (fv-pointer ,sto)))
	       (iter (for ,idx from 0 below (* ,(cffi:foreign-type-size (cl->cffi-type (store-element-type class))) ,size))
		     (setf (cffi:mem-aref ,ptr :uint8 ,idx) 0)))
	     (with-no-init-checks
		 (make-instance ',class
				:dimensions ,idims
				:strides ,strd
				:store ,sto))))))))

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
(defclass foreign-real-numeric-tensor (foreign-numeric-tensor real-blas-tensor) ())
(defleaf foreign-real-tensor (foreign-real-numeric-tensor) ())
(deft/method t/field-type (sym foreign-real-tensor) ()
  'double-float)

#+nil
(with-field-element foreign-real-tensor (fv 0d0 10)
  (let ((tens (zeros '(2 2) 'foreign-real-tensor fv)))
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
