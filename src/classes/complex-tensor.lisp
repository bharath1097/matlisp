(in-package #:matlisp)

(defmacro-method field-type ((sym (eql 'complex-tensor)))
  '(complex double-float))

(defmacro-method store-element-type ((sym (eql 'complex-tensor)))
  'double-float)

(defmacro-method compute-store-size ((sym (eql 'complex-tensor)) size)
  '(* 2 size))

(defmacro-method store-ref ((sym (eql 'complex-tensor)) store idx)
  (let ((store-s (gensym))
	(idx-s (gensym))
	(type (macroexpand-1 `(store-element-type ,sym))))
    `(let ((,store-s ,store)
	   (,idx-s ,idx)) 
       (declare (type (simple-array ,type) ,store-s))
       (complex (aref ,store-s (* 2 ,idx-s)) (aref ,store-s (1+ (* 2 ,idx-s)))))))

(defmacro-method store-set ((sym (eql 'complex-tensor)) value store idx)
  (let ((store-s (gensym))
	(idx-s (gensym))
	(value-s (gensym))
	(type (macroexpand-1 `(store-element-type ,sym)))
	(ftype (macroexpand-1 `(field-type ,sym))))
    `(let ((,store-s ,store)
	   (,idx-s ,idx)
	   (,value-s ,value))
       (declare (type (simple-array ,type) ,store-s)
		(type ,ftype ,value-s))
       (setf (aref ,store-s (* 2 ,idx-s)) (cl:realpart ,value-s)
	     (aref ,store-s (1+ (* 2 ,idx-s))) (cl:imagpart ,value-s))
       nil)))
;;
(defmethod print-element ((tensor complex-tensor)
			  element stream)
  (let ((realpart (realpart element))
	(imagpart (imagpart element)))
    (format stream (if (zerop imagpart)
		       "~11,5,,,,,'Eg"
		       "#C(~11,4,,,,,'Ee ~11,4,,,,,'Ee)")
	    realpart imagpart)))

