(in-package #:matlisp)

(defclass numeric-tensor (standard-tensor) ())
(deft/method t/field-type (sym numeric-tensor) ()
  'number)

;;
(defleaf integer-tensor (numeric-tensor) ())
(deft/method t/field-type (sym integer-tensor) ()
  'integer)

;;
(defclass blas-numeric-tensor (numeric-tensor) ())
(deft/generic (t/l1-lb #'subtypep) sym ())
(deft/generic (t/l2-lb #'subtypep) sym ())
(deft/generic (t/l3-lb #'subtypep) sym ())

;;
(defclass real-numeric-tensor (blas-numeric-tensor) ())
(deft/method t/field-type (sym real-numeric-tensor) ()
  'real)
(deft/method t/l1-lb (sym real-numeric-tensor) ()
  '*real-l1-fcall-lb*)
(deft/method t/l2-lb (sym real-numeric-tensor) ()
  '*real-l2-fcall-lb*)
(deft/method t/l3-lb (sym real-numeric-tensor) ()
  '*real-l3-fcall-lb*)

(defmethod print-element ((tensor real-numeric-tensor)
			  element stream)
  (format stream "~11,5,,,,,'Eg" element))
;;Real tensor
(defleaf real-tensor (real-numeric-tensor) ())
(deft/method t/field-type (sym real-tensor) ()
  'double-float)

#+nil
(progn
  (defleaf sreal-tensor (real-numeric-tensor) ())
  (deft/method t/field-type (sym sreal-tensor) ()
    'single-float))

;;Complex tensor
(defclass complex-numeric-tensor (blas-numeric-tensor) ())

(deft/method t/field-type (sym complex-numeric-tensor) ()
  `(complex real))

(deft/method t/l1-lb (sym complex-numeric-tensor) ()
  '*complex-l1-fcall-lb*)
(deft/method t/l2-lb (sym complex-numeric-tensor) ()
  '*complex-l2-fcall-lb*)
(deft/method t/l3-lb (sym complex-numeric-tensor) ()
  '*complex-l3-fcall-lb*)

(deft/method t/store-element-type (sym complex-numeric-tensor) ()
  (let ((cplx-type (macroexpand-1 `(t/field-type ,sym))))
    (second cplx-type)))
  
(deft/method t/compute-store-size (sym complex-numeric-tensor) (size)
  `(* 2 ,size))

(deft/method t/store-ref (sym complex-numeric-tensor) (store idx)
  (let ((store-s (gensym))
	(idx-s (gensym))
	(type (macroexpand-1 `(t/store-element-type ,sym))))
    `(let ((,store-s ,store)
	   (,idx-s ,idx)) 
       (declare (type (simple-array ,type) ,store-s))
       (complex (aref ,store-s (* 2 ,idx-s)) (aref ,store-s (1+ (* 2 ,idx-s)))))))

(deft/method t/store-set (sym complex-numeric-tensor) (value store idx)
  (let ((store-s (gensym))
	(idx-s (gensym))
	(value-s (gensym))
	(type (macroexpand-1 `(t/store-element-type ,sym)))
	(ftype (macroexpand-1 `(t/field-type ,sym))))
    `(let ((,store-s ,store)
	   (,idx-s ,idx)
	   (,value-s ,value))
       (declare (type (simple-array ,type) ,store-s)
		(type ,ftype ,value-s))
       (setf (aref ,store-s (* 2 ,idx-s)) (cl:realpart ,value-s)
	     (aref ,store-s (1+ (* 2 ,idx-s))) (cl:imagpart ,value-s))
       nil)))

(defmethod store-size ((tensor complex-numeric-tensor))
  (floor (/ (length (store tensor)) 2)))

(defmethod print-element ((tensor complex-numeric-tensor)
			  element stream)
  (let ((realpart (realpart element))
	(imagpart (imagpart element)))
    (format stream (if (zerop imagpart)
		       "~11,5,,,,,'Eg"
		       "#C(~11,4,,,,,'Ee ~11,4,,,,,'Ee)")
	    realpart imagpart)))
;;
(defleaf complex-tensor (complex-numeric-tensor) ())
(deft/method t/field-type (sym complex-tensor) ()
  '(complex double-float))

#+nil
(progn
  (defleaf scomplex-tensor (complex-numeric-tensor) ())
  (deft/method t/store-element-type (sym scomplex-tensor) ()
    'single-float))

