(in-package #:matlisp)

(defun subfieldp (a b)
  (subtypep (field-type a) (field-type b)))

(defclass numeric-tensor (standard-tensor) ())
(deft/method t/field-type (sym numeric-tensor) ()
  'number)

;;
(defleaf integer-tensor (numeric-tensor) ())
(deft/method t/field-type (sym integer-tensor) ()
  'integer)

(defleaf fixnum-tensor (numeric-tensor) ())
(deft/method t/field-type (sym fixnum-tensor) ()
  'fixnum)
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

(deft/method t/realified-type (sym real-numeric-tensor) ()
  sym)

(defmethod print-element ((tensor real-numeric-tensor)
			  element stream)
  (format stream "~12,4,0,,,,'Eg" element))

;;Real tensor
(define-constant +real-infinity+
    (matlisp-ffi::with-fortran-float-modes
      (/ 1d0 0d0)))

(define-constant +real-nan+
    (matlisp-ffi::with-fortran-float-modes
      (/ 0d0 0d0)))

(defleaf real-tensor (real-numeric-tensor) ())
(deft/method t/field-type (sym real-tensor) ()
  'double-float)

(deft/method t/complexified-type (sym real-tensor) ()
  'complex-tensor)

(defleaf sreal-tensor (real-numeric-tensor) ())
(deft/method t/field-type (sym sreal-tensor) ()
  'single-float)

;;Complex tensor
(defclass complex-numeric-tensor (blas-numeric-tensor) ())

(deft/method t/field-type (sym complex-numeric-tensor) ()
  `(complex real))

(deft/method t/complexified-type (sym complex-numeric-tensor) ()
  sym)

(deft/method t/l1-lb (sym complex-numeric-tensor) ()
  '*complex-l1-fcall-lb*)
(deft/method t/l2-lb (sym complex-numeric-tensor) ()
  '*complex-l2-fcall-lb*)
(deft/method t/l3-lb (sym complex-numeric-tensor) ()
  '*complex-l3-fcall-lb*)

;;Comment this block if you want to use (simple-array (complex double-float) (*))
;;as the underlying store. This will make Lisp-implementations of gemm .. faster
;;but you'll lose the ability to use tensor-realpart~/imagpart~.
(progn
  (deft/method t/store-element-type (sym complex-numeric-tensor) ()
	       (let ((cplx-type (macroexpand-1 `(t/field-type ,sym))))
		 (second cplx-type)))

  (deft/method t/compute-store-size (sym complex-numeric-tensor) (size)
	       `(* 2 ,size))

  (deft/method t/store-size (sym complex-numeric-tensor) (vec)
	       `(/ (length ,vec) 2))

  (deft/method t/store-ref (sym complex-numeric-tensor) (store &rest idx)
	       (assert (null (cdr idx)) nil "given more than one index for linear-store")
	       (let ((idx (car idx)))
		 (using-gensyms (decl (store idx))
		   `(let (,@decl)
		      (declare (type ,(store-type sym) ,store))
		      (complex (aref ,store (* 2 ,idx)) (aref ,store (1+ (* 2 ,idx))))))))

  (deft/method t/store-set (sym complex-numeric-tensor) (value store &rest idx)
	       (assert (null (cdr idx)) nil "given more than one index for linear-store")
	       (let ((idx (car idx)))
		 (using-gensyms (decl (store idx value))
		   `(let (,@decl)
		      (declare (type ,(store-type sym) ,store)
			       (type ,(field-type sym) ,value))
		      (setf (aref ,store (* 2 ,idx)) (cl:realpart ,value)
			    (aref ,store (1+ (* 2 ,idx))) (cl:imagpart ,value))
		      nil))))

  (defmethod store-size ((tensor complex-numeric-tensor))
    (floor (/ (length (store tensor)) 2))))

(defmethod print-element ((tensor complex-numeric-tensor)
			  element stream)
  (let ((realpart (realpart element))
	(imagpart (imagpart element)))
    (format stream "~6,4,-4,,,,'Eg + ~6,4,-4,,,,'Egi"  realpart imagpart)))
;;
(defleaf complex-tensor (complex-numeric-tensor) ())
(deft/method t/field-type (sym complex-tensor) ()
  '(complex double-float))

(deft/method t/realified-type (sym complex-tensor) ()
  'real-tensor)

(defleaf scomplex-tensor (complex-numeric-tensor) ())
(deft/method t/field-type (sym scomplex-tensor) ()
  '(complex single-float))

(deft/method t/realified-type (sym scomplex-tensor) ()
  'sreal-tensor)
