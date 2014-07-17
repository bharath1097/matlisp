(in-package #:matlisp)

(defun subfieldp (a b)
  (subtypep (field-type a) (field-type b)))

(defclass numeric-tensor (standard-tensor) ())
(deft/method t/field-type (sym numeric-tensor) ()
  'number)
;;
(defclass real-numeric-tensor (numeric-tensor) ())
(deft/method t/field-type (sym real-numeric-tensor) ()
  'real)
(deft/method t/realified-type (sym real-numeric-tensor) ()
  sym)
;;
(defleaf rational-tensor (real-numeric-tensor) ())
(deft/method t/field-type (sym rational-tensor) ()
  'rational)

(defleaf fixnum-tensor (real-numeric-tensor) ())
(deft/method t/field-type (sym fixnum-tensor) ()
  'fixnum)

(defleaf u8-tensor (real-numeric-tensor) ())
(deft/method t/field-type (sym u8-tensor) ()
  '(unsigned-byte 8))

(defleaf boolean-tensor (real-numeric-tensor) ())
(deft/method t/field-type (sym boolean-tensor) ()
  '(mod 2))
;;
(defclass blas-numeric-tensor (numeric-tensor) ())
(deft/generic (t/l1-lb #'subtypep) sym ())
(deft/generic (t/l2-lb #'subtypep) sym ())
(deft/generic (t/l3-lb #'subtypep) sym ())
;;Real tensor
(defclass real-blas-tensor (real-numeric-tensor blas-numeric-tensor) ())
(deft/method t/l1-lb (sym real-blas-tensor) ()
  '*real-l1-fcall-lb*)
(deft/method t/l2-lb (sym real-blas-tensor) ()
  '*real-l2-fcall-lb*)
(deft/method t/l3-lb (sym real-blas-tensor) ()
  '*real-l3-fcall-lb*)

(defmethod print-element ((tensor real-blas-tensor)
			  element stream)
  (format stream "~,4,-2,,,,'Eg" element))

(define-constant +real-infinity+
    (matlisp-ffi::with-fortran-float-modes
      (/ 1d0 0d0)))
(define-constant +real-nan+
    (matlisp-ffi::with-fortran-float-modes
      (/ 0d0 0d0)))
(define-constant +sreal-infinity+
    (matlisp-ffi::with-fortran-float-modes
      (/ 1e0 0e0)))
(define-constant +sreal-nan+
    (matlisp-ffi::with-fortran-float-modes
      (/ 0e0 0e0)))

(defleaf real-tensor (real-blas-tensor) ())
(deft/method t/field-type (sym real-tensor) ()
  'double-float)
(deft/method t/complexified-type (sym real-tensor) ()
  'complex-tensor)

(defleaf sreal-tensor (real-blas-tensor) ())
(deft/method t/field-type (sym sreal-tensor) ()
  'single-float)
(deft/method t/complexified-type (sym sreal-tensor) ()
  'scomplex-tensor)

;;Complex tensor
(defclass complex-numeric-tensor (numeric-tensor) ())
(deft/method t/field-type (sym complex-numeric-tensor) ()
  'complex)
(deft/method t/complexified-type (sym complex-numeric-tensor) ()
  sym)
;;
(defclass complex-blas-tensor (complex-numeric-tensor blas-numeric-tensor) ())
(deft/method t/l1-lb (sym complex-blas-tensor) ()
  '*complex-l1-fcall-lb*)
(deft/method t/l2-lb (sym complex-blas-tensor) ()
  '*complex-l2-fcall-lb*)
(deft/method t/l3-lb (sym complex-blas-tensor) ()
  '*complex-l3-fcall-lb*)

;;Comment this block if you want to use (simple-array (complex double-float) (*))
;;as the underlying store. This will make Lisp-implementations of gemm .. faster
;;but you'll lose the ability to use tensor-realpart~/imagpart~.
(progn
  (deft/method t/store-element-type (sym complex-blas-tensor) ()
	       (second (field-type sym)))

  (deft/method t/compute-store-size (sym complex-blas-tensor) (size)
	       `(* 2 ,size))

  (deft/method t/store-size (sym complex-blas-tensor) (vec)
	       `(/ (length ,vec) 2))

  (deft/method t/store-ref (sym complex-blas-tensor) (store &rest idx)
	       (assert (null (cdr idx)) nil "given more than one index for linear-store")
	       (let ((idx (car idx)))
		 (using-gensyms (decl (store idx))
		   `(let (,@decl)
		      (declare (type ,(store-type sym) ,store))
		      (complex (aref ,store (* 2 ,idx)) (aref ,store (1+ (* 2 ,idx))))))))

  (deft/method t/store-set (sym complex-blas-tensor) (value store &rest idx)
	       (assert (null (cdr idx)) nil "given more than one index for linear-store")
	       (let ((idx (car idx)))
		 (using-gensyms (decl (store idx value))
		   `(let (,@decl)
		      (declare (type ,(store-type sym) ,store)
			       (type ,(field-type sym) ,value))
		      (setf (aref ,store (* 2 ,idx)) (cl:realpart ,value)
			    (aref ,store (1+ (* 2 ,idx))) (cl:imagpart ,value))
		      nil))))

  (defmethod store-size ((tensor complex-blas-tensor))
    (floor (/ (length (store tensor)) 2))))

(defmethod print-element ((tensor complex-blas-tensor)
			  element stream)
  (let ((realpart (realpart element))
	(imagpart (imagpart element)))
    (if (not (zerop imagpart))
	(format stream "~,4,-2,,,,'Eg + ~,4,-2,,,,'Egi"  realpart imagpart)
	(format stream "~,4,-2,,,,'Eg" realpart))))
;;
(defleaf complex-tensor (complex-blas-tensor) ())
(deft/method t/field-type (sym complex-tensor) ()
  '(complex double-float))
(deft/method t/realified-type (sym complex-tensor) ()
  'real-tensor)

(defleaf scomplex-tensor (complex-blas-tensor) ())
(deft/method t/field-type (sym scomplex-tensor) ()
  '(complex single-float))
(deft/method t/realified-type (sym scomplex-tensor) ()
  'sreal-tensor)
