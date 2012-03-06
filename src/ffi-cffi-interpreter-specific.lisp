;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: :fortran-ffi-accessors; Base: 10 -*-
;; Yes the file name is an oxymoron.

(in-package "FORTRAN-FFI-ACCESSORS")

(defmacro with-fortran-float-modes (&body body)
  "Execute the body with the IEEE FP modes appropriately set for Fortran"
  #+cmu
  `(ext:with-float-traps-masked (:underflow :overflow :inexact :divide-by-zero :invalid)
   ,@body)
  #+sbcl
  `(sb-int:with-float-traps-masked (:underflow :overflow :inexact :divide-by-zero :invalid)
     ,@body)
  #+ccl
  (let ((old-fpu-modes (gensym "OLD-FPU-MODES-")))
    `(let ((,old-fpu-modes (ccl:get-fpu-mode)))
       (unwind-protect
	    (progn
	      (ccl:set-fpu-mode :overflow nil
				:underflow nil
				:division-by-zero nil
				:invalid nil
				:inexact nil)
	      ,@body)
	 (apply #'ccl:set-fpu-mode ,old-fpu-modes))))
  #-(or cmu sbcl ccl)
  `(progn
     ,@body))

;; Define specialised routines for CMUCL/SBCL
;; Borrowed from ffi-sbcl/ffi-cmucl.lisp
#+(or sbcl cmu ccl)
(declaim (inline vector-data-address))
#+(or sbcl cmu ccl)
(defun vector-data-address (vec)
  "Return the physical address of where the actual data of the object
VEC is stored.

  VEC - must be a either a (complex double-float), (complex single-float)
        or a specialized array type in CMU Lisp.  This currently means
        VEC is a simple-array of one dimension of one of the following types:

                  double-float
                  single-float
        or a
                  system-area-pointer

Returns
  1   - system area pointer to the actual data"
  (locally
      (declare (optimize (speed 1) (safety 3)))
    ;; It's quite important that the arrays have the write type.
    ;; Otherwise, we will probably get the address of the data wrong,
    ;; and then foreign function could be scribbling over who knows
    ;; where!
    ;;
    (check-type vec matlisp-specialized-array))
  (locally
      (declare (type matlisp-specialized-array vec)
	       (optimize (speed 3) (safety 0) (space 0)))
    ;;vec is either a simple-array or a system-area-pointer itself.
    (if (typep vec '(simple-array * (*)))
	#+sbcl (sb-sys:vector-sap vec)
	#+cmu (system:vector-sap vec)
	#+ccl (let ((addr-vec (ccl:%null-ptr)))
		(declare (type ccl:macptr addr-vec))
		(ccl::%vect-data-to-macptr vec addr-vec))
	vec)))

(defmacro without-gcing (&body body)
  (append
   #+sbcl `(sb-sys::without-gcing)
   #+cmu `(system::without-gcing)
   #+ccl `(ccl::without-gcing)
   body))

#+(or sbcl cmu ccl)
(defmacro with-vector-data-addresses (vlist &body body)
  "WITH-VECTOR-DATA-ADDRESSES (var-list &body body)

 Execute the body with the variables in VAR-LIST appropriately bound.
 VAR-LIST should be a list of pairs.  The first element is the address
 of the desired object; the second element is the variable whose address
 we want.

 Garbage collection is also disabled while executing the body."
  ;; We wrap everything inside a WITHOUT-GCING form to inhibit garbage
  ;; collection to avoid complications that may arise during a
  ;; collection while in a fortran call.
  ;;
  ;; This might not really be necessary, but it's not clear if the
  ;; alien object will have the right value if GC occurs after getting
  ;; the alien object but before the alien function is called.  Let's
  ;; be safe rather than sorry.
  `(with-fortran-float-modes
     (without-gcing
       (let (,@(mapcar #'(lambda (lst)
			   (destructuring-bind (addr-var var &key inc-type inc) lst
			     `(,addr-var ,@(if inc
					       `((cffi:inc-pointer (vector-data-address ,var)
								   ,@(case inc-type
									   (:double-float  `((* ,inc 8)))
									   (:single-float `((* ,inc 4)))
									   (:complex-double-float  `((* ,inc 16)))
									   (:complex-single-float  `((* ,inc 8)))
									   (t `(,inc)))))
					       `((vector-data-address ,var))))))
		       vlist))
	 ,@body))))