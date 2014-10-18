(in-package #:matlisp-ffi)

;;Pkhuong's trick
(defstruct (wrap (:constructor make-wrap (sap)))
  (sap (cffi:null-pointer) :type cffi:foreign-pointer :read-only t))

(defun sap-wrap (sap &optional finalizer)
  (let* ((wrap (make-wrap sap)))
    (tg:finalize wrap (if finalizer
			  #'(lambda () (funcall finalizer sap))
			  #'(lambda () (cffi:foreign-free sap))))))

;;
(defstruct (foreign-vector (:conc-name fv-))
  (pointer (cffi:null-pointer) :type cffi:foreign-pointer)
  (size 0 :type fixnum)
  (type nil :type symbol))

(defmethod print-object ((obj foreign-vector) stream)
  (format stream "#F(")
  (dotimes (i (fv-size obj))
    (format stream "~A " (fv-ref obj i)))
  (format stream ")"))

(definline fv-ref (x n)
  (declare (type foreign-vector x)
	   (type fixnum n))
  (assert (< -1 n (fv-size x)) nil 'out-of-bounds-error :requested n :bound (fv-size x) :message "From inside fv-ref.")
  (cffi:mem-aref (fv-pointer x) (fv-type x) n))

(definline (setf fv-ref) (value x n)
  (declare (type foreign-vector x)
	   (type fixnum n))
  (assert (< -1 n (fv-size x)) nil 'out-of-bounds-error :requested n :bound (fv-size x) :message "From inside fv-ref.")
  (setf (cffi:mem-aref (fv-pointer x) (fv-type x) n) value))
