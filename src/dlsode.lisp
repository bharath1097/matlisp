#+nil
(progn
(asdf:oos 'asdf:load-op :cffi)

(load "f77-mangling.lisp")
(load "cffi-helpers.lisp")
(load "ffi-cffi.lisp")
)

(cffi:define-foreign-library libodepack
  (:unix #.(translate-logical-pathname "matlisp:lib;libodepack.dylib"))
  (t (:default "libodepack")))

(cffi:use-foreign-library libodepack)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MATLISP")
(def-fortran-routine dlsode :void
  "DLSODE in ODEPACK"
  (field (:callback :void
		    (c-neq :integer :input)
		    (c-t :double-float :input)
		    (c-y (* :double-float) :input)
		    (c-ydot (* :double-float) :output)))
  (neq :integer :input)
  (y (* :double-float) :input-output)
  (ts :double-float :input-output)
  (tout :double-float :input)
  (itol :integer :input)
  (rtol (* :integer) :input)
  (atol (* :integer) :input)
  (itask :integer :input)
  (istate :integer :input-output)
  (iopt :integer :input)
  (rwork (* :double-float) :input-output)
  (lrw :integer :input)
  (iwork (* :integer) :input-output)
  (liw :integer :input)
  (jacobian (:callback :void
		       (c-neq :integer :input)
		       (c-t :double-float :input)
		       (c-y (* :double-float) :input)
		       (c-upper-bandwidth :integer :input)
		       (c-lower-bandwidth :integer :input)
		       (c-pd (* :double-float) :output)
		       (c-nrowpd :integer :input)))
  (mf :integer :input))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lsode-evolve (field y t-array report)
  ;; Use gensym ? Will have to use a macrolet.
  (cffi:defcallback *evolve-callback* :void ((c-neq :pointer :int)
					     (c-tc :pointer :double)
					     (c-y :pointer :double)
					     (c-ydot :pointer :double))
    (let* ((neq (cffi:mem-aref c-neq :int))
	   (y (make-array neq :element-type 'double-float :initial-element 0d0))
	   (ts (cffi:mem-aref c-tc :double)))
      ;; Copy things to simple-arrays
      (loop for i from 0 below neq
	 do (setf (aref y i) (cffi:mem-aref c-y :double i)))     
      ;; Assume form of field
      (let ((ydot (funcall field ts y)))
      ;; Copy ydot back
	(loop for i from 0 below neq
	   do (setf (cffi:mem-aref c-ydot :double i) (aref ydot i))))))
  ;;
  (let* ((neq (length y))
	 (lrw (+ 22 (* 9 neq) (* neq neq) 5))
	 (liw (+ 20 neq 5))
	 (tout 0d0)
	 (ts (aref t-array 0))
	 (tout (aref t-array 0))
	 (itol 1)
	 (atol (make-array 1 :element-type 'double-float :initial-element 1d-8))
	 (rtol (make-array 1 :element-type 'double-float :initial-element 0d0))
	 (itask 1)
	 (istate 1)
	 (iopt 0)
	 (mf 22)
	 (rwork (make-array lrw :element-type 'double-float :initial-element 0d0))
	 (iwork (make-array liw :element-type '(signed-byte 32) :initial-element 0)))
    (loop for i from 1 below (length t-array)
       do (progn
	    (setq tout (aref t-array i))
	    (multiple-value-bind (y-out ts-out istate-out rwork-out iwork-out)
		(dlsode (cffi:callback *evolve-callback*) neq y ts tout itol rtol atol itask istate iopt rwork lrw iwork liw (cffi:null-pointer) mf)
	      (setq ts ts-out)
	      (setq istate istate-out))
	    (funcall report ts y)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pend-field (ts y)
  (make-array 2 :element-type 'double-float :initial-contents `(,(aref y 1) ,(- (sin (aref y 0))))))

(defun pend-report (ts y)
  (format t "~A ~A ~A ~%" ts (aref y 0) (aref y 1)))

(defvar y (make-array 2 :element-type 'double-float :initial-contents `(,(/ pi 2) 0d0)))

(lsode-evolve #'pend-field y #(0d0 1d0) #'pend-report)