;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: :fortran-ffi-accessors; Base: 10 -*-
(in-package #:matlisp-ffi)

(eval-when (:compile-toplevel :load-toplevel :execute)

;; Create objects on the heap and run some stuff.
(defmacro with-foreign-objects-heaped (declarations &rest body)
"
  Allocate \"objects\" on the heap and run the \"body\" of code.

  with-foreign-objects-heap-ed (declarations) &rest body
  binding := {(var type &optional count &key (initial-contents nil))}*

  Example:
  > (with-foreign-objects-heaped ((x :int :count 10 :initial-element 2))
       (+ (cffi:mem-aref x :int 2) 1))
  3
  >
"
  (recursive-append
   (when declarations
     `(let (,@(mapcar #'(lambda (decl) (let ((var (first decl)))
					 (check-type var symbol)
					 `(,var (cffi:foreign-alloc ,@(cdr decl)))))
		      declarations))))
    ;; Store result and then free foreign-objects
   (when declarations `(unwind-protect))
   `((progn ,@body)
     ;;Free C objects
     ,@(mapcar #'(lambda (decl) `(cffi:foreign-free ,(car decl))) declarations))))

(defmacro with-foreign-objects-stacked (declarations &rest body)
  "
  Allocate @arg{objects} on the stack and run the @arg{body} of code.

  with-foreign-objects-stacked (declarations) &rest body
  binding := {(var type &optional count &key (initial-contents nil))}*

  Example:
  >> (with-foreign-objects-stacked ((x :int :count 10 :initial-element 2))
       (+ (cffi:mem-aref x :int 2) 1))
  3
  >>
  "
  (let* ((is (gensym)) (es (gensym)) (es+ (gensym))
	 count-decls
	 (code (mapcar #'(lambda (d) (destructuring-bind (var type &key (count 1 count-p) initial-element initial-contents) d
				       (declare (type symbol var type))
				       (let ((count (if (not count-p) count
							(with-gensyms (csym)
							  (push `(,csym ,count) count-decls)
							  csym))))
					 (cons
					  `(,var ,type ,count)
					  (cond
					    ((and initial-element initial-contents) (error "Cannot apply both :initial-element and :initial-contents at the same time."))
					    (initial-element (with-gensyms (init)
							       `(let ((,init ,initial-element))
								  ,(recursive-append
								    (when count-p `(dotimes (,is ,count)))
								    `(setf (cffi:mem-aref ,var ,type ,@(when count-p `(,is))) ,init)))))
					    (initial-contents `(loop :for ,is :of-type cl:fixnum :from 0 :below ,count
								  :for (,es . ,es+) :on ,initial-contents
								  :do (setf (cffi:mem-aref ,var ,type ,is) ,es)
								  :finally (assert (and (= ,is ,count) (null ,es+)) nil "length of argument 'INITIAL-CONTENTS' does not match that of the array"))))))))
		       declarations)))
    `(let (,@count-decls)
       (with-foreign-objects (,@(mapcar #'car code))
	 ,@(remove-if #'null (mapcar #'cdr code))
	 ,@body))))

;;TODO: Add support for {ECL, clisp, Allegro CL, Lispworks}
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
  #+ecl
  (let ((%trap-bits (gensym "%TRAP-BITS-")))
    `(let ((,%trap-bits (si::trap-fpe 'cl:last t)))
       (unwind-protect
	    (progn (si::trap-fpe ,%trap-bits nil) ,@body)
	 (si::trap-fpe ,%trap-bits t))))
  #-(or cmu sbcl ccl ecl)
  `(progn
     ,@body))

(defmacro without-gcing (&body body)
  (append
   #+sbcl `(sb-sys::without-gcing)
   #+cmu `(system::without-gcing)
   #+ccl `(ccl::without-gcing)
   body))

(definline vector-sap-interpreter-specific (vec)
  #+sbcl (sb-sys:vector-sap vec)
  #+cmu (system:vector-sap vec)
  #+ccl (let ((addr-vec (ccl:%null-ptr)))
	  (declare (type ccl:macptr addr-vec))
	  (ccl::%vect-data-to-macptr vec addr-vec)))

)
