;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: :fortran-ffi-accessors; Base: 10 -*-
;; Allowed types:
;; :single-float :double-float
;; :complex-single-float :complex-double-float
;; :integer :long

;; Callbacks : (:function <output-type> {(params)})

;;TODO add declarations to generated wrappers.

(in-package #:matlisp-ffi)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-constant +ffi-styles+
      '(:input :input-reference :input-value
	:input-output :output :workspace-output
	:workspace))

  (define-constant +ffi-types+
      '(:single-float :double-float
	:complex-single-float :complex-double-float
	:integer :long
	:string :character
	:callback))

  (define-constant +ffi-array-types+
      '(:single-float :double-float
	:integer :long))

  ;; Separte the body of code into documentation and parameter lists.
  (defun parse-doc-&-parameters (body &optional header footer)
    (if (stringp (first body))
	(values `(,(%cat% header (first body) footer)) (rest body))
	(values (if (or header footer)
		    (%cat% header "" footer)
		    nil)
		body))))

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
;; Allocate objects from the heap
  (recursive-append
   (when declarations
     `(let (,@(mapcar (lambda (decl) (let ((var (car decl)))
					 (check-type var symbol)
					 `(,var (cffi:foreign-alloc ,@(cdr decl)))))
			declarations))))
    ;; Store result and then free foreign-objects
   (when declarations
       `(multiple-value-prog1))
   `((progn
       ,@body)
     ;;Free C objects
     ,@(mapcar (lambda (decl) `(cffi:foreign-free ,(car decl)))
	       declarations))))

;; Create objects on the stack and run the "body" of code.
(defmacro with-foreign-objects-stacked (declarations &rest body)
"
  Allocate \"objects\" on the stack and run the \"body\" of code.

  with-foreign-objects-stacked (declarations) &rest body
  binding := {(var type &optional count &key (initial-contents nil))}*

  Example:
  >> (with-foreign-objects-stacked ((x :int :count 10 :initial-element 2))
       (+ (cffi:mem-aref x :int 2) 1))
  3
  >>
"
  (let ((wfo-decl nil)
	(wfo-body nil)
	(wfo-before nil))
    (dolist (decl declarations)
      (destructuring-bind (var type &key (count 1) initial-element initial-contents) decl
	;;Make sure the var and type are symbols;;
	(check-type var symbol)
	(check-type type symbol)
	(when (and initial-element initial-contents)
	  (error "Cannot apply both :initial-element and :initial-contents at the same time."))
	;;
	(if (eq count 1)
	    (progn
	      ;; Count defaults to one in with-foreign-objects
	      (nconsc wfo-decl `((,var ,type)))
	      (if (or initial-element initial-contents)
		  (nconsc wfo-body `((setf (cffi:mem-aref ,var ,type 0) ,@(cond
									   (initial-element `(,initial-element))
									   (initial-contents `((elt ,initial-contents 0)))))))))
	    ;;
	    (let ((decl-count (gensym))
		  (decl-init (gensym))
		  (loop-var (gensym)))
	      ;;
	      (nconsc wfo-before `((,decl-count ,count)))
	      (nconsc wfo-before `((,decl-init ,(or initial-element initial-contents))))
	      ;;
	      (nconsc wfo-decl `((,var ,type ,decl-count)))
	      (if (or initial-element initial-contents)
		  (nconsc wfo-body `((dotimes (,loop-var ,decl-count)
				       (setf (cffi:mem-aref ,var ,type ,loop-var) ,@(cond
										     (initial-element `(,decl-init))
										     (initial-contents `((elt ,decl-init ,loop-var)))))))))))))
	(recursive-append
	 (when wfo-before
	   `(let (,@wfo-before)))
	 (if wfo-decl
	     `(cffi:with-foreign-objects (,@wfo-decl))
	     `(progn))
	 `(,@wfo-body
	   ,@body))))

;; Increment the pointer.
(definline inc-sap (sap type &optional (n 1))
  "
  Increment the pointer address by one \"slot\"
  depending on the type:
          :double-float  8 bytes
          :single-float  4 bytes
          :complex-double-float 8x2 bytes
          :complex-single-float 4x2 bytes
 "
    (cffi:inc-pointer sap
		      (ecase type
			(:double-float  (* n 8))
			(:single-float  (* n 4))
			(:complex-double-float (* n 16))
			(:complex-single-float (* n 8)))))

(define-modify-macro incf-sap (type &optional (n 1)) inc-sap)

;; Supporting multidimensional arrays is a pain.
;; Only support types that we currently use.
(deftype matlisp-specialized-array ()
  `(or (simple-array double-float (*))
       (simple-array single-float (*))
       ;;
       (simple-array (signed-byte 64) *)
       (simple-array (signed-byte 32) *)
       ;;
       (simple-array (unsigned-byte 64) *)
       (simple-array (unsigned-byte 32) *)
       ;;
       cffi:foreign-pointer))

;; Very inefficient - compilation wise, not runtime wise- 
;; (but portable!) way of supporting both SAPs and simple-arrays.
;; 2^n branching within the macro.
#-(or sbcl cmu ccl)
(defmacro with-vector-data-addresses (vlist &rest body)
  "
with-vector-data-addresses vlist &rest body
vlist:
   binding := {(addr-var var)}

Example:
>> (let ((x (make-array 10 :element-type 'double-float :initial-element 1d0)))
      (with-vector-data-addresses ((addrx x))
         (+ (mem-aref addrx :double 0) pi)))
4.141592653589793d0
>>
"  
  (labels ((with-pointer-or-vector-data-address (vlist body)
	     (let ((inc-body (ecase (length vlist)
			       (2 nil)
			       (4 `((incf-sap ,(nth 2 vlist) ,(nth 0 vlist) ,(nth 3 vlist)))))))
	       `(if (cffi:pointerp ,(cadr vlist))
		    (let (,(car vlist) ,(cadr vlist))
		      ,@inc-body
		      ,@body)
		    (cffi-sys:with-pointer-to-vector-data (,(car vlist) ,(cadr vlist))
		      ,@inc-body
		      ,@body))))
	     (frob (v body)
		   (if (null v)
		       body
		       `(,(with-pointer-or-vector-data-address (car v)
							       (frob (rest v) body))))))
	   `(with-fortran-float-modes
	      ,@(frob vlist body))))
