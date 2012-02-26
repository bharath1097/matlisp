;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: :fortran-ffi-accessors; Base: 10 -*-
;; Allowed types:
;; :single-float :double-float
;; :complex-single-float :complex-double-float
;; :integer :long

;; Written by Akshay Srinivasan


;; Callbacks : (:function <output-type> {(params)})

(in-package "FORTRAN-FFI-ACCESSORS")

#+(or)
(defconstant +ffi-types+ '(:single-float :double-float
			   :complex-single-float :complex-double-float
			   :integer :long
			   :string
			   :callback))

#+(or)
(defconstant +ffi-styles+ '(:input :input-value
			    :input-output :output))


(defmacro with-gensyms (symlist &body body)
  `(let ,(mapcar #'(lambda (sym)
		     `(,sym (gensym ,(symbol-name sym))))
		 symlist)
     ,@body))

;; Helper macro to do setf and nconc
;; for destructive list updates.
(defmacro nconsc (var &rest args)
  (if (null args) var
      `(if (null ,var)
	   (progn
	     (setf ,var ,(car args))
	     (nconc ,var ,@(cdr args)))
	   (nconc ,var ,@args))))

;; Create objects on the heap and run some stuff.
(defmacro with-foreign-objects-heap-ed (declarations &rest body)
    "
Allocate \"objects\" on the heap and run the \"body\" of code.

with-foreign-objects-heap-ed (declarations) &rest body
binding := {(var type &optional count &key (initial-contents nil))}*

Example:
>> (with-foreign-objects-heap-ed ((x :int :count 10 :initial-element 2))
     (+ (cffi:mem-aref x :int 2) 1))
3
>>
  "
  (let ((ret (gensym)))
    ;; Allocate objects from the heap
    `(let* (,@(mapcar (lambda (decl) (list (car decl) `(cffi:foreign-alloc ,@(cdr decl))))
		      declarations)
	    ;; Store result temporarily
	    (,ret (progn ,@body)))
       ;;Free C objects
       ,@(mapcar (lambda (decl) `(cffi:foreign-free ,(car decl)))
		 declarations)
       ,ret)))

;; Create objects on the stack and run the "body" of code.
(defmacro with-foreign-objects-stack-ed (declarations &rest body)
  "
Allocate \"objects\" on the stack and run the \"body\" of code.

with-foreign-objects-stack-ed (declarations) &rest body
binding := {(var type &optional count &key (initial-contents nil))}*

Example:
>> (with-foreign-objects-stack-ed ((x :int :count 10 :initial-element 2))
     (+ (cffi:mem-aref x :int 2) 1))
3
>>
  "
  (if (null declarations)
      `(progn ,@body)
      (let ((wfo-decl nil)
	    (wfo-body nil)
	    (wfo-before nil))
	(loop for decl in declarations
	   do (destructuring-bind (var type &key (count 1) initial-element initial-contents) decl
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
										   (initial-contents `((car ,initial-contents)))))))))
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
			    (nconsc wfo-body `((loop for ,loop-var from 0 below ,decl-count
						  do (setf (cffi:mem-aref ,var ,type ,loop-var) ,@(cond
												   (initial-element `(,decl-init))

												   (initial-contents `((elt ,decl-init ,loop-var)))))))))))))
	`(let (,@wfo-before)
	   (cffi:with-foreign-objects (,@wfo-decl)
	     ,@wfo-body
	     ,@body)))))

;; Get the equivalent CFFI type.
;; If the type is an array, get the type of the array element type.
(defun ->cffi-type (type)
  "Convert the given Fortran FFI type into a type understood by CFFI."
  (cond
    ((and (listp type) (eq (first type) '*))
     `(:pointer ,@(->cffi-type (second type))))
    ((callback-type-p type)
     `(:pointer ,@(->cffi-type :callback)))
    ((eq type :complex-single-float)
     `(:pointer ,@(->cffi-type :single-float)))
    ((eq type :complex-double-float)
     `(:pointer ,@(->cffi-type :double-float)))
    (t `(,(ecase type
	    (:void :void)
	    (:integer :int)
	    (:long :long)
	    (:single-float :float)
	    (:double-float :double)
	    (:string :string)
	    ;; Pass a pointer to the function.
	    (:callback :void))))))

;; Check if given type is a string
(declaim (inline string-p))
(defun string-p (type)
  (eq type :string))

;; Check if given type is an array
(declaim (inline array-p))
(defun array-p (type)
  (and (listp type) (eq (car type) '*)))

;; Check if the given type is - or has to be passed as - an array.
(defun cast-as-array-p (type)
  (or (if (listp type)
	  (eq (car type) '*))
      (eq type :complex-single-float)
      (eq type :complex-double-float)))

;; Check if the given type is a callback.
(declaim (inline callback-type-p))
(defun callback-type-p (type)
  (and (listp type) (eq (first type) :callback)))

;; Fortran functions return-by-values.
(defun get-return-type (type)
  (if (or (cast-as-array-p type) (callback-type-p type))
      (error "Cannot have a Fortran function output the type: ~S directly." type)
      (->cffi-type type)))

;; If output
(declaim (inline output-p))
(defun output-p (style)
  (member style '(:output :input-output)))

;; CFFI doesn't nearly have as nice an FFI as SBCL/CMUCL.
(defun get-read-in-type (type &optional (style :input))
  (cond
    ;; Can't do much else if type is an array/complex or input is passed-by-value.
    ((or (callback-type-p type) (cast-as-array-p type) (eq style :input-value))
     (->cffi-type type))
    ;; else pass-by-reference
    (t
     `(:pointer ,@(->cffi-type type)))))

;; Separte the body of code into documentation and parameter lists.
(defun parse-doc-&-parameters (body &optional header footer)
  (if (stringp (first body))
      (values `(,(%cat% header (first body) footer)) (rest body))
    (values (if (or header footer)
		(%cat% header "" footer)
	      nil)
	    body)))

;; Parse fortran parameters and convert parameters to native C90 types (and
;; add additional function parameters)
(defun parse-fortran-parameters (body)
  (multiple-value-bind (doc pars)
      (parse-doc-&-parameters body)
    (declare (ignore doc))

    (let* ((aux-pars nil)
	   (new-pars
	     (mapcar #'(lambda (decl)
			 (destructuring-bind (name type &optional (style :input))
			     decl
			   (case type
			     (:string
			      ;; String lengths are appended to the function arguments,
			      ;; passed by value.
			      (pushnew `(,(scat "LEN-" name) ,@(->cffi-type :integer))
				       aux-pars)
			      `(,name ,@(->cffi-type :string)))
			     (t
			      `(,name ,@(get-read-in-type type style))))))
		     pars)))
      `( ;; don't want documentation for direct interface, not useful
	;; ,@doc
	,@new-pars ,@aux-pars))))

;; Call defcfun to define the foreign function.
;; Also creates a nice lisp helper function.
(defmacro def-fortran-routine (name return-type &rest body)
  (let ((fortran-name (make-fortran-name `,name))
	(lisp-name  (make-fortran-ffi-name `,name))
	(hack-return-type `,return-type)
	(hack-body `(,@body))
	(hidden-var-name nil))
    
    (multiple-value-bind (doc pars)
	(parse-doc-&-parameters `(,@body))
      (when (member hack-return-type '(:complex-single-float :complex-double-float))
	;; The return type is complex.  Since this is a "structure",
	;; Fortran inserts a "hidden" first parameter before all
	;; others.  This is used to store the resulting complex
	;; number.  Then there is no "return" value, so set the return
	;; type to :void.
	;;
	(setq hidden-var-name (gensym "HIDDEN-COMPLEX-RETURN-"))
	(setq hack-body `(,@doc
			  (,hidden-var-name ,hack-return-type :output)
			  ,@pars))
	(setq hack-return-type :void)))
			  
    `(eval-when (load eval compile)
       (progn

	 ;; Removing 'inlines' It seems that CMUCL has a problem with
	 ;; inlines of FFI's when a lisp image is saved.  Until the
	 ;; matter is clarified we leave out 'inline's
         
	 ;(declaim (inline ,lisp-name)) ;sbcl 0.8.5 has problems with
	 ;inlining
	 (cffi:defcfun (,fortran-name ,lisp-name) ,@(get-return-type hack-return-type)
	   ,@(parse-fortran-parameters hack-body))
	 ,@(def-fortran-interface name hack-return-type hack-body hidden-var-name)))))

;; Create a form specifying a simple Lisp function that calls the
;; underlying Fortran routine of the same name.
(defun def-fortran-interface (name return-type body hidden-var-name)
  (multiple-value-bind (doc pars)
      (parse-doc-&-parameters body)
    (let ((ffi-fn (make-fortran-ffi-name name))
	  (return-vars nil)
	  (array-vars nil)
	  (ref-vars nil)
	  ;;
	  (defun-args nil)
	  (aux-args nil)
	  ;;
	  (ffi-args nil)
	  (aux-ffi-args nil))
      (loop for decl in pars
	 do (destructuring-bind (var type &optional style) decl
	      (let ((ffi-var nil)
		    (aux-var nil))
		(cond
		  ;; Callbacks are tricky because the data inside
		  ;; pointer arrays will need to be copied without
		  ;; implicit knowledge of the size of the array.
		  ;; This is usually taken care of by special data
		  ;; structure - ala GSL - or by passing additional
		  ;; arguments to the callback to apprise it of the
		  ;; bounds on the arrays.
		  ;; TODO: Add support for declaring array dimensions
		  ;; in the callback declaration.
		  ((callback-type-p type)
		   (setq ffi-var var))
		  ;; Can't really enforce "style" when given an array.
		  ;; Complex numbers do not latch onto this case, they
		  ;; are passed by value.
		  ((array-p type)
		   (setq ffi-var (scat "ADDR-" var))
		   (nconsc array-vars `((,ffi-var ,var))))
		  ;; Strings
		  ((string-p type)
		   (setq ffi-var var)
		   (setq aux-var (scat "LEN-" var))
		   (nconsc aux-args `((,aux-var (length (the string ,var))))))
		  ;; Pass-by-value variables
		  ((eq style :input-value)
		   (setq ffi-var var))
		  ;; Pass-by-reference variables
		  (t
		   (cond
		     ;; Makes more sense to copy complex numbers into
		     ;; arrays, rather than twiddling around with lisp
		     ;; memory internals.
		     ((member type '(:complex-single-float :complex-double-float))
		      (setq ffi-var (scat "ADDR-REAL-CAST-" var))
		      (nconsc ref-vars
			      `((,ffi-var ,(second (->cffi-type type)) :count 2 :initial-contents (list (realpart ,var) (imagpart ,var))))))
		     (t
		      (setq ffi-var (scat "REF-" var))
		      (nconsc ref-vars
			      `((,ffi-var ,@(->cffi-type type) :initial-element ,var)))))))
		;; Output variables
		(when (and (output-p style) (not (eq type :string)))
		  (nconsc return-vars
			  `((,ffi-var ,var ,type))))
		;; Arguments for the lisp wrapper
		(when (not (eq var hidden-var-name))
		  (nconsc defun-args
			  `(,var)))
		;; Arguments for the FFI function
		(nconsc ffi-args
			`(,ffi-var))
		;; Auxillary arguments for FFI
		(when (not (null aux-var))
		  (nconsc aux-ffi-args
			  `(,aux-var))))))
    ;;Return the function definition
      (let ((retvar (gensym)))
	`(
	  (defun ,name ,defun-args
	    ,@doc
	    (let (,@(if (not (null hidden-var-name))
			`((,hidden-var-name ,@(if (eq (second (first pars))
						      :complex-single-float)
						  `(#C(0e0 0e0))
						  `(#C(0d0 0d0)))))))
	      (with-foreign-objects-stack-ed (,@ref-vars)
		(with-vector-data-addresses (,@array-vars)
		  (let* (,@aux-args		     
			 ;;Style warnings are annoying.
			 ,@(if (not (eq return-type :void))
			       `((,retvar (,ffi-fn ,@ffi-args ,@aux-ffi-args))))
			 )
		    ,@(if (eq return-type :void)
			  `((,ffi-fn ,@ffi-args ,@aux-ffi-args)))
		    ;; Copy values in reference pointers back to local
		    ;; variables.  Lisp has local scope; its safe to
		    ;; modify variables in parameter lists.
		    ,@(mapcar #'(lambda (decl)
				  (destructuring-bind (ffi-var var type) decl
				    (if (member type '(:complex-single-float :complex-double-float))
					`(setq ,var (complex (cffi:mem-aref ,ffi-var ,(second (->cffi-type type)) 0)
							     (cffi:mem-aref ,ffi-var ,(second (->cffi-type type)) 1)))
					`(setq ,var (cffi:mem-aref ,ffi-var ,@(->cffi-type type))))))
			      (remove-if-not #'(lambda (x)
						 (member (first x) ref-vars :key #'car))
					     return-vars))
		    (values
		     ,@(if (not (eq return-type :void))
			   `(,retvar))
		     ,@(mapcar #'second return-vars))))))))))))

;; Increment the pointer.
(defmacro incf-sap (type sap &optional (n 1))
  "Increment the pointer address by one \"slot\"
   depending on the type:
          :double-float  8 bytes
          :single-float  4 bytes
          :complex-double-float 8x2 bytes
          :complex-single-float 4x2 bytes
  "
  `(setf ,sap
	 (cffi:inc-pointer ,sap
			   ,@(ecase type
				    (:double-float  `((* ,n 8)))
				    (:single-float `((* ,n 4)))
				    (:complex-double-float  `((* ,n 16)))
				    (:complex-single-float  `((* ,n 8)))))))

;; Supporting multidimensional arrays is a pain.
;; Only support types that we currently use.
(deftype matlisp-specialized-array ()
  `(or (simple-array (complex double-float) (*))
       (simple-array (complex single-float) (*))
       (simple-array double-float (*))
       (simple-array single-float (*))
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
	     `(if (cffi:pointerp ,(cadr vlist))
		   (let (,vlist)
		     ,@body)
		   (cffi-sys:with-pointer-to-vector-data ,vlist
		     ,@body)))
	   (frob (v body)
	     (if (null v)
		 body
		 `(,(with-pointer-or-vector-data-address `(,(caar v) ,(cadar v))
							 (frob (rest v) body))))))
    `(with-fortran-float-modes
	 ,@(frob vlist body))))