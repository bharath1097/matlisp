;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: :fortran-ffi-accessors; Base: 10 -*-
;; Allowed types:
;; :single-float :double-float
;; :complex-single-float :complex-double-float
;; :integer :long

;; Written by Akshay Srinivasan

;; Callbacks : (:function <output-type> {(params)})

(in-package "FORTRAN-FFI-ACCESSORS")

;; (defconstant +ffi-types+ '(:single-float :double-float
;; 			   :complex-single-float :complex-double-float
;; 			   :integer :long
;; 			   :string
;; 			   :callback))

;; (defconstant +ffi-styles+ '(:input :input-value
;; 			    :input-output :output))

;; Get the equivalent CFFI type.
;; If the type is an array, get the type of the array element type.
(defun ->cffi-type (type)
  "Convert the given Fortran FFI type into a type understood by CFFI."
  (cond
   ((and (listp type) (eq (first type) '*)) `(:pointer ,@(->cffi-type (second type))))
   ((callback-type-p type) `(:pointer ,@(->cffi-type :callback)))
   ((eq type :complex-single-float) `(:pointer ,@(->cffi-type :single-float)))
   ((eq type :complex-double-float) `(:pointer ,@(->cffi-type :double-float)))
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
			(destructuring-bind (name type &optional (style :input)) decl
			  (case type
			    (:string
			     ;; String lengths are appended to the function arguments,
			     ;; passed by value.
			     (pushnew `(,(scat "LEN-" name) ,@(->cffi-type :integer)) aux-pars)
			     `(,name ,@(->cffi-type :string)))
			    (t
			     `(,name ,@(get-read-in-type type style))))))
		    pars)))
      `(;; don't want documentation for direct interface, not useful
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
		  ;; Callbacks are tricky because the data inside pointer arrays will need to
		  ;; be copied without implicit knowledge of the size of the array.
		  ;; This is usually taken care of by special data structure - ala GSL -
		  ;; or by passing additional arguments to the callback to apprise it of the
		  ;; bounds on the arrays. This *cannot* be automated within a macro and has to
		  ;; be hand-tweaked.
		  ((callback-type-p type)
		   (setq ffi-var var))
		  ;; Can't really enforce "style" when given an array.
		  ;; Complex numbers do not latch onto this case, they are passed by value.
		  ((array-p type)
		   (setq ffi-var (scat "ADDR-" var))
		   (setq array-vars
			 `(,@array-vars (,ffi-var ,var))))
		  ;; Strings
		  ((string-p type)
		   (setq ffi-var var)
		   (setq aux-var (scat "LEN-" var))
		   (setq aux-args
			 `(,@aux-args (,aux-var (length (the string ,var))))))
		  ;; Pass-by-value variables
		  ((eq style :input-value)
		   (setq ffi-var var))
		  ;; Pass-by-reference variables
		  (t
		   (cond
		     ;; Makes more sense to copy complex numbers into arrays,
		     ;; rather than twiddling around with lisp memory internals.
		     ((member type '(:complex-single-float :complex-double-float))
		      (setq ffi-var (scat "ADDR-REAL-CAST-" var))
		      (setq ref-vars
			    `(,@ref-vars (,ffi-var ,(second (->cffi-type type)) :count 2 :initial-contents (list (realpart ,var) (imagpart ,var))))))
		     (t
		      (setq ffi-var (scat "REF-" var))
		      (setq ref-vars
			    `(,@ref-vars (,ffi-var ,@(->cffi-type type) :initial-element ,var)))))))
		;; Output variables
		(when (and (output-p style) (not (eq type :string)))
		  (setq return-vars
			`(,@return-vars (,ffi-var ,var ,type))))
		;; Arguments for the lisp wrapper
		(when (not (eq var hidden-var-name))
		  (setq defun-args
			`(,@defun-args ,var)))
		;; Arguments for the FFI function
		(setq ffi-args
		      `(,@ffi-args ,ffi-var))
		;; Auxillary arguments for FFI
		(when (not (null aux-var))
		  (setq aux-ffi-args
			`(,@aux-ffi-args ,aux-var))))))
      ;;Return the function definition
      `(
      	(defun ,name ,defun-args
      	  ,@doc
	  (let (,@(if (not (null hidden-var-name))
		      `((,hidden-var-name ,@(if (eq (second (first pars)) :complex-single-float)
						`(#C(0e0 0e0))
						`(#C(0d0 0d0)))))))
	    (with-foreign-objects-stack-ed (,@ref-vars)
	      (with-vector-data-addresses (,@array-vars)
		(let* (,@aux-args		     
		       ;;Style warnings are annoying.
		       ,@(if (not (eq return-type :void))
			     `((ret (,ffi-fn ,@ffi-args ,@aux-ffi-args))))
		       )
		  ,@(if (eq return-type :void)
			`((,ffi-fn ,@ffi-args ,@aux-ffi-args)))
		  ;; Copy values in reference pointers back to local variables.
		  ;; Lisp has local scope; its safe to modify variables in parameter lists.
		  ,@(mapcar #'(lambda (decl)
				(destructuring-bind (ffi-var var type) decl
				  (if (member type '(:complex-single-float :complex-double-float))
				      `(setq ,var (complex (cffi:mem-aref ,ffi-var ,(second (->cffi-type type)) 0) (cffi:mem-aref ,ffi-var ,(second (->cffi-type type)) 1)))
				      `(setq ,var (cffi:mem-aref ,ffi-var ,@(->cffi-type type))))))
			    (remove-if-not #'(lambda (x) (member (first x) ref-vars :key #'car))
					   return-vars))
		  ,(if (not (eq return-type :void))
		       `(values ret ,@(mapcar #'second return-vars))
		       `(values ,@(mapcar #'second return-vars))))))))))))

;; Supporting multidimensional arrays is a pain.
(deftype matlisp-specialized-array ()
  `(or (complex double-float)
       (complex single-float)
       (simple-array (complex double-float) (*))
       (simple-array (complex single-float) (*))
       (simple-array double-float (*))
       (simple-array single-float (*))
       (simple-array (signed-byte 32) (*))
       (simple-array (signed-byte 16) (*))
       (simple-array (signed-byte  8) (*))
       (simple-array (unsigned-byte 32) (*))
       (simple-array (unsigned-byte 16) (*))
       (simple-array (unsigned-byte  8) (*))))

(defun vector-sap (vec)
  #+cmu (sys:vector-sap vec)
  #+sbcl (sb-sys:vector-sap vec))

(defun vector-data-address (vec)
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
    (if (typep vec '(simple-array * (*)))
	(vector-sap vec)
	(error "Unsupported type (!) : ~S" (type-of vec)))))

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
    `(let ((old-fpu-modes (get-fpu-mode)))
       (unwind-protect
	    (progn ,@body)
	 (apply #'set-fpu-mode old-fpu-modes)))))


#+nil
(defmacro with-vector-data-addresses (vlist &body body)
  `(with-fortran-float-modes
     (#+cmu sys::without-gcing
      #+sbcl sb-sys::without-gcing
      #-(or cmu sbcl) progn
      (let (,@(mapcar #'(lambda (pair)
			  `(,(first pair)
			    (vector-data-address ,(second pair))))
		      vlist))
	,@body))))

(defmacro with-vector-data-addresses (vlist &body body)
  (labels ((frob (v body)
	     (if (rest v)
		 `(cffi-sys:with-pointer-to-vector-data (,(caar v) ,(cadar v))
		    ,(frob (rest v) body))
		 `(cffi-sys:with-pointer-to-vector-data (,(caar v) ,(cadar v))
		    ,@body))))
    `(with-fortran-float-modes
       ,@(if (null vlist)
	     `(,@body)
	     `(,(frob vlist body))))))

#+ccl
(defmacro ccl-with-vector-data-addresses (vlist &body body)
  (if (rest vlist)
      `(with-fortran-float-modes
	 (with-pointer-to-ivector (,(caar vlist) ,(cadar vlist))
	   (ccl-with-vector-data-addresses ,(rest vlist) ,@body)))
      `(with-pointer-to-ivectoor (,(caar vlist) ,(cdar vlist))
	 ,@body)))


