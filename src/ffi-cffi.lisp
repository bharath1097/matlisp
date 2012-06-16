;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: :fortran-ffi-accessors; Base: 10 -*-
;; Allowed types:
;; :single-float :double-float
;; :complex-single-float :complex-double-float
;; :integer :long

;; Written by Akshay Srinivasan


;; Callbacks : (:function <output-type> {(params)})

(in-package :ffi)

(define-constant +ffi-types+ '(:single-float :double-float
			       :complex-single-float :complex-double-float
			       :integer :long
			       :string
			       :callback))

(define-constant +ffi-styles+ '(:input :input-value :workspace
				:input-output :output :workspace-output))

  
;; Create objects on the heap and run some stuff.
(defmacro with-foreign-objects-heaped (declarations &rest body)
"
  Allocate \"objects\" on the heap and run the \"body\" of code.

  with-foreign-objects-heap-ed (declarations) &rest body
  binding := {(var type &optional count &key (initial-contents nil))}*

  Example:
  >> (with-foreign-objects-heaped ((x :int :count 10 :initial-element 2))
       (+ (cffi:mem-aref x :int 2) 1))
  3
>>
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
  (member style '(:output :input-output :workspace-output)))

;; If input
(declaim (inline input-p))
(defun input-p (style)
  (member style '(:input :input-value :workspace)))

;; CFFI doesn't nearly have as nice an FFI as SBCL/CMUCL.
(defun get-read-in-type (type &optional (style :input))
  (unless (member style +ffi-styles+)
    (error "Don't know how to handle style ~A." style))
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
			      (nconsc aux-pars `((,(scat "LEN-" name) ,@(->cffi-type :integer))))
			      `(,name ,@(->cffi-type :string)))
			     (t
			      `(,name ,@(get-read-in-type type style))))))
		     pars)))
      `( ;; don't want documentation for direct interface, not useful
	;; ,@doc
	,@new-pars ,@aux-pars))))

;;
;; DEF-FORTRAN-ROUTINE
;;
;; An external Fortran routine definition form (DEF-FORTRAN-ROUTINE
;; MY-FUN ...) creates two functions:
;;
;;   1. a raw FFI (foreign function interface),
;;   2. an easier to use lisp interface to the raw interface.
;;
;; The documentation given here relates in the most part to the
;; simplified lisp interface.
;;
;; Example:
;; ========
;; libblas.a contains the fortran subroutine DCOPY(N,X,INCX,Y,INCY)
;; which copies the vector Y of N double-float's to the vector X.
;; The function name in libblas.a is \"dcopy_\" (by Fortran convention).
;;
;; (DEF-FORTRAN-ROUTINE DCOPY :void 
;;   (N :integer :input)
;;   (X (* :double-float) :output)
;;   (INCX :integer :input)
;;   (Y (* :double-float) :input)
;;   (INCY :integer :input))
;;
;; will expand into:
;;
;; (CFFI:DEFCFUN ("dcopy_" FORTRAN-DCOPY) :VOID
;;   (N :POINTER :INT)
;;   (DX :POINTER :DOUBLE)
;;   (INCX :POINTER :INT)
;;   (DY :POINTER :DOUBLE)
;;   (INCY :POINTER :INT))
;;
;; and
;; 
;; (DEFUN DCOPY (N,X,INCX,Y,INCY)
;;    ...
;;
;; In turn, the lisp function DCOPY calls FORTRAN-DCOPY which calls
;; the Fortran function "dcopy_" in libblas.a.
;;
;; Arguments:
;; ==========
;;
;;
;; NAME    Name of the lisp interface function that will be created.
;;         The name of the raw FFI will be derived from NAME via
;;         the function MAKE-FFI-NAME.  The name of foreign function
;;         (presumable a Fortran Function in an external library) 
;;         will be derived from NAME via MAKE-FORTRAN-NAME.
;;
;; RETURN-TYPE
;;         The type of data that will be returned by the external
;;         (presumably Fortran) function.
;;       
;;             (MEMBER RETURN-TYPE '(:VOID :INTEGER :SINGLE-FLOAT :DOUBLE-FLOAT
;;                                   :COMPLEX-SINGLE-FLOAT :COMPLEX-DOUBLE-FLOAT))
;;
;;         See GET-READ-OUT-TYPE.
;;
;; BODY    A list of parameter forms.  A parameter form is:
;;
;;                  (VARIABLE TYPE &optional (STYLE :INPUT))
;;
;;         The VARIABLE is the name of a parameter accepted by the
;;         external (presumably Fortran) routine.  TYPE is the type of
;;         VARIABLE.  The recognized TYPE's are:
;;
;;                TYPE                    Corresponds to Fortran Declaration
;;                ----                    ----------------------------------
;;                :STRING                  CHARACTER*(*)
;;                :INTEGER                 INTEGER
;;                :SINGLE-FLOAT            REAL
;;                :DOUBLE-FLOAT            DOUBLE PRECISION
;;                :COMPLEX-SINGLE-FLOAT    COMPLEX
;;                :COMPLEX-DOUBLE-FLOAT    COMPLEX*16
;;                 (* X)                   An array of type X.
;;                (:CALLBACK args)         A description of a function or subroutine
;;
;;               (MEMBER X '(:INTEGER :SINGLE-FLOAT :DOUBLE-FLOAT
;;                           :COMPLEX-SINGLE-FLOAT :COMPLEX-DOUBLE-FLOAT)
;;
;;
;;         The STYLE (default :INPUT) defines how VARIABLE is treated.
;;         This is by far the most difficult quantity to learn.  To
;;         begin with:
;;
;;
;;                (OR (MEMBER STYLE '(:INPUT :OUTPUT :INPUT-OUTPUT))
;;                    (MEMBER STYLE '(:IN :COPY :IN-OUT :OUT)))
;;
;;            TYPE        STYLE             Description
;;            ----        -----             -----------
;;              X          :INPUT            Value will be used but not modified.
;;
;;                         :OUTPUT           Input value not used (but some value must be given),
;;                                           a value is returned as one of the values lisp
;;                                           function NAME.  Similar to the :IN-OUT style
;;                                           of DEF-ALIEN-ROUTINE.
;;                         :INPUT-OUTPUT     Input value may be used, a value is returned
;;                                           as one of the values from the lisp function
;;                                           NAME.
;;
;;           ** Note:  In all 3 cases above the input VARIABLE will not be destroyed
;;                     or modified directly, a copy is taken and a pointer of that
;;                     copy is passed to the (presumably Fortran) external routine.
;;
;;           (OR (* X)     :INPUT           Array entries are used but not modified.
;;               :STRING)  :OUTPUT          Array entries need not be initialized on input,
;;                                          but will be *modified*.  In addition, the array
;;                                          will be returned via the Lisp command VALUES
;;                                          from the lisp function NAME.
;;
;;                         :INPUT-OUTPUT    Like :OUTPUT but initial values on entry may be used.
;;              
;;         The keyword :WORKSPACE is a nickname for :INPUT.  The
;;         keywords :INPUT-OR-OUTPUT, :WORKSPACE-OUTPUT,
;;         :WORKSPACE-OR-OUTPUT are nicknames for :OUTPUT.
;;
;;         This is complicated.  Suggestions are encouraged to
;;         interface a *functional language* to a *pass-by-reference
;;         language*.
;;
;; CALLBACKS
;;
;; A callback here means a function (or subroutine) that is passed into the Fortran
;; routine which calls it as needed to compute something.
;;
;; The syntax of :CALLBACK is similar to the DEF-FORTRAN-ROUTINE:
;;
;;   (name (:CALLBACK return-type
;;            {arg-description}))
;;
;; The RETURN-TYPE is the same as for DEF-FORTRAN-ROUTINE.  The arg description is the
;; same syntax as list of parameter forms for DEF-FORTRAN-ROUTINE.  However, if the type
;; is a pointer type (like (* :double-float)), then a required keyword option must be
;; specified:
;;
;;   (name (* type :size size) &optional style)
;;
;; The size specifies the total length of the Fortran array.  This array is treated as a
;; one dimentionsal vector and should be accessed using the function FV-REF, which is
;; analogous to AREF.  The SIZE parameter can be any Lisp form and can refer to any of the
;; arguments to the Fortran routine.
;;
;; For example, a fortran routine can have the callback
;;
;;   (def-fortran-routine foo :void
;;     (m (* :integer) :input)
;;     (fsub (:callback :void
;;            (x :double-float :input)
;;            (z (* :double-float :size (aref m 0)) :input)
;;            (f (* :double-float :size (aref m 0)) :output)))))
;;
;; This means that the arrays Z and F in FSUB have a dimension of (AREF M 0), the first
;; element of the vector M.  The function FSUB can be written in Lisp as
;;
;;   (defun fsub (x z f)
;;     (setf (fv-ref f 0) (* x x (fv-ref z 3))))
;;
;; Further Notes:
;; ===============
;;
;; Some Fortran routines use Fortran character strings in the
;; parameter list.  The definition here is suitable for Solaris
;; where the Fortran character string is converted to a C-style null
;; terminated string, AND an extra hidden parameter that is appended
;; to the parameter list to hold the length of the string.
;;
;; If your Fortran does this differently, you'll have to change this
;; definition accordingly!

;; Call defcfun to define the foreign function.
;; Also creates a nice lisp helper function.
(defmacro def-fortran-routine (func-name return-type &rest body)
  (multiple-value-bind (fortran-name name) (if (listp func-name)
					       (values (car func-name) (cadr func-name))
					       (values (make-fortran-name func-name) func-name))
    (let* ((lisp-name  (make-fortran-ffi-name `,name))
	   (hack-return-type `,return-type)
	   (hack-body `(,@body))
	   (hidden-var-name nil))
      ;;
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

	   ;; (declaim (inline ,lisp-name)) ;sbcl 0.8.5 has problems with
	   (cffi:defcfun (,fortran-name ,lisp-name) ,@(get-return-type hack-return-type)
	     ,@(parse-fortran-parameters hack-body))
	   ,@(def-fortran-interface name hack-return-type hack-body hidden-var-name))))))

;; Create a form specifying a simple Lisp function that calls the
;; underlying Fortran routine of the same name.
(defun def-fortran-interface (name return-type body hidden-var-name)
  (multiple-value-bind (doc pars)
      (parse-doc-&-parameters body)
    (let ((ffi-fn (make-fortran-ffi-name name))
	  (return-vars nil)
	  (array-vars nil)
	  (ref-vars nil)
	  (callback-code nil)
	  ;;
	  (defun-args nil)
	  (defun-keyword-args nil)
	  ;;
	  (aux-args nil)
	  ;;
	  (ffi-args nil)
	  (aux-ffi-args nil))
      (dolist (decl pars)
	(destructuring-bind (var type &optional style) decl
	  (let ((ffi-var nil)
		(aux-var nil))
	    (cond
	      ;; Callbacks are tricky.
	      ((callback-type-p type)
	       (let* ((callback-name (gensym (symbol-name var)))
		      (c-callback-code (def-fortran-callback var callback-name (second type) (cddr type))))
		 (nconsc callback-code c-callback-code)
		 (setq ffi-var `(cffi:callback ,callback-name))))
	      ;; Can't really enforce "style" when given an array.
	      ;; Complex numbers do not latch onto this case, they
	      ;; are passed by value.
	      ((array-p type)
	       (setq ffi-var (scat "ADDR-" var))
	       (nconsc array-vars `((,ffi-var ,var)))
	       ;;
	       (when-let (arg (getf type :inc))
		 (nconsc defun-keyword-args
			 `((,arg 0)))
		 (nconc (car (last array-vars)) `(:inc-type ,(cadr type) :inc ,arg))))
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
	    (unless (eq var hidden-var-name)
	      (nconsc defun-args
		      `(,var)))
	    ;; Arguments for the FFI function
	    (nconsc ffi-args
		    `(,ffi-var))
	    ;; Auxillary arguments for FFI
	    (unless (null aux-var)
	      (nconsc aux-ffi-args
		      `(,aux-var))))))
      ;;Complex returns through hidden variable.
      (unless (null hidden-var-name)
	(nconsc aux-args `((,hidden-var-name ,(ecase (second (first pars))
						     (:complex-single-float #c(0e0 0e0))
						     (:complex-double-float #c(0d0 0d0)))))))
      ;;Keyword argument list
      (unless (null defun-keyword-args)
	(setq defun-keyword-args (cons '&key defun-keyword-args)))
      ;;Return the function definition
      (let ((retvar (gensym)))
	`(
	  ,(recursive-append
	    `(defun ,name ,(append defun-args defun-keyword-args)
	       ,@doc)
	    ;;
	    (unless (null aux-args)
	      `(let (,@aux-args)))
	    ;;Don't use with-foreign.. if ref-vars is nil
	    (unless (null ref-vars)
	      `(with-foreign-objects-stacked (,@ref-vars)))
	    ;;Don't use with-vector-dat.. if array-vars is nil
	    (unless (null array-vars)
	      `(with-vector-data-addresses (,@array-vars)))
	    ;;Declare callbacks
	    callback-code
	    ;;Call the foreign-function
	    `(let ((,retvar (,ffi-fn ,@ffi-args ,@aux-ffi-args)))
	       ;;Ignore return if type is :void
	       ,@(when (eq return-type :void)
		       `((declare (ignore ,retvar))))
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
		,@(unless (eq return-type :void)
			  `(,retvar))
		,@(mapcar #'second return-vars)))))))))


(defun def-fortran-callback (func callback-name return-type parm)
  (let* ((hack-return-type `,return-type)
	 (hack-parm `(,@parm))
	 (hidden-var-name nil))
    ;;
    (when (member hack-return-type '(:complex-single-float :complex-double-float))
      (setq hidden-var-name (gensym "HIDDEN-COMPLEX-RETURN-"))
      (setq hack-parm `((,hidden-var-name ,hack-return-type :output)
			,@parm))
      (setq hack-return-type :void))
    ;;
    (let* ((new-pars nil)
	   (aux-pars nil)
	   (func-pars nil)
	   (array-vars nil)
	   (return-vars nil)
	   (ref-vars nil))
      (dolist (decl hack-parm)
	(destructuring-bind (var type &optional (style :input)) decl
	  (let ((ffi-var nil)
		(func-var nil))
	    (cond
	      ;; Callbacks are tricky.
	      ((callback-type-p type)
	       (setq ffi-var var)
	       (setq func-var var))
	      ;;
	      ((array-p type)
	       (setq ffi-var (scat "ADDR-" var))
	       (setq func-var var)
	       (nconsc array-vars `((,func-var (make-foreign-vector :pointer ,ffi-var :type ,(second (->cffi-type type))
								    :size ,(if-let (size (getf type :size))
										   size
										   1))))))
	      ;;
	      ((string-p type)
	       (setq ffi-var var)
	       (setq func-var var)
	       (nconsc aux-pars
		       `((,(scat "LEN-" var) ,@(->cffi-type :integer)))))
	      ;;
	      ((eq style :input-value)
	       (setq ffi-var var)
	       (setq func-var var))
	      ;; Pass-by-reference variables
	      (t
	       (cond
		 ((member type '(:complex-single-float :complex-double-float))
		  (setq ffi-var (scat "ADDR-REAL-CAST-" var))
		  (setq func-var var)
		  (nconsc ref-vars
			  `((,func-var (complex (cffi:mem-aref ,ffi-var ,(second (->cffi-type type)) 0)
						(cffi:mem-aref ,ffi-var ,(second (->cffi-type type)) 1))))))
		 (t
		  (setq ffi-var (scat "REF-" var))
		  (setq func-var var)
		  (nconsc ref-vars
			  `((,func-var (cffi:mem-aref ,ffi-var ,@(->cffi-type type)))))))))
	    ;;
	    (nconsc new-pars `((,ffi-var ,@(get-read-in-type type style))))
	    (nconsc func-pars `(,func-var))
	    (when (and (output-p style) (not (eq type :string)))
	      (nconsc return-vars
		      `((,func-var ,ffi-var ,type)))))))
      
      (let ((retvar (gensym)))
	`(
	  ,(recursive-append
	    `(cffi:defcallback ,callback-name ,@(get-return-type hack-return-type)
		 (,@new-pars ,@aux-pars))
	    ;;
	    (when ref-vars
	      `(let (,@ref-vars)))
	    ;;
	    (when array-vars
	      `(let (,@array-vars)))
	    ;;
	    `(multiple-value-bind (,retvar ,@(mapcar #'car return-vars)) (funcall ,func ,@func-pars)
	       ,@(when (eq hack-return-type :void)
		       `((declare (ignore ,retvar))))
	       ,@(mapcar #'(lambda (decl)
			     (destructuring-bind (func-var ffi-var type) decl
			       (if (member type '(:complex-single-float :complex-double-float))
				   `(setf (cffi:mem-aref ,ffi-var ,(second (->cffi-type type)) 0) (realpart ,func-var)
					  (cffi:mem-aref ,ffi-var ,(second (->cffi-type type)) 1) (imagpart ,func-var))
				   `(setf (cffi:mem-aref ,ffi-var ,@(->cffi-type type)) ,func-var))))
			 (remove-if-not #'(lambda (x)
					(member (first x) ref-vars :key #'car))
				    return-vars))
	       ,(if (eq hack-return-type :void)
		     nil
		     retvar))))))))

;; Increment the pointer.
(defun inc-sap (sap type &optional (n 1))
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
       (simple-array (signed-byte 16) *)
       (simple-array (signed-byte 8) *)
       ;;
       (simple-array (unsigned-byte 64) *)
       (simple-array (unsigned-byte 32) *)
       (simple-array (unsigned-byte 16) *)
       (simple-array (unsigned-byte 8) *)
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
