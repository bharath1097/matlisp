(in-package #:matlisp-ffi)

(eval-when (:compile-toplevel :load-toplevel :execute)
;;
(definline %ffv-types (x)
  (member (fv-type x) '(:double :float :int32 :int64)))

(deftype %ffv* ()
  `(or cffi:foreign-pointer (and foreign-vector (satisfies allowed-fv-type?))))

;;(simple-array (or single-float double-float (signed-byte 32) (signed-byte 64)) (*))
;;
(defun %ffc->cffi (type)
  "Convert the given matlisp-ffi type into one understood by CFFI."
  (if (consp type)
      (ecase (first type)
	(:& (ecase (second type)
	      (:complex-single-float `(:pointer ,(%ffc->cffi :single-float)))
	      (:complex-double-float `(:pointer ,(%ffc->cffi :double-float)))	      
	      ((:double-float :single-float :character :integer :long)
	       `(:pointer ,(%ffc->cffi (second type))))))
	(:* (ecase (second type)
	      (:complex-single-float `(:pointer ,(%ffc->cffi :single-float)))
	      (:complex-double-float `(:pointer ,(%ffc->cffi :double-float)))
	      ((:double-float :single-float :integer :long)
	       `(:pointer ,(%ffc->cffi (second type)))))))
      (ecase type
	(:double-float :double)
	(:single-float :single)
	(:character :char)
	(:string :string)
	(:integer :int32)
	(:long :int64)
	((:* :callback) `(:pointer :void)))))

(defun %ffc->lisp (type)
  "Convert the given matlisp-ffi type into one understood by Lisp"
  (if (consp type)
      (ecase (first type)
	(:& (ecase (second type)
	      (:complex-single-float `(complex single-float))
	      (:complex-double-float `(complex double-float))
	      ((:double-float :single-float :character :integer :long)
	       (%ffc->lisp (second type)))))
	(:* (ecase (second type)
	      (:complex-single-float `(simple-array single-float (*)))
	      (:complex-double-float `(simple-array double-float (*)))
	      ((:double-float :single-float :integer :long)
	       `(simple-array ,(%ffc->lisp (second type)) (*))))))
      (ecase type
	(:double-float 'double-float)
	(:single-float 'single-float)
	(:character 'character)
	(:string 'string)
	(:integer '(signed-byte 32))
	(:long '(signed-byte 64))
	(:* 'cffi:foreign-pointer)
	(:callback 'symbol))))

(defun lisp->ffc (type &optional refp)
  "Convert the given matlisp-ffi type into one understood by Lisp"
  (cond
    ((eq type 'cl:single-float) :single-float)
    ((eq type 'cl:double-float) :double-float)
    ((eq type 'cl:character) :character)
    ((eq type 'cl:string) :string)
    ((tree-equal type '(cl:complex cl:single-float)) (if refp :complex-single-float :single-float))
    ((tree-equal type '(cl:complex cl:double-float)) (if refp :complex-double-float :double-float))
    ((tree-equal type '(cl:signed-byte 32)) :integer)
    ((tree-equal type '(cl:signed-byte 64)) :long)))

;; type -> Pass by value
;; (:& type &key output) -> Pass by reference, if 'output' return value after exiting from foreign function.
;; (:* type &key +) -> Pointer/Array/Foreign-vector, if '+' increment pointer by '+' times foreign-type-size.
(defun %ffc.parse-ffargs (args &optional append-string-length?)
  (loop :for (type expr) :on args :by #'cddr
     :collect
     (list* :cffi
	    (let ((ctype (%ffc->cffi type)))
	      (if (consp ctype) (first ctype) ctype))
	    (etypecase type
	      (symbol (case type
			(:callback (list :argument `(cffi-sys:%callback (the ,(%ffc->lisp type) ,expr))))
			(:string (if append-string-length?
				     (with-gensyms (s)
				       (list  :argument s :let-bind `(,s ,expr :type ,(%ffc->lisp type))
					      :aux `(:int32 (the fixnum (length ,s)))))
				     (list :argument `(the ,(%ffc->lisp type) ,expr))))
			(t (list :argument `(the ,(%ffc->lisp type) ,expr)))))
	      (cons (ecase (first type)
		      (:& (destructuring-bind (tok sub-type &optional output) type
			    (declare (ignore tok))
			    (when output (assert (eq output :output)) nil "unknown token.")
			    (ecase sub-type
			      ((:complex-double-float :complex-single-float)
			       (let ((utype (second (%ffc->cffi type))))
				 (with-gensyms (var c)
				   (list :argument `(the cffi:foreign-pointer ,var)
					 :alloc `(,var ,utype :count 2)
					 :init `(let-typed ((,c ,expr :type ,(%ffc->lisp type)))
						  (setf (cffi:mem-aref ,var ,utype 0) (realpart ,c)
							(cffi:mem-aref ,var ,utype 1) (imagpart ,c)))
					 :output (when output
						   `(complex (cffi:mem-aref ,var ,utype 0) (cffi:mem-aref ,var ,utype 1)))))))			      
			      ((:double-float :single-float :character :integer :long)
			       (let ((utype (second (%ffc->cffi type))))
				 (with-gensyms (var)
				   (list :argument `(the cffi:foreign-pointer ,var)
					 :alloc `(,var ,utype :initial-element ,(recursive-append
										 (when (eq sub-type :character) `(char-code))
										 `(the ,(%ffc->lisp type) ,expr)))
					 :output (when output `(cffi:mem-ref ,var ,utype)))))))))
		      (:* (destructuring-bind (tok sub-type &key +) type
			    (declare (ignore tok))
			    (with-gensyms (vec)
			      (list :argument (let ((ptr `(etypecase ,vec
							    (,(%ffc->lisp type) (vector-sap-interpreter-specific ,vec))
							    (cffi:foreign-pointer ,vec)
							    (foreign-vector (fv-pointer ,vec)))))
						(if +
						    `(cffi:inc-pointer ,ptr (* (the fixnum ,+)
									       ,(ecase sub-type
										       (:complex-single-float (* 2 (cffi:foreign-type-size (%ffc->cffi :single-float))))
										       (:complex-double-float (* 2 (cffi:foreign-type-size (%ffc->cffi :double-float))))
										       ((:double-float :single-float :integer :long) (cffi:foreign-type-size (%ffc->cffi sub-type))))))
						    ptr))
				    :let-bind `(,vec ,expr ,@(when (and (consp expr) (eq (first expr) 'cl:the)) `(:type ,(second expr))))))))))))))

;;
(define-constant +f77-name-mangler+
    (find-if #'(lambda (f) (cffi:foreign-symbol-pointer (funcall f "ddot")))
	     (mapcart #'(lambda (a b) (compile-and-eval `(lambda (x) ,(subst b 'x a))))
		      '((string-upcase x) (string-downcase x)) '((id x) (string+ x "_") (string+ x "__")))))

(defmacro ffuncall (name-&-return-type &rest args)
  "This macro provides a thin wrapper around cffi:foreign-funcall for making calls to Fortran functions
that much easier. We use the F2C convention, which isn't really followed by compilers when returning
complex values (so be wary of complex number returning functions). 

 (FFUNCALL (name &optional (return-type :void) (mode :f2c)) *[type arg])

If (eq mode :f2c) then a name mangler is called on name, and string lengths are appended at the
end of the argument to the foreign function. Neither of this is done if (eq mode :c).

Type (credits for aesthetics goes to CCL) is of the general form:
 type -> Pass by value.
 (:& type &key output) -> Pass by reference, if 'output' return value after exiting from foreign function.
 (:* type &key +) -> Pointer/Array/Foreign-vector, if '+' increment pointer by '+' times foreign-type-size.
There are restrictions as to what types can be used with '(:& :*), see source of %ffc->cffi and %ffc->lisp.

Example:
@lisp
> (let ((a (make-array 4 :element-type 'double-float :initial-contents '(1d0 2d0 3d0 4d0))))
    (ffuncall (\"zdotc\" :double-float) (:& :integer) (/ (length a) 2)
	      (:* :complex-double-float) a (:& :integer) 1
	      (:* :complex-double-float) a (:& :integer) 1))
=> 30d0
> (let ((a (make-array 4 :element-type 'double-float :initial-contents '(1d0 2d0 3d0 4d0))))
    (ffuncall (\"ddot\" :double-float) (:& :integer) (length a)
	      (:* :double-float) a (:& :integer) 1
	      (:* :double-float) a (:& :integer) 1))
=> 30d0
@end lisp
"
  (destructuring-bind (name &optional (return-type :void) (mode :f2c)) (ensure-list name-&-return-type)
    (if (member return-type '(:complex-single-float :complex-double-float))
	`(ffuncall (,name :void ,mode) (:& ,return-type :output) ,(coerce #c(0 0) (%ffc->lisp `(:& ,return-type))) ,@args)
	(let ((pargs (%ffc.parse-ffargs args (when (eq mode :f2c) t))))
	  (labels ((mapf (place) (remove-if #'null (mapcar #'(lambda (x) (getf x place)) pargs))))
	    `(with-fortran-float-modes
	       (without-gcing
		 ,(recursive-append
		   (when-let (bd (mapf :let-bind))
		     `(let-typed (,@bd)))
		   (when-let (al (mapf :alloc))
		     `(with-foreign-objects-stacked (,@al)))
		   (when-let (init (mapf :init))
		     `(progn ,@init))
		   (let ((callc `(cffi-sys:%foreign-funcall ,(ecase mode
								    (:f2c (funcall +f77-name-mangler+ name))
								    (:c name))
							    (,@(apply #'append (zip (mapf :cffi) (mapf :argument))) ,@(apply #'append (mapf :aux)) ,(if (eq return-type :void) :void (first (ensure-list (%ffc->cffi return-type))))))))
		     (if (eq return-type :void)
			 `(progn ,callc (values ,@(mapf :output)))
			 (with-gensyms (ret)
			   `(let ((,ret ,callc))
			      (values ,ret ,@(mapf :output))))))))))))))
)


