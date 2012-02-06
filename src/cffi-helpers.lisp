;;
(in-package "FORTRAN-FFI-ACCESSORS")

(defmacro with-gensyms (symlist &body body)
  `(let ,(mapcar #'(lambda (sym)
		      `(,sym (gensym ,(symbol-name sym))))
		  symlist)
    ,@body))

;;

(defmacro econd (&rest clause)
  `(cond
     ,@clause
     (t (error "Fell through the ECOND expression."))))

;; Create objects on the heap and run some stuff.
(defmacro with-foreign-objects-heap-ed (declarations &rest body)
  (let ((ret (gensym)))
    `(let
	 ;; Allocate C objects
	 ,(mapcar (lambda (decl) (list (car decl) `(cffi:foreign-alloc ,@(cdr decl))))
		  declarations)
       ;; Store result temporarily
       (let ((,ret
	      (progn ,@body)))
	 
	 ;;Free C objects
	 (progn
	   ,@(mapcar (lambda (decl) `(cffi:foreign-free ,(car decl)))
		     declarations))
	 ,ret))))

;; Create objects on the stack and run the "body" of code.
(defmacro with-foreign-objects-stack-ed (declarations &rest body)
"with-foreign-objects-stack-ed (declarations) &rest body
  binding := {(var type &optional count &key (initial-contents nil))}*
"
  (if (null declarations)
      `(progn ,@body)
      (let ((decl-count (gensym))
	    (decl-init (gensym))
	    (loop-var (gensym)))
	(destructuring-bind (var type &key (count 1) initial-element initial-contents)
	    (car declarations)
	  ;;Make sure the var and type are symbols;;
	  (cond
	    ((not (symbolp var)) (error "Variable: ~S, is not a symbol." var))
	    ((not (symbolp type)) (error "Type: ~S, is not a symbol." type))
	    ((and initial-element initial-contents) (error "Cannot apply both :initial-element and :initial-contents at the same time")))
	  `(let ((,decl-count ,count)
		 (,decl-init ,(or initial-element initial-contents)))
	     (cffi:with-foreign-object (,var ,type ,decl-count)
	       ,@(if initial-element
		     `((loop for ,loop-var from 0 below ,decl-count
			     do (setf (cffi:mem-aref ,var ,type ,loop-var) ,decl-init))))
	       ,@(if initial-contents
		     `((loop for ,loop-var from 0 below ,decl-count
			     do (setf (cffi:mem-aref ,var ,type ,loop-var) (elt ,decl-init ,loop-var)))))
	       (with-foreign-objects-stack-ed ,(cdr declarations) ,@body)))))))
;;