(in-package #:matlisp-utilities)
  
;;TODO: cleanup!
(defmacro mlet* (decls &rest body)
"
  mlet* ({ {(var*) | var} values-form &keyform declare type}*) form*

  o var is just one symbol -> expands into let
  o var is a list -> expands into multiple-value-bind

  This macro also handles type declarations.

  Example:
  > (mlet* ((x 2 :type fixnum :declare ((optimize (safety 0) (speed 3))))
           ((a b) (floor 3) :type (nil fixnum)))
       (+ x b))

  expands into:

  > (let ((x 2))
      (declare (optimize (safety 0) (speed 3))
               (type fixnum x))
      (multiple-value-bind (a b)
          (floor 3)
        (declare (ignore a)
                 (type fixnum b))
        (+ x b)))
"
  (labels ((mlet-decl (vars type decls)
	     (when (or type decls)
	       `((declare ,@decls
			  ,@(when type
				  (mapcar #'(lambda (tv) (if (null (first tv))
							     `(ignore ,(second tv))
							     `(type ,(first tv) ,(second tv))))
					  (map 'list #'list type vars)))))))
	   (mlet-transform (elst nest-code)
	     (destructuring-bind (vars form &key declare type) elst
	       `(,(append (cond
			    ;;If there is only one element use let
			    ;;instead of multiple-value-bind
			    ((or (symbolp vars))
			     `(let ((,vars ,form))))
			    (t
			     `(multiple-value-bind (,@vars) ,form)))
			  (if (symbolp vars)
			      (mlet-decl (list vars) (list type) declare)
			      (mlet-decl vars type declare))
			  nest-code))))
	   (mlet-walk (elst body)
	     (if (null elst)
		 `(,@body)
		 (mlet-transform (car elst) (mlet-walk (cdr elst) body)))))
    (if decls
	(car (mlet-walk decls body))
	`(progn
	   ,@body))))

(defmacro let-typed (bindings &rest body)
"
  let-typed ({var form &key type}*) form*

  This macro also handles type declarations.

  Example:
  > (let-typed ((x 1 :type fixnum))
       (+ 1 x))

  expands into:

  > (let ((x 1))
      (declare (type fixnum x))
      (+ 1 x))
"
  (labels ((parse-bindings (bdng let-decl type-decl)
	     (if (null bdng) (values (reverse let-decl) (reverse type-decl))
		 ;;Unless the user gives a initialisation form, no point declaring type
		 ;; {var is bound to nil}.
		 (destructuring-bind (var &optional form &key (type nil)) (ensure-list (car bdng))
		   (parse-bindings (cdr bdng)
				   (cons (if form `(,var ,form) var) let-decl)
				   (if type
				       (cons `(type ,type ,var) type-decl)
				       type-decl))))))
    (multiple-value-bind (let-bdng type-decl) (parse-bindings bindings nil nil)
      (let ((decl-code (recursive-append
			(cond
			  ((and (consp (first body))
				(eq (caar body) 'declare))
			   (first body))
			  ((consp type-decl)
			   '(declare ))
			  (t nil))
			type-decl)))
      `(let (,@let-bdng)
	 ,@(if (null decl-code) nil `(,decl-code))
	 ,@(if (and (consp (first body))
		    (eq (caar body) 'declare))
	       (cdr body)
	       body))))))

(defmacro let-rec (name arglist &rest code)
"
  (let-rec name ({var [init-form]}*) declaration* form*) => result*
  Works similar to \"let\" in Scheme.

  Example:
  > (let-rec rev ((x '(1 2 3 4)) (ret nil))
        (if (null x) ret
   	  (rev (cdr x) (cons (car x) ret))))
"
  (let ((init (mapcar #'second arglist))
	(args (mapcar #'first arglist)))
    `(labels ((,name (,@args)
		,@code))
       (,name ,@init))))

(defmacro with-gensyms (symlist &body body)
"
  (with-gensyms (var *) form*)
  Binds every variable in SYMLIST to a gensym."
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

(defun pop-arg! (arglist sym)
  (check-type sym symbol)
  (labels ((get-sym (sym arglist prev)
	     (cond
	       ((null arglist) nil)
	       ((eq (car arglist) sym) (prog1
					   (cadr arglist)
					 (if prev
					     (rplacd prev (cddr arglist)))))
	       (t (get-sym sym (cdr arglist) arglist)))))
    (get-sym sym arglist nil)))

(defun slot-values (obj slots)
  (values-list (mapcar #'(lambda (slt) (slot-value obj slt))
		       slots)))

(declaim (inline linear-array-type))
(defun linear-array-type (type-sym &optional (size '*))
  `(simple-array ,type-sym (,size)))

(declaim (inline ensure-list))
(defun ensure-list (lst)
  (if (listp lst)
      lst
      `(,lst)))

(defmacro if-ret (form &rest else-body)
"
  if-ret (form &rest else-body)
  Evaluate form, and if the form is not nil, then return it,
  else run else-body"
  (let ((ret (gensym)))
    `(let ((,ret ,form))
       (or ,ret
	   (progn
	     ,@else-body)))))

;;
(defmacro when-let ((var . form) &rest body)
  (check-type var symbol)
  `(let ((,var ,@form))
     (when ,var
       ,@body)))

(defmacro if-let ((var . form) &rest body)
  (check-type var symbol)
  `(let ((,var ,@form))      
     (if ,var
	 ,@body)))

;;
(defun cut-cons-chain! (lst test)
  (check-type lst cons)
  (labels ((cut-cons-chain-tin (lst test parent-lst)
	     (cond
	       ((null lst) nil)
	       ((funcall test (cadr lst))
		(let ((keys (cdr lst)))
		  (setf (cdr lst) nil)
		  (values parent-lst keys)))
	       (t (cut-cons-chain-tin (cdr lst) test parent-lst)))))
    (cut-cons-chain-tin lst test lst)))

;;
(defun zip (&rest args)
  (apply #'map 'list #'list args))

;;
(defmacro mcase (keyform &rest body)
  (labels ((app-equal (lst)
	     (if (null lst)
		 nil
		 `(((and ,@(mapcar (lambda (pair) (cons 'eq pair))
				   (zip keyform (caar lst))))
		    ,@(cdar lst))
		   ,@(app-equal (cdr lst))))))
    `(cond
       ,@(app-equal body))))

(defmacro zip-eq (a b)
  `(and ,@(mapcar (lambda (pair) (cons 'eq pair))
		  (zip (ensure-list a) (ensure-list b)))))

(defun recursive-append (&rest lsts)  
  (labels ((bin-append (x y)
	     (if (null x)
		 (if (typep (car y) 'symbol)
		     y
		     (car y))
		 (append x (if (null y)
			       nil
			       (if (typep (car y) 'symbol)
				   `(,y)
				   y))))))
    (if (null lsts)
	nil
	(bin-append (car lsts) (apply #'recursive-append (cdr lsts))))))

(defun unquote-args (lst args)
  "
  Makes list suitable for use inside macros (sort-of).
  Example:
  > (unquote-args '(+ x y z) '(x y))
  (LIST '+ X Y 'Z)

  DO NOT use backquotes!
  "
  (labels ((replace-atoms (lst ret)
	     (cond
	       ((null lst) (reverse ret))
	       ((atom lst)
		(let ((ret (reverse ret)))
		  (rplacd (last ret) lst)
		  ret))
	       ((consp lst)
		(replace-atoms (cdr lst) (let ((fst (car lst)))
					   (cond 
					     ((atom fst)
					      (if (member fst args)
						  (cons fst ret)
						  (append `(',fst) ret)))
					     ((consp fst)
					      (cons (replace-lst fst nil) ret))))))))
	   (replace-lst (lst acc)
	     (cond
	       ((null lst) acc)
	       ((consp lst)
		(if (eq (car lst) 'quote)
		    lst
		    (cons 'list (replace-atoms lst nil))))
	       ((atom lst) lst))))
    (replace-lst lst nil)))

(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec
		       (car x)
		       (rec (cdr x) acc))))))
    (rec x nil)))

(defmacro macrofy (lambda-func)
  "
  Macrofies a lambda function, for use later inside macros (or for symbolic math ?).
  Example:
  > (macroexpand-1 `(macrofy (lambda (x y z) (+ (sin x) y (apply #'cos (list z))))))
  (LAMBDA (X Y Z)
    (LIST '+ (LIST 'SIN X) Y (LIST 'APPLY (LIST 'FUNCTION 'COS) (LIST 'LIST Z))))
  T
  > (funcall (macrofy (lambda (x y z) (+ (sin x) y (apply #'cos (list z))))) 'a 'b 'c)
  (+ (SIN A) B (APPLY #'COS (LIST C)))

  DO NOT USE backquotes in the lambda function!
  "
  (destructuring-bind (labd args &rest body) lambda-func
    (assert (eq labd 'lambda))
    `(lambda ,args ,@(cdr (unquote-args body args)))))

(defmacro looped-mapcar ((func lst) &rest body)
  "
  A macro to use when caught between the efficiency of imperative looping, and
  the elegance of mapcar (in a dozen places).

  Collects references to func and replaces them with a varible inside a loop.
  Note that although we traverse through the list only once, the collected lists
  aren't freed until the macro is closed.

  Example:
  > (macroexpand-1
      `(looped-mapcar (lmap '(1 2 3 4 5 6 7 8 9 10))
			(cons (lmap #'even) (lmap #'(lambda (x) (+ x 1))))))
  (LET ((#:|lst1118| '(1 2 3 4 5 6 7 8 9 10)))
    (LOOP FOR #:|ele1117| IN #:|lst1118|
        COLLECT (FUNCALL #'(LAMBDA (X) (+ X 1))
                         #:|ele1117|) INTO #:|collect1116|
        COLLECT (FUNCALL #'EVEN #:|ele1117|) INTO #:|collect1115|
        FINALLY (RETURN (PROGN (CONS #:|collect1115| #:|collect1116|)))))
  "
  (let ((ret nil))
    (labels ((collect-funcs (code tf-code)
	       (cond
		 ((null code)
		  (reverse tf-code))
		 ((atom code)
		  (let ((ret (reverse tf-code)))
		    (rplacd (last ret) code)
		    ret))
		 ((consp code)
		  (let ((carcode (car code)))
		    (cond
		      ((and (consp carcode)
			    (eq (first carcode) func))
		       (assert (null (cddr carcode)) nil 'invalid-arguments
			       :message "The mapper only takes one argument.")
		       (let ((col-sym (gensym "collect")))
			 (push `(,col-sym ,(second carcode)) ret)
			 (collect-funcs (cdr code) (cons col-sym tf-code))))
		      ((consp carcode)
		       (collect-funcs (cdr code) (cons (collect-funcs carcode nil) tf-code)))
		      (t
		       (collect-funcs (cdr code) (cons carcode tf-code)))))))))
      (let ((tf-code (collect-funcs body nil))
	    (ele-sym (gensym "ele"))
	    (lst-sym (gensym "lst")))
	(if (null ret)
	    `(progn
	       ,@tf-code)
	    `(let ((,lst-sym ,lst))
	       (loop for ,ele-sym in ,lst-sym
		    ,@(loop for decl in ret
			 append `(collect (funcall ,(second decl) ,ele-sym) into ,(first decl)))
		  finally (return
			    (progn
			      ,@tf-code)))))))))

(declaim (inline string+))
(defun string+ (&rest strings)
  (apply #'concatenate (cons 'string strings)))

(defun format-to-string (fmt &rest args)
  (let ((ret (make-array 0 :element-type 'character :adjustable t :fill-pointer t)))
    (with-output-to-string (ostr ret)
      (apply #'format (append `(,ostr ,fmt) args)))
    ret))

(defun list-dimensions (lst)
  (declare (type list lst))
  (labels ((lst-tread (idx lst)
	     (if (null lst) (reverse idx)
		 (progn
		   (setf (car idx) (length lst))
		   (if (consp (car lst))
		       (lst-tread (cons 0 idx) (car lst))
		       (reverse idx))))))
    (lst-tread (list 0) lst)))

(defmacro define-constant (name value &optional doc)
  "
  Keeps the lisp implementation from defining constants twice.
  "
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(defmacro inlining (&rest definitions)
  "Declaims the following definitions inline together with executing them."
  `(progn ,@(loop for def in definitions when (eq (first def) 'defun) collect
		  `(declaim (inline ,(second def))) collect def)))

(defmacro definline (name &rest rest)
  "Short form for defining an inlined function.  It should probably be
deprecated, because it won't be recognized by default by some IDEs.  Better
use the inlining macro directly."
  `(inlining (defun ,name ,@rest)))

;;TODO: Add general permutation support for currying, and composition.
(inlining
 (defun curry (func &rest args)
   "Supplies @arg{args} to @arg{func} from the left."
   #'(lambda (&rest after-args)
       (apply func (append args after-args)))))

;;---------------------------------------------------------------;;
;; Optimization
;;---------------------------------------------------------------;;
;;TODO: Figure out a way of adding \"#+lispworks (float 0)\"
(defmacro with-optimization ((&key speed safety space debug) &body forms)
  "with-optimization (&key speed safety space debug) declaration* form*
   Macro creates a local environment with optimization declarations, and
   executes form*"
  (mapcar #'(lambda (x) (assert (member x '(nil 0 1 2 3))))
	  (list speed safety space debug))
  `(locally
       ,(recursive-append
	 `(declare ,(append `(optimize)
			    (when speed
			      `((speed ,speed)))
			    (when safety
			      `((safety ,safety)))
			    (when space
			      `((space ,space)))
			    (when debug
			      `((debug ,debug)))))
	 (when (eq (caar forms) 'declare)
	   (cdar forms)))
     ,@(if (eq (caar forms) 'declare) (cdr forms) forms)))

(defmacro quickly (&body forms)
  `(with-optimization (:speed 3)
     ,@forms))

(defmacro very-quickly (&body forms)
  `(with-optimization (:safety 0 :space 0 :speed 3)
     ,@forms))

(defmacro slowly (&body forms)
  `(with-optimization (:speed 1)
     ,@forms))

(defmacro quickly-if (test &body forms)
  `(if ,test ;runtime test
       (quickly ,@forms)
        (progn ,@forms)))
;;---------------------------------------------------------------;;

