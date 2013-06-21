(in-package #:matlisp-utilities)

(eval-when (:compile-toplevel :load-toplevel :execute)
;;Note to self: do not indent!
  
(defmacro define-constant (name value &optional doc)
  "
  Keeps the lisp implementation from defining constants twice.
  "
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(defmacro mlet* (vars &rest body)
  "
  This macro extends the syntax of let* to handle multiple values, it also handles
  type declarations. The declarations list @arg{vars} is similar to that in let: look
  at the below examples.

  Examples:
  @lisp
  > (macroexpand-1
       `(mlet* ((x 2 :type fixnum :declare ((optimize (safety 0) (speed 3))))
                ((a b) (floor 3) :type (nil fixnum)))
           (+ x b)))
  => (LET ((X 2))
       (DECLARE (OPTIMIZE (SAFETY 0) (SPEED 3))
                (TYPE FIXNUM X))
       (MULTIPLE-VALUE-BIND (A B)
          (FLOOR 3)
          (DECLARE (IGNORE A)
                   (TYPE FIXNUM B))
          (+ X B)))
  @end lisp
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
			      (mlet-decl (list vars) (when type (list type)) declare)
			      (mlet-decl vars type declare))
			  nest-code))))
	   (mlet-walk (elst body)
	     (if (null elst)
		 `(,@body)
		 (mlet-transform (car elst) (mlet-walk (cdr elst) body)))))
    (if vars
	(car (mlet-walk vars body))
	`(progn
	   ,@body))))

(defmacro make-array-allocator (allocator-name type init &optional doc)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (definline ,allocator-name (size &optional (initial-element ,init))
       ,@(unless (null doc)
		 `(,doc))
       (make-array size
		   :element-type ,type :initial-element initial-element))))

(defmacro let-typed (bindings &rest body)
  "
  This macro works basically like let, but also allows type-declarations
  with the key :type.

  Example:
  @lisp
  > (macroexpand-1
      `(let-typed ((x 1 :type fixnum))
          (+ 1 x)))
  => (LET ((X 1))
        (DECLARE (TYPE FIXNUM X))
        (+ 1 X))
  @end lisp
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

(defmacro let*-typed (bindings &rest body)
  "
  This macro works basically like let*, but also allows type-declarations
  with the key :type.

  Example:
  @lisp
  > (macroexpand-1
      `(let*-typed ((x 1 :type fixnum))
          (+ 1 x)))
  => (LET* ((X 1))
        (DECLARE (TYPE FIXNUM X))
        (+ 1 X))
  @end lisp
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
      `(let* (,@let-bdng)
	 ,@(if (null decl-code) nil `(,decl-code))
	 ,@(if (and (consp (first body))
		    (eq (caar body) 'declare))
	       (cdr body)
	       body))))))

(defmacro let-rec (name arglist &rest code)
  "
  This works implements the named let used in Scheme for recursion
  using labels.

  Example:
  @lisp
  > (macroexpand-1
      `(let-rec rev ((x '(1 2 3 4)) (ret nil))
         (if (null x) ret
           (rev (cdr x) (cons (car x) ret)))))
  => (LABELS ((REV (X RET)
                (IF (NULL X)
                   RET
                  (REV (CDR X) (CONS (CAR X) RET)))))
       (REV '(1 2 3 4) NIL))

  > (let-rec rev ((x '(1 2 3 4)) (ret nil))
       (if (null x) ret
          (rev (cdr x) (cons (car x) ret))))
  => (4 3 2 1)
  @end lisp
  "
  (let ((init (mapcar #'second arglist))
	(args (mapcar #'first arglist)))
    `(labels ((,name (,@args)
		,@code))
       (,name ,@init))))

(defmacro with-gensyms (symlist &body body)
  "
  Binds every variable in @arg{symlist} to a (gensym).

  Example:
  @lisp
  > (macroexpand-1
       `(with-gensyms (a b c)
           `(let ((,a 1) (,b 2) (,c 3))
                 (+ ,a ,b ,c))))
  => (LET ((A (GENSYM \"A\")) (B (GENSYM \"B\")) (C (GENSYM \"C\")))
      `(LET ((,A 1) (,B 2) (,C 3))
          (+ ,A ,B ,C)))
  @end lisp
  "
  `(let ,(mapcar #'(lambda (sym)
		     `(,sym (gensym ,(symbol-name sym))))
		 symlist)
     ,@body))

(defmacro using-gensyms ((decl (&rest syms)) &rest body)
  `(let ((,decl (zipsym (list ,@syms))))
     (destructuring-bind (,@syms) (mapcar #'car ,decl)
       ,@body)))

(defmacro nconsc (var &rest args)
  "
  Macro to do setf and nconc for destructive list updates. If @arg{var}
  is null then @arg{var} is set to (apply #'nconc @arg{args}), else
  does (apply #'nconc (cons @arg{var} @arg{args})).

  Example:
  @lisp
  > (let ((x nil))
      (nconsc x (list 1 2 3) (list 'a 'b 'c))
      x)
  => (1 2 3 A B C)

  > (let ((x (list 'a 'b 'c)))
      (nconsc x (list 1 2 3))
       x)
  => (A B C 1 2 3)  
  @end lisp
  "
  (assert (and (symbolp var) (not (member var '(t nil)))))
  (if (null args) var
      `(if (null ,var)
	   (progn
	     (setf ,var ,(car args))
	     (nconc ,var ,@(cdr args)))
	   (nconc ,var ,@args))))

(defmacro if-ret (form &rest else-body)
  "
  If @arg{form} evaluates to non-nil, it is returned, else
  the s-expression @arg{else-body} is evaluated.

  Example:
  @lisp
  > (macroexpand-1
      `(if-ret (when (evenp x) x)
             (+ x 1)))
  => (LET ((#:G927 (WHEN (EVENP X) X)))
         (OR #:G927 (PROGN (+ X 1))))
  @end lisp
  "
  (let ((ret (gensym)))
    `(let ((,ret ,form))
       (or ,ret
	   (progn
	     ,@else-body)))))

(defmacro when-let ((var . form) &rest body)
  "
  Binds the result of @arg{form} to the symbol @arg{var}; if this value
  is non-nil, the s-expression @arg{body} is executed.

  Example:
  @lisp
  > (macroexpand-1
      `(when-let (parity (evenp x))
             (+ x 1)))
  => (LET ((PARITY (EVENP X)))
        (WHEN PARITY (+ X 1)))
  @end lisp
  "
  (check-type var symbol)
  `(let ((,var ,@form))
     (when ,var
       ,@body)))

(defmacro if-let ((var . form) &rest body)
  "
  Binds the result of @arg{form} to the symbol @arg{var}; this value
  is used immediately in an if-statement with the usual semantics.

  Example:
  @lisp
  > (macroexpand-1
      `(if-let (parity (evenp x))
             (+ x 1)
             x))
  => (LET ((PARITY (EVENP X)))
        (IF PARITY
           (+ X 1)
           X))
  @end lisp
  "
  (check-type var symbol)
  `(let ((,var ,@form))
     (if ,var
	 ,@body)))

(defmacro zip-eq (a b)
  "
  Macro which which checks for eq over respective elements of the lists
  @arg{a} and @arg{b}.

  Example:
  @lisp
  > (macroexpand-1
       `(zip-eq (a b c) (1 2 3)))
  => (AND (EQ A 1) (EQ B 2) (EQ C 3))
  @end lisp
  "
  `(and ,@(mapcar (lambda (pair) (cons 'eq pair))
		  (zip (ensure-list a) (ensure-list b)))))

(defmacro macrofy (lambda-func)
  "
  Macrofies a lambda function, for use later inside macros (or for symbolic math ?).
  Returns a macro-function like function which can be called later for use inside
  macros.

  DO NOT USE backquotes in the lambda function!

  Example:
  @lisp
  > (macroexpand-1 `(macrofy (lambda (x y z) (+ (sin x) y (apply #'cos (list z))))))
  =>   (LAMBDA (X Y Z)
           (LIST '+ (LIST 'SIN X) Y (LIST 'APPLY (LIST 'FUNCTION 'COS) (LIST 'LIST Z))))
  T

  > (funcall (macrofy (lambda (x y z) (+ (sin x) y (apply #'cos (list z))))) 'a 'b 'c)
  => (+ (SIN A) B (APPLY #'COS (LIST C)))

  @end lisp
  "
  (destructuring-bind (labd args &rest body) lambda-func
    (assert (eq labd 'lambda))
    `(lambda ,args ,@(cdr (unquote-args body args)))))
    
(defmacro looped-mapcar ((func lst) &rest body)
  "
  A macro to use when caught between the efficiency of imperative looping, and
  the elegance of the mapcar (in a dozen places).

  Works by collecting references to the symbol @arg{func} and replacing them with a varible
  inside a loop. Note that although we traverse through the list only once, the collected
  lists aren't freed until the macro is closed.

  Example:
  @lisp
  > (macroexpand-1
      `(looped-mapcar (lmap '(1 2 3 4 5 6 7 8 9 10))
			(cons (lmap #'even) (lmap #'(lambda (x) (+ x 1))))))
  => (LET ((#:|lst1118| '(1 2 3 4 5 6 7 8 9 10)))
        (LOOP FOR #:|ele1117| IN #:|lst1118|
            COLLECT (FUNCALL #'(LAMBDA (X) (+ X 1))
                             #:|ele1117|) INTO #:|collect1116|
            COLLECT (FUNCALL #'EVEN #:|ele1117|) INTO #:|collect1115|
            FINALLY (RETURN (PROGN (CONS #:|collect1115| #:|collect1116|)))))
  @end lisp
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
	       (loop :for ,ele-sym :in ,lst-sym
		  ,@(loop :for decl :in ret
		       :append `(collect (funcall ,(second decl) ,ele-sym) into ,(first decl)))
		  :finally (return
			     (progn
			       ,@tf-code)))))))))

(defmacro inlining (&rest definitions)
  "
  Function created in the body of code @arg{definitions} with @macro{defun} isand declaims
  them as inline.
  Example:
  @lisp
  > (macroexpand-1
      `(inlining
         (defun sum (a b) (+ a b))))
  => (PROGN (DECLAIM (INLINE SUM)) (DEFUN SUM (A B) (+ A B)))
  "
  `(progn ,@(loop :for def :in definitions :when (eq (first def) 'defun) :collect
		  `(declaim (inline ,(second def))) collect def)))

(defmacro definline (name &rest rest)
  "
  Creates a function and declaims them inline: short form for defining an inlined function.

  Example:
  @lisp
  > (macroexpand-1 `(definline f (a b) (+ a b)))
  => (INLINING (DEFUN F (A B) (+ A B)))
  "
  `(inlining (defun ,name ,@rest)))

;;---------------------------------------------------------------;;
;; Optimization
;;---------------------------------------------------------------;;
(defmacro with-optimization ((&rest args) &body forms)
  "
  Macro creates a local environment with optimization declarations, and
  executes form.

  Example:
  @lisp
  > (macroexpand-1
      `(with-optimization (:speed 2 :safety 3)
          (+ 1d0 2d0)))
  => (LOCALLY (DECLARE (OPTIMIZE (SPEED 2) (SAFETY 3))) (+ 1.0d0 2.0d0))
  @end lisp
  "
  `(locally
       ,(recursive-append
	 `(declare (optimize ,@(multiple-value-call #'mapcar #'(lambda (key val) (list (intern (symbol-name key)) val))
						    (loop :for ele :in args
						       :counting t :into cnt
						       :if (oddp cnt)
						         :collect ele into key
						       :else
						         :collect (progn (assert (member ele '(0 1 2 3))) ele) into val
						       :finally (return (values key val))))))
	 (when (and (consp (car forms)) (eq (caar forms) 'declare))
	   (cdar forms)))
     ,@(if (and (consp (car forms)) (eq (caar forms) 'declare)) (cdr forms) forms)))

(defmacro quickly (&body forms)
  "
  Macro which encloses @arg{forms} inside
  (declare (optimize (speed 3))).
  "
  `(with-optimization (:speed 3)
     ,@forms))

(defmacro very-quickly (&body forms)
  "
  Macro which encloses @arg{forms} inside
  (declare (optimize (speed 3) (safety 0) (space 0)))
  "
  `(with-optimization
       #+lispworks
       (:safety 0 :space 0 :speed 3 :float 0 :fixnum-safety 0)
       #-lispworks
       (:safety 0 :space 0 :speed 3)
     ,@forms))

(defmacro slowly (&body forms)
  "
  Macro which encloses @arg{forms} inside
  (declare (optimize (speed 1)))
  "
  `(with-optimization (:speed 1)
     ,@forms))

)
