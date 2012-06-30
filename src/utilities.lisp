(in-package :utilities)

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

(defun pop-arg! (sym arglist)
  (check-type sym symbol)
  (locally
      (declare (optimize (speed 3) (safety 0)))
    (labels ((get-sym (sym arglist prev)
	       (cond
		 ((null arglist) nil)
		 ((eq (car arglist) sym) (prog1
					   (cadr arglist)
					   (if prev
					       (rplacd prev (cddr arglist)))))
		 (t (get-sym sym (cdr arglist) arglist)))))
      (get-sym sym arglist nil))))

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
  (labels ((replace-atoms (lst ret)
	     (if (null lst) (reverse ret)
		 (let ((fst (car lst)))
		   (replace-atoms (cdr lst)
				  (cond 
				    ((atom fst)
				     (if (member fst args)
					 (cons fst ret)
					 (append `(',fst) ret)))
				    ((consp fst)
				     (cons (replace-lst fst nil) ret)))))))
	   (replace-lst (lst acc)
	     (cond
	       ((null lst) acc)
	       ((consp lst)
		(cons 'list (replace-atoms lst nil)))
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
  (destructuring-bind (labd args &rest body) lambda-func
    (assert (eq labd 'lambda))
    `(lambda ,args ,@(cdr (unquote-args body args)))))

(declaim (inline string+))
(defun string+ (&rest strings)
  (apply #'concatenate (cons 'string strings)))

(defun format-to-string (fmt &rest args)
  (let ((ret (make-array 0 :element-type 'character :fill-pointer t)))
    (with-output-to-string (ostr ret)
      (apply #'format (append `(,ostr ,fmt) args)))
    ret))
;;---------------------------------------------------------------;;
(defstruct (foreign-vector
	     (:conc-name fv-)
	     (:print-function (lambda (obj stream depth)
				(declare (ignore depth))
				(format stream "#F(")
				(let ((sz (fv-size obj)))
				  (dotimes (i sz)
				    (format stream (if (= i (- sz 1))
						       "~A)"
						       "~A ") (fv-ref obj i)))))))
  (pointer
   (cffi:null-pointer)
   :type cffi:foreign-pointer)
  (size
   0
   :type fixnum)
  (type
   nil
   :type symbol))

(defun fv-ref (x n)
  (declare (type foreign-vector x)
	   (type fixnum n))
  (let ((sap (fv-pointer x))
	(ss (fv-size x))
	(sty (fv-type x)))
   (unless (< -1 n ss)
     (error "Index N out of bounds."))
   (cffi:mem-aref sap sty n)))

(defun (setf fv-ref) (value x n)
  (declare (type foreign-vector x)
	   (type fixnum n))
  (let ((sap (fv-pointer x))
	(ss (fv-size x))
	(sty (fv-type x)))
   (unless (< -1 n ss)
     (error "Index N out of bounds."))
   (setf (cffi:mem-aref sap sty n) value)))

;;; Rudimentary support for making it a bit easier to deal with Fortran
;;; arrays in callbacks.

;; If the Array dimensions are (d1, d2, d3, ...)
;;
;; Then x(n1, n2, n3, ...) means the index is, essentially,
;;
;; n1 + d1*(n2 + d2*(n3 + d3*(n4 + d4*(n5))))
;;
;; Return an expression that computes the column major index given the
;; indices (a list) and a list of the bounds on each dimension.  Each
;; bound is a list of the upper and lower bounds for each dimension.
;; For example, for the Fortran array declared as x(3:10, -4:2), the
;; bounds would be written as ((3 10) (-4 2)).  If the lower bound is
;; the default of 1, you can omit the lower bound.
(defun col-major-index (indices dims)
  ;; Return a form that computes the column major index of a Fortran array.
  (flet ((get-offset (n bound)
	   (let ((lo (first bound)))
	     (if (and (numberp lo) (zerop lo))
		 n
		 `(the fixnum (- (the fixnum ,n) (the fixnum ,lo))))))
	 (get-size (bound)
	   (destructuring-bind (lo hi)
	       bound
	     (cond ((numberp lo)
		    (cond ((numberp hi)
			   (1+ (- hi lo)))
			  ((= lo 1)
			   hi)
			  (t
			   `(- ,hi ,(- lo 1)))))
		   (t
		    `(the fixnum (- ,hi (the fixnum (- (the fixnum ,lo) 1)))))))))
    (let* ((rev-idx (reverse indices))
	   (rev-dim (reverse dims))
	   (idx (get-offset (first rev-idx) (first rev-dim))))
      (do ((d (rest rev-dim) (rest d))
	   (n (rest rev-idx) (rest n)))
	  ((endp d)
	   idx)
	(setf idx `(the fixnum (+ ,(get-offset (first n) (first d))
				  (the fixnum (* ,(get-size (first d)) ,idx)))))))))

(defmacro with-fortran-matrix ((name fv &rest dims) &body body)
  (let ((indices (gensym (symbol-name '#:indices-))))
    `(macrolet ((,name (&rest ,indices)
		  `(fv-ref ,',fv ,(col-major-index ,indices ',dims))))
       ,@body)))

;; WITH-FORTRAN-MATRICES is a convenience macro for accessing Fortran
;; arrays that have been passed in as parameters of a callback.
;;
;; For example, Fortran callback function that might be
;;
;;   subroutine sub(z, f)
;;   real z(4), df(2, 4)
;;   df(1,4) = 3*z(2)
;;   end
;;
;; This can be written in a Lisp call back as
;;
;;   (defun fsub (z-arg f-arg)
;;     (with-fortran-matrices ((z z-arg (1 4))
;;                             (f f-arg ((1 2) (1 4))))
;;       (setf (f 1 4) (* 3 (z 2)))))
;;

(defmacro with-fortran-matrices ((&rest array-list) &body body)
  (if (cdr array-list)
      `(with-fortran-matrix ,(car array-list)
	 (with-fortran-matrices ,(cdr array-list)
	   ,@body))
      `(with-fortran-matrix ,(car array-list)
	 ,@body)))

(defmacro make-array-allocator (allocator-name type init &optional (doc ""))
  `(definline ,allocator-name (size &optional (initial-element ,init))
     ,@(unless (string= doc "")
	       `(,doc))
     (make-array size
		 :element-type ,type :initial-element initial-element)))

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
