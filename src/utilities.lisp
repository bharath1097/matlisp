(in-package :utilities)

;;
(defmacro mlet* (decls &rest body)
" mlet* ({ {(var*) | var} values-form &keyform declare type}*) form*

  o var is just one symbol -> expands into let
  o var is a list -> expands into multiple-value-bind

  This macro also handles type declarations.

  Example:
  (mlet* ((x 2 :type fixnum :declare ((optimize (safety 0) (speed 3))))
  	((a b) (floor 3) :type (nil fixnum)))
         (+ x b))

  expands into:

  (let ((x 2))
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
	   ;;
	   (mlet-transform (elst nest-code)
	     (destructuring-bind (vars form &key declare type) elst
	       `(,(append (cond
			    ;;If there is only one element use let
			    ;;instead of multiple-value-bind
			    ((or (symbolp vars))
			     `(let ((,vars ,form))))
                            ;;
			    (t
			     `(multiple-value-bind (,@vars) ,form)))
			  (if (symbolp vars)
			      (mlet-decl (list vars) (list type) declare)
			      (mlet-decl vars type declare))
			  nest-code))))
	   ;;
	   (mlet-walk (elst body)
	     (if (null elst)
		 `(,@body)
		 (mlet-transform (car elst) (mlet-walk (cdr elst) body)))))
    ;;
    (if decls
	(car (mlet-walk decls body))
	`(progn
	   ,@body))))

;;
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

;;
(defun get-arg (sym arglist)
  (check-type sym symbol)
  (locally
      (declare (optimize (speed 3) (safety 0)))
    (labels ((get-sym (sym arglist)
	       (cond
		 ((null arglist) nil)
		 ((eq (car arglist) sym) (cadr arglist))
		 (t (get-sym sym (cddr arglist))))))
      (get-sym sym arglist))))

;;
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

;;
(defun slot-values (obj slots)
  (values-list (mapcar #'(lambda (slt) (slot-value obj slt))
		       slots)))

;;
(defun ensure-list (lst)
  (if (listp lst)
      lst
      `(,lst)))

(defmacro if-ret (form &rest else-body)
  "if-ret (form &rest else-body)
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

;;
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

;;
(defstruct (foreign-vector
	     (:conc-name fv-)
	     (:print-function (lambda (obj stream depth)
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
;;
