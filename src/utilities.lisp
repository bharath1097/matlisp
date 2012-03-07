(in-package :utilities)

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
(defmacro mlet (decls &rest body)
  (let ((let-defns nil)
	(let-decls nil))
    (labels ((mlet-parse! (decl)
	       (check-type (car decl) symbol)
	       (if (null (cddr decl))
		   (prog1
		       (cadr decl)
		     (setf (cdr decl) nil))
		   (mlet-parse! (cdr decl))))
	     (mlet-walk (elst body)
	       (let* ((decl (car elst))
		      (elst-decl (pop-arg! :declare decl)))
		 (cond
		   ((null decl)
		    `(,@body))
		   ((null (cddr decl))
		    (nconsc let-defns `((,(caar elst) ,(cadar elst))))
		    (nconsc let-decls `(,@elst-decl))
		    (mlet-walk (cdr elst) body))
		   (t
		    (let ((form (mlet-parse! decl)))
		      `((multiple-value-bind (,@decl) ,form
			  ,@(when elst-decl
				  `((declare ,@elst-decl)))
			  ,@(mlet-walk (cdr elst) body)))))))))
      (if decls
	  (let ((tc (mlet-walk decls body)))
	    (if let-defns
		`(let (,@let-defns)
		   ,@(when let-decls
			   `((declare ,@let-decls)))
		   ,@tc)
		(car tc)))
      `(progn
	 ,@body)))))
;;
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
(defmacro when-let (lst &rest body)
  (let ((ret (car lst))
	(form (cadr lst)))
    (check-type ret symbol)
    `(let ((,ret ,form))
       (when ,ret
	 ,@body))))

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
(defmacro lcase (keyform &rest body)
  (let ((key-eval (gensym)))
    (labels ((app-equal (lst)
	       (if (null lst)
		   nil
		   `(((equal ,key-eval ,(caar lst)) ,@(cdar lst))
		     ,@(app-equal (cdr lst))))))
      `(let ((,key-eval ,keyform))
	 (cond
	   ,@(app-equal body))))))