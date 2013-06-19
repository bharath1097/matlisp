(in-package :matlisp-template)
;;Macro-management is the word.
;;Suck on that C++ :)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *template-table* (make-hash-table)))

(defun match-lambda-lists (lsta lstb)
  (let ((optional? nil))
    (labels ((optp? (a b)
	       (if (and (consp a) (atom b)) (optp? b a)
		   (progn
		     (if (or (member a lambda-list-keywords) (not optional?)) nil
			 (if (null (cddr b)) t nil)))))
	     (lst-walker (a b)
	       (cond
		 ((and (atom a) (atom b))
		  (if (eq a b)
		      (progn
			(when (member a lambda-list-keywords)
			  (setq optional? (if (member a '(&optional &key)) t nil)))
			t)
		      (if (or (member a lambda-list-keywords) (member b lambda-list-keywords)) nil t)))
		 ((or (atom a) (atom b))
		  (if (optp? a b) t nil))
		 ((and (consp a) (consp b))
		  (and (lst-walker (car a) (car b))
		       (lst-walker (cdr a) (cdr b)))))))
      (lst-walker lsta lstb))))

;;
(defgeneric compute-t/dispatch (name args)
  (:method ((name symbol) args)
    (let* ((data (or (gethash name *template-table*)
		     (error "undefined template : ~a~%" name)))
	   (pred (getf data :predicate))
	   (meth (getf data :methods)))
      (or (car (loop :for spl :in meth
		  :do (when (funcall pred args (second spl))
			(return spl))))
	  (error "could not find a \"~a\" template for : ~a~%" name args)))))

;;
(defun single-argp (name)
  (let* ((data (or (gethash name *template-table*)
		   (error "Undefined template : ~a~%" name)))
	 (ll (getf data :lambda-list)))
    (values (not (consp (first ll))) ll)))

(defgeneric preprocess-t/dispatch (name args)
  (:method ((name symbol) args)
    (funcall (if (single-argp name) #'funcall #'mapcar)
	     #'macroexpand-1 args)))
;;
(defmacro deft/generic ((name predicate &optional sorter) disp args)
  (when (consp disp)
    (assert (null (remove-if-not #'(lambda (x) (member x cl:lambda-list-keywords)) disp)) nil "dispatch list contains keywords."))
  (with-gensyms (warg-sym disp-sym meth-sym pred-sym)
    (multiple-value-bind (disp-arg disp-far)
	(if (consp disp)
	    (values `(&whole ,disp-sym ,@disp) disp-sym)
	    (values disp disp))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
	 (setf (gethash ',name *template-table*) (list :lambda-list (list ',disp ',args) :predicate ,predicate :sorter ,(or sorter predicate) :methods nil))
	 (defmacro ,name (&whole ,warg-sym ,disp-arg ,@args)
	   (declare (ignore ,@(remove-if #'(lambda (x) (member x cl:lambda-list-keywords)) args)))
	   (let* ((,pred-sym (preprocess-t/dispatch ',name ,disp-far))
		  (,meth-sym (compute-t/dispatch ',name ,pred-sym)))
	     (apply ,meth-sym (cons ,pred-sym (cddr ,warg-sym)))))))))

(defmacro deft/method (name disp args &rest body)
  (with-gensyms (data-sym meth-sym afun-sym disp-sym sort-sym)
    (let* ((data (or (gethash name *template-table*)
		     (error "Undefined template : ~a~%" name)))
	   (ll (getf data :lambda-list))
	   (single? (not (consp (first ll))))
	   ;;
	   (disp-vars (funcall (if single? #'funcall #'mapcar) #'(lambda (x) (if (consp x) (car x) x)) disp))
	   (disp-spls (funcall (if single? #'funcall #'mapcar) #'(lambda (x) (if (consp x) (cadr x) t)) disp)))
      (assert (match-lambda-lists (list disp-vars args) ll) nil "mismatch in lambda-lists.")
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let* ((,data-sym (or (gethash ',name *template-table*)
			     (error "Undefined template : ~a~%" ',name)))
	      (,meth-sym (getf ,data-sym :methods))
	      (,afun-sym (lambda (,(if single? disp-vars disp-sym) ,@args)
			   ,(recursive-append 
			     (unless single?
			       `(destructuring-bind (,@disp-vars) ,disp-sym))
			     `(progn
				,@body))))
	      (,sort-sym (getf ,data-sym :sorter)))
	 (setf ,meth-sym (sort (setadd ,meth-sym (list ,afun-sym ',disp-spls) #'(lambda (a b) (list-eq (second a) (second b)))) #'(lambda (a b) (funcall ,sort-sym (second a) (second b)))))
	 (setf (getf ,data-sym :methods) ,meth-sym)
	 ,afun-sym)))))

(defun remt/method (name spls)
  (let* ((data (or (gethash name *template-table*)
		   (error "Undefined template : ~a~%" name)))
	 (meth (getf data :methods)))
    (setf (getf data :methods) (setrem meth spls  #'(lambda (a b) (list-eq (second a) b))))
    nil))

