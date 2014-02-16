(in-package #:matlisp)

;;tensor specializations
(deft/generic (t/field-type #'subtypep) sym ())
(deft/method t/field-type (sym base-tensor) ()
  t)

(eval-every
  (defun coerceable? (clx cly)
    (handler-case
	(progn
	  (macroexpand-1 `(t/strict-coerce ((t/field-type ,clx) (t/field-type ,cly)) x))
	  t)
      (error () nil))))

(eval-every
  (defun field-type (clname)
    (macroexpand-1 `(t/field-type ,clname))))

;;This is useful for Eigenvalue decompositions
(deft/generic (t/complexified-type #'subtypep) sym ())
(eval-every 
  (defun complexified-type (type)
    (macroexpand-1 `(t/complexified-type ,type))))

(deft/generic (t/store-allocator #'subtypep) sym (size &optional initial-element))

(deft/generic (t/store-type #'subtypep) sym (&optional size))
(eval-every
  (defun store-type (cl &optional (size '*))
    (macroexpand-1 `(t/store-type ,cl ,size))))

(deft/generic (t/store-ref #'subtypep) sym (store &rest idx))
(deft/generic (t/store-set #'subtypep) sym (value store &rest idx))

;;standard-tensor specific.

;;Beware of infinite loops here.
(deft/generic (t/store-element-type #'subtypep) sym ())
(deft/method t/store-element-type (sym standard-tensor) ()
  (macroexpand-1 `(t/field-type ,sym)))

(eval-every
  (defun store-element-type (clname)
    (macroexpand-1 `(t/store-element-type ,clname))))
;;
(deft/method t/store-type (sym standard-tensor) (&optional (size '*))
 `(simple-array ,(store-element-type sym) (,size)))
;;
(deft/generic (t/compute-store-size #'subtypep) sym (size))
(deft/method t/compute-store-size (sym standard-tensor) (size)
  size)
;;
(deft/generic (t/store-size #'subtypep) sym (ele))
(deft/method t/store-size (sym standard-tensor) (ele)
  `(length ,ele))
;; 
(deft/method t/store-allocator (sym standard-tensor) (size &optional initial-element)
  (with-gensyms (sitm size-sym arr idx init)
    (let ((type (macroexpand-1 `(t/store-element-type ,sym))))
      `(let*-typed ((,size-sym (t/compute-store-size ,sym (let ((,sitm ,size))
							    (etypecase ,sitm
							      (index-type ,sitm)
							      (index-store-vector (lvec-foldr #'* (the index-store-vector ,sitm)))
							      (cons (reduce #'* ,sitm))))))
		    ,@(when initial-element `((,init ,initial-element :type ,(field-type sym))))
		    (,arr (make-array ,size-sym :element-type ',type :initial-element ,(if (subtypep type 'number) `(t/fid+ ,type) nil)) :type ,(store-type sym)))
	,@(when initial-element
		`((very-quickly
		    (loop :for ,idx :from 0 :below ,size-sym
		       :do (t/store-set ,sym ,init ,arr ,idx)))))
	,arr))))
;;
(deft/generic (with-field-element #'subtypep) sym (decl &rest body))
(deft/method with-field-element (sym standard-tensor) (decl &rest body)
  (destructuring-bind (var init &optional (count 1)) decl
    `(let-typed ((,var (t/store-allocator ,sym ,count ,init) :type ,(store-type sym)))
       (locally
	   ,@body))))
;;
(deft/method t/store-ref (sym linear-store) (store &rest idx)
   (assert (null (cdr idx)) nil "given more than one index for linear-store")
  `(aref (the ,(store-type sym) ,store) (the index-type ,(car idx))))

(deft/method t/store-set (sym linear-store) (value store &rest idx)
   (assert (null (cdr idx)) nil "given more than one index for linear-store")
  `(setf (aref (the ,(store-type sym) ,store) (the index-type ,(car idx))) (the ,(field-type sym) ,value)))
;;
;;A helper macro which takes of care of the class checking and stuff.
(defparameter *generated-methods* (make-hash-table))

(definline lazy-coerce (x out)
  (if (typep x out) x
      (copy x out)))

(defun cclass-max (lst)
  (let ((max nil))
    (loop :for ele :in lst
       :do (when (or (null max) (and (coerceable? max ele)
				     (or (not (coerceable? ele max))
					 (and (subtypep ele 'blas-numeric-tensor) (subtypep max 'blas-numeric-tensor)
					      (> (float-digits (coerce 0 (store-element-type ele)))
						 (float-digits (coerce 0 (store-element-type max))))))))	     
	     (setf max ele)))
    max))

(defmacro define-tensor-method (name (&rest args) &body body)
  (let* ((inputs (mapcar #'car (remove-if-not #'(lambda (x) (and (consp x) (eql (third x) :input))) args)))
	 (outputs (mapcar #'car (remove-if-not #'(lambda (x) (and (consp x) (eql (third x) :output))) args)))
	 (iclsym (zipsym inputs))
	 (oclsym (zipsym outputs)))
    ;;
    (multiple-value-bind (val exists?) (gethash name *generated-methods*)
      (if exists?
	  (let ((type-meths (assoc (mapcar #'(lambda (x) (if (consp x) (cadr x) t)) args) (cdr val) :test #'list-eq)))
	    (if type-meths
		(progn
		  (loop :for ele in (cdr type-meths)
		     :do (remove-method (symbol-function name) ele))
		  (setf (cdr type-meths) nil))
		(setf (cdr val) (list* (list (mapcar #'(lambda (x) (if (consp x) (cadr x) t)) args)) (cdr val)))))
	  (setf (gethash name *generated-methods*) (list name (list (mapcar #'(lambda (x) (if (consp x) (cadr x) t)) args))))))
    ;;
    (with-gensyms (x classes iclasses oclasses)
      `(defmethod ,name (,@(mapcar #'(lambda (x) (if (consp x) (subseq x 0 2) x)) args))
	 (let* (,@(mapcar #'(lambda (lst) `(,(car lst) (class-name (class-of ,(cadr lst))))) (append iclsym oclsym))
		(,iclasses (list ,@(mapcar #'car iclsym)))
		(,oclasses (list ,@(mapcar #'car oclsym)))
		(,classes (append ,iclasses ,oclasses)))
	   (labels ((generate-code (class)
		      (let ((args (mapcar #'(lambda (x) (if (and (consp x) (member (third x) '(:input :output)))
							    (list (car x) class)
							    x))
					  '(,@args)))
			    (ebody (macrolet ((cl (,x)
						(let ((slook '(,@(mapcar #'(lambda (x) `(,(cadr x) class)) iclsym)
							       ,@(mapcar #'(lambda (x) `(,(cadr x) class)) oclsym))))
						  (or (cadr (assoc ,x slook)) (error "Can't find class of ~a" ,x)))))
				     (list ,@body))))
			`(defmethod ,',name (,@args)
			   ,@ebody))))
	     (cond
	       ((every #'(lambda (,x) (eql ,x (car ,classes))) ,classes)
		(assert (member (car ,classes) *tensor-type-leaves*)
			nil 'tensor-abstract-class :tensor-class ,classes)
		(let* ((method (compile-and-eval (generate-code (car ,classes))))
		       (lst (assoc ',(mapcar #'(lambda (x) (if (consp x) (cadr x) t)) args) (cdr (gethash ',name *generated-methods*)) :test #'list-eq)))
		  (assert lst nil "Method table missing from *generated-methods* !")
		  (setf (cdr lst) (list* method (cdr lst))))
		(,name ,@(mapcar  #'(lambda (x) (if (consp x) (car x) x)) args)))
	       ((and (every #'(lambda (,x) (eql ,x (car ,oclasses))) ,oclasses)
		     (or (null ,oclasses) (coerceable? (cclass-max ,iclasses) (car ,oclasses))))
		(let* ((clm (or (car ,oclasses) (cclass-max ,iclasses)))
		       ,@(mapcar #'(lambda (x) `(,x (lazy-coerce ,x clm))) inputs))
		  (,name ,@(mapcar #'(lambda (x) (if (consp x) (car x) x)) args))))
	       (t
		(error "Don't know how to apply ~a to classes ~a, ~a." ',name ,iclasses ,oclasses)))))))))
