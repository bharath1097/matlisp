(in-package #:matlisp)

;;
(defgeneric mapsor! (func x y)
  (:documentation
"
    Syntax
    ======
    (MAPSOR! func x y)

    Purpose
    =======
    Applies the function element-wise on x, and sets the corresponding
    elements in y to the value returned by the function.

    Example
    =======
    > (mapsor! #'(lambda (idx x y)
		  (if (= (car idx) (cadr idx))
		      (sin x)
		      y))
       (randn '(2 2)) (zeros '(2 2)))
    #<REAL-TENSOR #(2 2)
    -9.78972E-2  0.0000
     0.0000     -.39243
    >
    >
")
  (:method :before ((func function) (x standard-tensor) (y standard-tensor))
	   (assert (very-quickly (lvec-eq (dimensions x) (dimensions y))) nil 'tensor-dimension-mismatch)))

(defmethod mapsor! ((func function) (x standard-tensor) (y standard-tensor))
  (let ((clx (class-name (class-of x)))
	(cly (class-name (class-of y))))
    (assert (and
	     (member clx *tensor-type-leaves*)
	     (member cly *tensor-type-leaves*))
	    nil 'tensor-abstract-class :tensor-class (list clx cly))
    (compile-and-eval
     `(defmethod mapsor! ((func function) (x ,clx) (y ,cly))
	(dorefs (idx (dimensions x))
		((ref-x x :type ,clx)
		 (ref-y y :type ,cly))
		(setf ref-y (funcall func (lvec->list idx) ref-x ref-y)))
	y)))
  (mapsor! func x y))

(definline mapsor (func x &optional output-type)
  (let ((ret (zeros (dimensions x) (or output-type (class-of x)))))
    (mapsor! #'(lambda (idx x y) (declare (ignore idx y)) (funcall func x)) x ret)))

(defmacro map-tensor! (type x func &optional null-arity?)
  (using-gensyms (decl (x) (idx ref))
    `(let (,@decl)
       (declare (type ,type ,x))
       (very-quickly (dorefs (,idx (dimensions ,x))
			     ((,ref ,x :type ,type))
			     (setf ,ref (,func ,@(unless null-arity? `(,ref))))))
       ,x)))

;;
(defun check-dims (axlst tensors)
  (let ((axlst (if (numberp axlst) (make-list (length tensors) :initial-element axlst) axlst)))
    (iter (for x in tensors)
	  (for axis in axlst)
	  (with dims = nil)
	  (cond
	    ((typep x 'standard-tensor)
	     (let-typed ((xdims (dimensions x) :type index-store-vector))
			(assert (< axis (order x)) nil 'tensor-dimension-mismatch)
			(if (null dims)
			    (setf dims (aref xdims (mod axis (order x))))
			    (setf dims (min (aref xdims (mod axis (order x))) dims))))
	     (collect (aref (strides x) (mod axis (order x))) into strides)
	     (collect (slice~ x axis 0 (if (> (order x) 1) nil t)) into slices))
	    ((eq x nil)
	     (collect nil into strides)
	     (collect nil into slices))
	    (t (error 'invalid-arguments)))
	  (finally (return (values dims strides slices))))))

(defun mapslice (axis func tensor &rest more-tensors)
  (multiple-value-bind (d.axis strides slices) (check-dims axis (cons tensor more-tensors))
    (loop :for i :from 0 :below d.axis
       :collect (prog1 (apply func (mapcar #'copy slices))
		  (when (< i (1- d.axis))
		    (loop :for slc :in slices
		       :for std :in strides
		       :do (when slc (incf (slot-value slc 'head) std))))))))

(defun mapslice~ (axis func tensor &rest more-tensors)
  (multiple-value-bind (d.axis strides slices) (check-dims axis (cons tensor more-tensors))
   (loop :for i :from 0 :below d.axis
       :collect (prog1 (apply func slices)
		  (when (< i (1- d.axis))
		    (loop :for slc :in slices
		       :for std :in strides
		       :do (when slc (incf (slot-value slc 'head) std))))))))

(defun mapslicec~ (axis func tensor &rest more-tensors)
  (multiple-value-bind (d.axis strides slices) (check-dims axis (cons tensor more-tensors))
    (loop :for i :from 0 :below d.axis
       :do (prog1 (apply func slices)
	     (when (< i (1- d.axis))
	       (loop :for slc :in slices
		  :for std :in strides
		  :do (when slc (incf (slot-value slc 'head) std)))))))
  (values-list (cons tensor more-tensors)))
;;

(defmacro tensor-foldl (type func ten init &key (init-type (field-type type)) (key nil))
  (using-gensyms (decl (ten init))
    (with-gensyms (sto idx of funcsym keysym)
    `(let* (,@decl
	    ,@(unless (symbolp func)
		`((,funcsym ,func)))
	    ,@(unless (symbolp key)
		`((,keysym ,key)))
	    (,sto (store ,ten)))
       (declare (type ,type ,ten)
		,@(unless (symbolp func) `((type function ,funcsym)))
		,@(unless (symbolp key) `((type function ,keysym)))
		(type ,(store-type type) ,sto)
		,@(when init-type
			`((type ,init-type ,init))))
       (very-quickly
	 (mod-dotimes (,idx (dimensions ,ten))
	   :with (linear-sums
		  (,of (strides ,ten)))
	   :do (setf ,init (,@(if (symbolp func)
				  `(,func)
				  `(funcall ,funcsym)) ,init ,(recursive-append
							       (when key
								 (if (symbolp key)
								     `(,key)
								     `(funcall ,keysym)))
							       `(t/store-ref ,type ,sto ,of))))))
       ,init))))

;;
(defmacro with-peeky! (((&rest tensors) &optional (step 1)) &rest body)
  (let ((ts (zipsym tensors)))
    (with-gensyms (e.step s)
      `(let (,@ts
	     (,e.step ,step))
	 (unless (and
		  ,@(mapcar #'(lambda (x) `(when-let (,s (gethash 'slice-increment (attributes ,x)))
					     (incf (slot-value ,x 'head) (* ,e.step ,s))))
			    (mapcar #'car ts)))
	   (error 'tensor-error :message "Can't find slice-increment in tensor's attributes"))
	 (prog1 (progn ,@body)
	   (unless (and
		    ,@(mapcar #'(lambda (x) `(when-let (,s (gethash 'slice-increment (attributes ,x)))
					       (decf (slot-value ,x 'head) (* ,e.step ,s))))
			      (mapcar #'car ts)))
	     (error 'tensor-error :message "Can't find slice-increment in tensor's attributes")))))))

(definline peek-tensor! (x &optional (step 1))
  (if-let (s (gethash 'slice-increment (attributes x)))
    (progn (incf (slot-value x 'head) (* step s)) x)
    (error 'tensor-error :message "Can't find slice-increment in tensor's attributes" :tensor x)))

(defmacro-clause (FOR xa SLICING x ALONG axis &optional FROM start BELOW oend TO cend DOWNTO dend)
  (let* ((xa (if (listp xa) xa (list xa))))
    (when (or (and oend cend) (and dend (or cend oend))) (error "Use only one of BELOW TO DOWNTO."))
    (unless (null xa)
      (with-gensyms (dim alist asym xsym slst tmp-axis tmp-dim tmp-x x-eval)
	(let* ((n (length xa)))
	  `(progn
	     (with ,x-eval = ,x)
	     (with ,dim = -1)
	     (with ,alist = ,(with-gensyms (axis-eval)
			       `(let ((,axis-eval ,axis))
				  (if (listp ,axis-eval)
				      (assert (= (length ,axis-eval) ,n) nil 'invalid-arguments)
				      (setq ,axis-eval (make-list ,n :initial-element ,axis-eval)))
				  ,axis-eval)))
	     ,@(mapcan #'(lambda (x) `((with ,x = nil))) xa)
	     (initially
	      (let ((,x-eval (if (listp ,x-eval)
				 (progn (assert (= (length ,x-eval) ,n) nil 'invalid-arguments) ,x-eval)
				 (make-list ,n :initial-element ,x-eval))))
		(iter (for ,xsym in ,x-eval)
		      (for ,asym on ,alist)
		      (etypecase ,xsym
			(null (setf (car ,asym) nil))
			(standard-tensor
			 (let* ((,tmp-axis (modproj (car,asym) (order ,xsym))))
			   (let ((,tmp-x ,(if (or start oend cend dend)
					      `(let ((,tmp-dim (dimensions ,xsym (the index-type ,tmp-axis)))
						     (,slst (make-list (order ,xsym) :initial-element '(nil nil))))
						 (declare (ignorable ,tmp-dim))
						 (setf (nth ,tmp-axis ,slst) ,(cond
									       (dend `(list* ,start (- (modproj ,dend ,tmp-dim) ,tmp-dim 1) -1))
									       (oend `(list ,start ,oend))
									       (cend `(list ,start (1+ (modproj ,cend ,tmp-dim))))
									       (t `(list ,start nil))))
						 (subtensor~ ,xsym ,slst))
					      xsym)))
			     (when (or (< ,dim 0) (> ,dim (dimensions ,tmp-x (the index-type ,tmp-axis))))
			       (setf ,dim (dimensions ,tmp-x (the index-type ,tmp-axis))))
			     (setf (car ,asym) (cons ,(with-gensyms (tmp)
								    `(let ((,tmp (slice~ ,tmp-x ,tmp-axis)))
								       (setf (gethash 'slice-increment (attributes ,tmp)) (strides ,tmp-x ,tmp-axis))
								       ,tmp))
						     (strides ,tmp-x ,tmp-axis)))))))))
	      (let* ((,alist ,alist))
		(setf ,@(mapcan #'(lambda (x) `(,x (caar ,alist)
						  ,alist (cdr ,alist)))
				xa))))
	     (repeat ,dim)
	     (after-each
	      ,(with-gensyms (tmp)
		 `(iter (for ,tmp in ,alist)
			(when ,tmp (incf (slot-value (car ,tmp) 'head) (cdr ,tmp))))))))))))

(defmacro values-n ((n) &rest values)
  (using-gensyms (decl (n))
    (labels ((make-cd (i rets vrets)
	       `((let (,(car rets))
		   ,(if (null (cdr rets))
			`(values ,@(reverse vrets) ,(caar rets))
			`(if (<= ,n ,i)
			     (values ,@(reverse vrets) ,(caar rets))
			     ,@(make-cd (1+ i) (cdr rets) (cons (caar rets) vrets))))))))
      `(let (,@decl)
	 ,@(make-cd 1 (zipsym values) nil)))))
