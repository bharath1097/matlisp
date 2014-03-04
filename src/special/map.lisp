(in-package #:matlisp)

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
	(let-typed ((sto-x (store x) :type ,(store-type clx))
		    (sto-y (store y) :type ,(store-type cly)))
	  (mod-dotimes (idx (dimensions x))
	    :with (linear-sums
		   (of-x (strides x) (head x))
		   (of-y (strides y) (head y)))
	    :do (t/store-set ,cly (funcall func (lvec->list idx) (t/store-ref ,clx sto-x of-x) (t/store-ref ,cly sto-y of-y)) sto-y of-y)))
	y)))
  (mapsor! func x y))

(defmethod mapsor! ((func function) (x standard-tensor) (y standard-tensor))
  (let ((clx (class-name (class-of x)))
	(cly (class-name (class-of y))))
    (assert (and
	     (member clx *tensor-type-leaves*)
	     (member cly *tensor-type-leaves*))
	    nil 'tensor-abstract-class :tensor-class (list clx cly))
    (compile-and-eval
     `(defmethod mapsor! ((func function) (x ,clx) (y ,cly))
	(let-typed ((sto-x (store x) :type ,(store-type clx))
		    (sto-y (store y) :type ,(store-type cly)))
	  (mod-dotimes (idx (dimensions x))
	    :with (linear-sums
		   (of-x (strides x) (head x))
		   (of-y (strides y) (head y)))
	    :do (t/store-set ,cly (funcall func (lvec->list idx) (t/store-ref ,clx sto-x of-x) (t/store-ref ,cly sto-y of-y)) sto-y of-y)))
	y)))
  (mapsor! func x y))

(definline mapsor (func x &optional output-type)
  (let ((ret (zeros (dimensions x) (or output-type (class-of x)))))
    (mapsor! #'(lambda (idx x y) (declare (ignore y)) (funcall func idx x)) x ret)))
;;

(defun mapslice (func x &optional (axis 0))
  (declare (type standard-tensor x))
  (if (tensor-vectorp x)
      (loop :for i :from 0 :below (aref (dimensions x) axis)
	 :collect (funcall func (ref x i)))
      (let* ((v-x (slice~ x axis))
	     (st-x (aref (strides x) axis)))
	(loop :for i :from 0 :below (aref (the index-store-vector (dimensions x)) axis)
	   :collect (prog1 (funcall func (copy v-x))
		      (incf (slot-value v-x 'head) st-x))))))

(defun mapslice~ (func x &optional (axis 0))
  (declare (type standard-tensor x))
  (if (tensor-vectorp x)
      (loop :for i :from 0 :below (aref (dimensions x) axis)
	 :collect (funcall func (ref x i)))
      (let* ((v-x (slice~ x axis))
	     (st-x (aref (strides x) axis)))
	(loop :for i :from 0 :below (aref (the index-store-vector (dimensions x)) axis)
	   :collect (prog1 (funcall func (subtensor~ v-x nil))
		      (incf (slot-value v-x 'head) st-x))))))

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
