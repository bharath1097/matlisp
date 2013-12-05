(in-package #:matlisp)

(defgeneric mapsor! (func x y)
  (:documentation "
   Syntax
   ======
   (MAPSOR! func x y)

   Purpose
   =======
   Applies the function element-wise on x, and sets the corresponding
   elements in y to the value returned by the function.

   Example
   =======
   > (mapsor! #'sin (randn '(2 2)) (zeros '(2 2)))
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
	(let ((sto-x (store x))
	      (sto-y (store y)))
	  (declare (type ,(store-type clx) sto-x)
		   (type ,(store-type cly) sto-y))
	  (very-quickly
	    (mod-dotimes (idx (dimensions x))
	      :with (linear-sums
		     (of-x (strides x))
		     (of-y (strides y)))
	      :do (t/store-set ,cly (funcall func (t/store-ref ,clx sto-x of-x)) sto-y of-y))))
	y)))
  (mapsor! func x y))

(definline mapsor (func x)
  (let ((ret (zeros (dimensions x) (class-of x))))
    (mapsor! func x ret)))
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
