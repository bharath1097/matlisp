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

(definline mapsor (func x &optional output-type)
  (let ((ret (zeros (dimensions x) (or output-type (class-of x)))))
    (mapsor! #'(lambda (idx x y) (declare (ignore idx y)) (funcall func x)) x ret)))

;;
;; (defmacro fapply ((ref tensa &key ttype) &optional (loop-optimizations '(progn)) &rest body)
;;   (with-gensyms (tens sto idx of)
;;     `(let ((,tens ,tensa))
;;        (declare (type ,ttype ,tens))
;;        (let ((,sto (store ,tens)))
;;	 (declare (type ,(store-type ttype) ,sto))
;;	 (,@loop-optimizations
;;	   (mod-dotimes (,idx (dimensions ,tens))
;;	     :with (linear-sums
;;		    (,of (strides ,tens) (head ,tens)))
;;	     :do (let ((,ref (t/store-ref ,ttype ,sto ,of)))
;;		   (declare (type ,(field-type ttype) ,ref))
;;		   (t/store-set ,ttype
;;				(progn
;;				  ,@body)
;;				,sto ,of))))
;;	 ,tens))))

;;
(defun check-dims (axis tensors)
  (loop :for x :of-type standard-tensor :in tensors
     :with dims := nil
     :do (let-typed ((xdims (dimensions x) :type index-store-vector))
	   (assert (or (not dims) (= (order x) (length dims))) nil 'tensor-dimension-mismatch)
	   (if (null dims)
	       (setf dims (copy-seq xdims))
	       (loop :for i :from 0 :below (length dims)
		  :do (if (/= i axis)
			  (assert (= (aref xdims i) (aref dims i)) nil 'tensor-dimension-mismatch)
			  (setf (aref dims i) (min (aref xdims i) (aref dims i)))))))
     :collect (aref (strides x) axis) :into strides
     :collect (slice~ x axis) :into slices
     :finally (return (values (aref dims axis) strides slices))))

(defun mapslice (axis func tensor &rest more-tensors)
  (multiple-value-bind (d.axis strides slices) (check-dims axis (cons tensor more-tensors))
    (loop :for i :from 0 :below d.axis
       :collect (prog1 (apply func (mapcar #'copy slices))
		  (unless (< i (1- d.axis))
		    (loop :for slc :in slices
		       :for std :in strides
		       :do (incf (slot-value slc 'head) std)))))))

(defun mapslice~ (axis func tensor &rest more-tensors)
  (multiple-value-bind (d.axis strides slices) (check-dims axis (cons tensor more-tensors))
   (loop :for i :from 0 :below d.axis
       :collect (prog1 (apply func slices)
		  (unless (< i (1- d.axis))
		    (loop :for slc :in slices
		       :for std :in strides
		       :do (incf (slot-value slc 'head) std)))))))

(defun mapslicec~ (axis func tensor &rest more-tensors)
  (multiple-value-bind (d.axis strides slices) (check-dims axis (cons tensor more-tensors))
    (loop :for i :from 0 :below d.axis
       :do (prog1 (apply func slices)
	     (unless (< i (1- d.axis))
	       (loop :for slc :in slices
		  :for std :in strides
		  :do (incf (slot-value slc 'head) std))))))
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
