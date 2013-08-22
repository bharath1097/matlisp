(in-package #:matlisp)

(defgeneric mapsor! (func x y)
  (:documentation "
  y <- func(x)
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

(defun mapcol (func x &optional (axis 1))
  (declare (type standard-tensor x))
  (assert (tensor-matrixp x) nil 'tensor-dimension-mismatch)
  (let* ((v-x (slice~ x axis))
	 (st-x (aref (the index-store-vector (strides x)) axis))
	 (ret (zeros (aref (the index-store-vector (dimensions x)) axis) (class-of x))))
    (loop :for i :from 0 :below (aref (the index-store-vector (dimensions x)) axis)
       :do (progn
	     (setf (ref ret i) (funcall func v-x))
	     (incf (slot-value v-x 'head) st-x)))
    ret))

(defun tensor-min (x)
  (let ((min nil))
    (mod-dotimes (idx (dimensions x))
      :do (when (or (null min) (< (ref x idx) min))
	    (setf min (ref x idx))))
    min))

(defun idx-min (v)
  (let ((min-idx nil)
	(min nil))
    (loop :for i :from 0 :below (aref (the index-store-vector (dimensions v)) 0)       
       :do (when (or (null min) (< (ref v i) min))
	     (setf min-idx i
		   min (ref v i))))
    (values min-idx min)))

(defun mapxis! (func x y &optional (axis 0))
  (declare (type standard-tensor x y))
  (multiple-value-bind (view-x view-y) (let ((slst (make-list (rank x) :initial-element '(* * *))))
					 (rplaca (nthcdr axis slst) (list 0 '* 1))
					 (values (sub-tensor~ x slst nil) (sub-tensor~ y slst nil)))
    (let ((st-x (aref (the index-store-vector (dimensions x)) axis))
	  (st-y (aref (the index-store-vector (dimensions x)) axis)))
      (loop :for i :from 0 :below (aref (the index-store-vector (dimensions x)) axis)
	 :do (progn
	       ;;May die if you're doing fancy stuff.
	       (funcall func view-x view-y)
	       
  
