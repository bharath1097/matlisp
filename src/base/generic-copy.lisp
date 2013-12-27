(in-package #:matlisp)

(defgeneric copy! (from to)
  (:documentation
   "
  Syntax
  ======
  (COPY! x y)

  Purpose
  =======
  Copies the contents of X into Y. Returns Y.
")
  (:method :before ((x array) (y array))
	   (assert (list-eq (array-dimensions x) (array-dimensions y))
		   nil 'dimension-mismatch)))

(defmethod copy! ((from cons) (to cons))
  (do ((flst from (cdr flst))
       (tlst to (cdr tlst)))
      ((or (null flst) (null tlst)))
    (setf (car tlst) (car flst)))
  to)

(defmethod copy! ((from t) (to cons))
  (mapl #'(lambda (lst) (rplaca lst from)) to)
  to)

(defmethod copy! ((from array) (to array))
  (let ((lst (make-list (array-rank to))))
    (mod-dotimes (idx (make-index-store (array-dimensions to)))
      :do (progn
	    (lvec->list! idx lst)
	    (setf (apply #'aref to lst) (apply #'aref from lst)))))
  to)

(defmethod copy! ((from t) (to array))
  (let ((lst (make-list (array-rank to))))
    (mod-dotimes (idx (make-index-store (array-dimensions to)))
      do (progn
	   (lvec->list! idx lst)
	   (setf (apply #'aref to lst) from)))
  to))

;;
(defmethod copy! :before ((x array) (y standard-tensor))
  (assert (list-eq (array-dimensions x) (lvec->list (dimensions y)))
	  nil 'dimension-mismatch))
(defmethod copy! :before ((x standard-tensor) (y array))
  (assert (list-eq (array-dimensions y) (lvec->list (dimensions x)))
	  nil 'dimension-mismatch))

(defmethod copy! ((x array) (y standard-tensor))
  (let ((clname (class-name (class-of y))))
    (assert (member clname *tensor-type-leaves*) nil 'tensor-abstract-class :tensor-class clname)
    (compile-and-eval
     `(defmethod copy! ((x array) (y ,clname))
	(let-typed ((sto-y (store y) :type (simple-array ,(store-element-type clname)))
		    (lst (make-list (array-rank x)) :type cons))
	  (mod-dotimes (idx (dimensions y))
	    :with (linear-sums
		   (of-y (strides y) (head y)))
	    :do (t/store-set ,clname (t/coerce ,(field-type clname) (apply #'aref x (lvec->list! idx lst))) sto-y of-y)))
	y))
    (copy! x y)))

(defmethod copy! ((x standard-tensor) (y array))
  (let ((clname (class-name (class-of x))))
    (assert (member clname *tensor-type-leaves*) nil 'tensor-abstract-class :tensor-class clname)
    (compile-and-eval
     `(defmethod copy! ((x ,clname) (y array))
	(let-typed ((sto-x (store x) :type (simple-array ,(store-element-type clname)))
		    (lst (make-list (array-rank y)) :type cons))
	  (mod-dotimes (idx (dimensions x))
	    :with (linear-sums
		   (of-x (strides x) (head x)))
	    :do (setf (apply #'aref y (lvec->list! idx lst)) (t/store-ref ,clname sto-x of-x))))
	y))
    (copy! x y)))

(defmethod copy! ((x cons) (y standard-tensor))
  ;;You seriously weren't expecting efficiency were you :) ?
  (let ((arr (make-array (list-dimensions x) :initial-contents x)))
    (copy! arr y)))
;;
(defgeneric copy-generic (object type)
  (:documentation
   "
  Syntax
  ======
  (COPY-GENERIC x type)

  Purpose
  =======
  Return a copy of X coerced to TYPE"))

(definline copy (obj &optional type)
  (copy-generic obj type))

(defmethod copy-generic ((num number) type)
  (if type (coerce num type) num))

(defmethod copy-generic ((lst cons) type)
  (cond
    ((member type '(list cons nil)) (copy-tree lst))
    ((eql type 'vector) (make-array (length lst) :initial-contents lst))
    ((eql type 'array)
     (make-array (list-dimensions lst) :initial-contents lst))
    ((subtypep type 'standard-tensor)
     (let ((ret (zeros (list-dimensions lst) type)))
       (copy! lst ret)))
    (t (error "don't know how to copy a list to type ~a" type))))

(defmethod copy-generic ((arr array) type)
  (cond
    ((member type '(array nil))
     (let ((ret (make-array (array-dimensions arr) :element-type (array-element-type arr))))
       (copy! arr ret)))
    ((member type '(list cons))
     (labels ((mtree (arr idx)
		(let ((n (length idx)))
		  (if (= n (array-rank arr)) (apply #'aref arr idx)
		      (loop :for i :from 0 :below (array-dimension arr n)
			 :collect (mtree arr (append idx (list i))))))))
       (mtree arr nil)))
    ((subtypep type 'standard-tensor)
     (let ((ret (zeros (array-dimensions arr) type)))
       (copy! arr ret)))
    (t (error "don't know how to copy a list to type ~a" type))))
