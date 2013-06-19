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
  (let-rec cdr-writer ((flst from) (tlst to))
    (unless (or (null flst) (null tlst))
      (setf (car tlst) (car flst))
      (cdr-writer (cdr flst) (cdr tlst))))
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

;;
(defgeneric copy (object)
  (:documentation 
   "
  Syntax
  ======
  (COPY x)
 
  Purpose
  =======
  Return a copy of X"))

(defmethod copy ((lst cons))
  (copy-list lst))

(defmethod copy ((arr array))
  (let ((ret (make-array (array-dimensions arr) :element-type (array-element-type arr))))
    (copy! arr ret)))
