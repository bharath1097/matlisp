(in-package #:matlisp)

(defgeneric copy! (from-tensor to-tensor)
  (:documentation
   "
  Syntax
  ======
  (COPY! x y)

  Purpose
  =======
  Copies the contents of X into Y. Returns Y.

  X,Y must have the same dimensions, and
  ergo the same number of elements.

  Furthermore, X may be a scalar, in which
  case Y is filled with X.
")
  (:method :before ((x cons) (y cons))
	   (assert (= (length x) (length y))))
  (:method :before ((x array) (y array))
	   (assert (subtypep (array-element-type x) (array-element-type y))
		   nil 'invalid-type
		   :given (array-element-type y) :expected (array-element-type x))
	   (assert (and
		    (= (array-rank x) (array-rank y))
		    (reduce #'(lambda (x y) (and x y))
			    (mapcar #'= (array-dimensions x) (array-dimensions y))))
		   nil 'dimension-mismatch)))

(defmethod copy! ((from cons) (to cons))
  (let-rec cdr-writer ((flst from) (tlst to))
	   (if (null flst) to
	       (progn
		 (rplaca tlst (car flst))
		 (cdr-writer (cdr flst) (cdr tlst))))))

(defmethod copy! (from (to cons))
  (mapl #'(lambda (lst) (rplaca lst from)) to)
  to)

(defmethod copy! ((from array) (to array))
  (let ((lst (make-list (array-rank to))))
    (mod-dotimes (idx (make-index-store (array-dimensions to)))
      do (progn
	   (lvec->list! idx lst)
	   (setf (apply #'aref to lst) (apply #'aref from lst)))))
  to)

(defmethod copy! (from (to array))
  (let ((lst (make-list (array-rank to))))
    (mod-dotimes (idx (make-index-store (array-dimensions to)))
      do (progn
	   (lvec->list! idx lst)
	   (setf (apply #'aref to lst) from)))
  to))

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
