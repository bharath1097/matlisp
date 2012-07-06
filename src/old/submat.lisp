(in-package #:matlisp)

;;
(defgeneric sub-matrix~ (matrix origin dim)
  (:documentation
"
   Syntax
   ======
   (SUB-MATRIX~ matrix origin dimensions)

   Purpose
   =======
   Create a block sub-matrix of \"matrix\" starting at \"origin\"
   of dimension \"dim\", sharing the store.

   origin, dim are lists with two elements.

   Store is shared with \"matrix\"

   Settable
   ========
   (setf (SUB-MATRIX~ matrix origin dim) value)

   is basically the same as

   (copy! value (SUB-MATRIX~ matrix origin dim))
"))

(defun sub-matrix (matrix origin dim)
  (copy (sub-matrix~ matrix origin dim)))

(defun (setf sub-matrix~) (value matrix origin dim)
  (copy! value (sub-matrix~ matrix origin dim)))

(defmethod sub-matrix~ ((matrix real-matrix) (origin list) (dim list))
  (destructuring-bind (o-i o-j) origin
    (destructuring-bind (nr-s nc-s) dim
      (mlet* (((hd nr nc rs cs st) (slot-values matrix '(head number-of-rows number-of-cols row-stride col-stride store))
	       :type (fixnum fixnum fixnum fixnum fixnum (real-matrix-store-type *))))
    (unless (and (< -1 o-i (+ o-j nr-s) nr) (< -1 o-j (+ o-j nc-s) nc))
      (error "Bad index and/or size.
Cannot create a sub-matrix of size (~a ~a) starting at (~a ~a)" nr-s nc-s o-i o-j))
    (make-instance 'sub-real-matrix
		   :nrows nr-s :ncols nc-s
		   :store st
		   :head (store-indexing o-i o-j hd rs cs)
		   :row-stride rs :col-stride cs)))))

(defmethod sub-matrix~ ((matrix complex-matrix) (origin list) (dim list))
  (destructuring-bind (o-i o-j) origin
    (destructuring-bind (nr-s nc-s) dim
      (mlet* (((hd nr nc rs cs st) (slot-values matrix '(head number-of-rows number-of-cols row-stride col-stride store))
	       :type (fixnum fixnum fixnum fixnum fixnum (complex-matrix-store-type *))))
    (unless (and (< -1 o-i (+ o-j nr-s) nr) (< -1 o-j (+ o-j nc-s) nc))
      (error "Bad index and/or size.
Cannot create a sub-matrix of size (~a ~a) starting at (~a ~a)" nr-s nc-s o-i o-j))
    (make-instance 'sub-complex-matrix
		   :nrows nr-s :ncols nc-s
		   :store st
		   :head (store-indexing o-i o-j hd rs cs)
		   :row-stride rs :col-stride cs)))))

;;
(defgeneric row~ (matrix i)
  (:documentation
"
   Syntax
   ======
   (ROW~ matrix i)

   Purpose
   =======
   Returns the i'th row of the matrix.
   Store is shared with \"matrix\".

   Settable
   ========
   (setf (ROW~ matrix i) value)

   is basically the same as

   (copy! value (ROW~ matrix i))
"))

(defun row (matrix i)
  (copy (row~ matrix i)))

(defun (setf row~) (value matrix i)
  (copy! value (row~ matrix i)))

(defmethod row~ ((matrix real-matrix) (i fixnum))
  (mlet* (((hd nr nc rs cs st) (slot-values matrix '(head number-of-rows number-of-cols row-stride col-stride store))
	   :type (fixnum fixnum fixnum fixnum fixnum (real-matrix-store-type *))))
	 (unless (< -1 i nr)
	   (error "Index ~a is outside the valid range for the given matrix." i))
	 (make-instance 'sub-real-matrix
			:nrows 1 :ncols nc
			:store st
			:head (store-indexing i 0 hd rs cs)
			:row-stride rs :col-stride cs)))

(defmethod row~ ((matrix complex-matrix) (i fixnum))
  (mlet* (((hd nr nc rs cs st) (slot-values matrix '(head number-of-rows number-of-cols row-stride col-stride store))
	   :type (fixnum fixnum fixnum fixnum fixnum (complex-matrix-store-type *))))
	 (unless (< -1 i nr)
	   (error "Index ~a is outside the valid range for the given matrix." i))
	 (make-instance 'sub-complex-matrix
			:nrows 1 :ncols nc
			:store st
			:head (store-indexing i 0 hd rs cs)
			:row-stride rs :col-stride cs)))

;;
(defgeneric col~ (matrix j)
 (:documentation
"
   Syntax
   ======
   (COL~ matrix j)

   Purpose
   =======
   Returns the j'th column of the matrix.
   Store is shared with \"matrix\".

   Settable
   ========
   (setf (COL~ matrix j) value)

   is basically the same as

   (copy! value (COL~ matrix j))
"))

(defun col (matrix j)
  (copy (col~ matrix j)))

(defun (setf col~) (value matrix j)
  (copy! value (col~ matrix j)))

(defmethod col~ ((matrix real-matrix) (j fixnum))
  (mlet* (((hd nr nc rs cs st) (slot-values matrix '(head number-of-rows number-of-cols row-stride col-stride store))
	   :type (fixnum fixnum fixnum fixnum fixnum (real-matrix-store-type *))))
	 (unless (< -1 j nc)
	   (error "Index ~a is outside the valid range for the given matrix." j))
	 (make-instance 'sub-real-matrix
			:nrows nr :ncols 1
			:store st
			:head (store-indexing 0 j hd rs cs)
			:row-stride rs :col-stride cs)))

(defmethod col~ ((matrix complex-matrix) (j fixnum))
  (mlet* (((hd nr nc rs cs st) (slot-values matrix '(head number-of-rows number-of-cols row-stride col-stride store))
	   :type (fixnum fixnum fixnum fixnum fixnum (complex-matrix-store-type *))))
	 (unless (< -1 j nc)
	   (error "Index ~a is outside the valid range for the given matrix." j))
	 (make-instance 'sub-complex-matrix
			:nrows nr :ncols 1
			:store st
			:head (store-indexing 0 j hd rs cs)
			:row-stride rs :col-stride cs)))

;;
(defgeneric diag~ (matrix &optional d)
  (:documentation
"
   Syntax
   ======
   (DIAG~ matrix &optional (d 0))

   Purpose
   =======
   Returns a row-vector representing the d'th diagonal of the matrix.
   [a_{ij} : j - i = d]

   Store is shared with \"matrix\".

   Settable
   ========
   (setf (DIAG~ matrix d) value)

   is basically the same as

   (copy! value (DIAG~ matrix d))
"))

(defun diag (matrix &optional d)
  (copy (diag~ matrix d)))

(defun (setf diag~) (value matrix &optional (d 0))
  (copy! value (diag~ matrix d)))

(defmethod diag~ ((matrix real-matrix) &optional (d 0))
  (declare (type fixnum d))
  (mlet* (((hd nr nc rs cs st) (slot-values matrix '(head number-of-rows number-of-cols row-stride col-stride store))
	   :type (fixnum fixnum fixnum fixnum fixnum (real-matrix-store-type *)))
	  ((f-i f-j) (if (< d 0)
			 (values (- d) 0)
			 (values 0 d))
	   :type (fixnum fixnum)))
	 (unless (and (< -1 f-i nr) (< -1 f-j nc))
	   (error "Index ~a is outside the valid range for the given matrix." d))
	 (let ((d-s (min (- nr f-i) (- nc f-j))))
	   (declare (type fixnum d-s))
	   (make-instance 'sub-real-matrix
			  :nrows 1 :ncols d-s
			  :store st
			  :head (store-indexing f-i f-j hd rs cs)
			  :row-stride 1 :col-stride (+ rs cs)))))


(defmethod diag~ ((matrix complex-matrix) &optional (d 0))
  (declare (type fixnum d))
  (mlet* (((hd nr nc rs cs st) (slot-values matrix '(head number-of-rows number-of-cols row-stride col-stride store))
	   :type (fixnum fixnum fixnum fixnum fixnum (complex-matrix-store-type *)))
	  ((f-i f-j) (if (< d 0)
			 (values (- d) 0)
			 (values 0 d))
	   :type (fixnum fixnum)))
	 (unless (and (< -1 f-i nr) (< -1 f-j nc))
	   (error "Index ~a is outside the valid range for the given matrix." d))
	 (let ((d-s (min (- nr f-i) (- nc f-j))))
	   (declare (type fixnum d-s))
	   (make-instance 'sub-complex-matrix
			  :nrows 1 :ncols d-s
			  :store st
			  :head (store-indexing f-i f-j hd rs cs)
			  :row-stride 1 :col-stride (+ rs cs)))))
