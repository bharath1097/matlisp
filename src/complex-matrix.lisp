;;; Definitions of COMPLEX-MATRIX.

(in-package :matlisp)

(eval-when (load eval compile)
(deftype complex-matrix-element-type ()
  "The type of the elements stored in a COMPLEX-MATRIX"
  'double-float)

(deftype complex-matrix-store-type (size)
  "The type of the storage structure for a COMPLEX-MATRIX"
  `(simple-array double-float (,size)))

(deftype complex-double-float ()
  '(cl:complex (double-float * *)))
)

;;
(declaim (inline complex-coerce)
	 (ftype (function (number) (complex complex-matrix-element-type)) 
		complex-coerce))
(defun complex-coerce (val)
  "
 Syntax
 ======
 (COMPLEX-COERCE number)

 Purpose
 =======
 Coerce NUMBER to a complex number.
"
  (declare (type number val))
  (typecase val
    ((complex complex-matrix-element-type) val)
    (complex (complex (coerce (realpart val) 'complex-matrix-element-type)
		      (coerce (imagpart val) 'complex-matrix-element-type)))
    (t (complex (coerce val 'complex-matrix-element-type) 0.0d0))))

;;
(defclass complex-matrix (standard-matrix)
  ((store
    :initform nil
    :type (complex-matrix-store-type *)))
  (:documentation "A class of matrices with complex elements."))

(defclass sub-complex-matrix (complex-matrix)
  ((parent-matrix
    :initarg :parent
    :accessor parent
    :type complex-matrix))
  (:documentation "A class of matrices with complex elements."))

;;
(defmethod initialize-instance ((matrix complex-matrix) &rest initargs)
  (setf (store-size matrix) (/ (length (get-arg :store initargs)) 2))
  (call-next-method))

;;
(defmethod matrix-ref-1d ((matrix complex-matrix) (idx fixnum))
  (let ((store (store matrix)))
    (declare (type (complex-matrix-store-type *) store))
    (complex (aref store (* 2 idx)) (aref store (+ 1 (* 2 idx))))))

(defmethod (setf matrix-ref-1d) ((value number) (matrix complex-matrix) (idx fixnum))
  (let ((store (store matrix))
	(coerced-value (complex-coerce value)))
    (declare (type (complex-matrix-store-type *) store))
    (setf (aref store (* 2 idx)) (realpart coerced-value)
	  (aref store (+ 1 (* 2 idx))) (imagpart coerced-value))))

;;
(defmethod transpose ((matrix complex-matrix))
  (mlet* (((hd nr nc rs cs st) (slot-values matrix '(head number-of-rows number-of-cols row-stride col-stride store))
	   :type (fixnum fixnum fixnum fixnum fixnum (complex-matrix-store-type *))))
	 (make-instance 'sub-complex-matrix
			:nrows nc :ncols nr
			:store st
			:head hd
			:row-stride cs :col-stride rs
			:parent matrix)))

;;
(defmethod sub-matrix ((matrix complex-matrix) (origin list) (dim list))
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
(defmethod row ((matrix complex-matrix) (i fixnum))
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
(defmethod col ((matrix complex-matrix) (j fixnum))
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
(defmethod diag ((matrix complex-matrix) &optional (d 0))
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

;;
(declaim (inline allocate-complex-store))
(defun allocate-complex-store (size)
  (make-array (* 2 size) :element-type 'complex-matrix-element-type
	      :initial-element (coerce 0 'complex-matrix-element-type)))

;;
(defmethod fill-matrix ((matrix complex-matrix) (fill number))
  (copy! fill matrix))

;;
(defun make-complex-matrix-dim (n m &key (fill #c(0.0d0 0.0d0)) (order :row-major))
  "
  Syntax
  ======
  (MAKE-COMPLEX-MATRIX-DIM n m {fill-element #C(0d0 0d0)} {order :row-major})

  Purpose
  =======
  Creates an NxM COMPLEX-MATRIX with initial contents FILL-ELEMENT,
  the default #c(0.0d0 0.0d0), in the row-major order by default.

  See MAKE-COMPLEX-MATRIX.
"
  (declare (type fixnum n m))
  (let* ((size (* n m))
	 (store (allocate-complex-store size)))
    (multiple-value-bind (row-stride col-stride)
	(ecase order
	  (:row-major (values m 1))
	  (:col-major (values 1 n)))
      (let ((matrix
	     (make-instance 'complex-matrix
			    :nrows n :ncols m
			    :row-stride row-stride :col-stride col-stride
			    :store store)))
	(fill-matrix matrix fill)
	matrix))))

;;
(defun make-complex-matrix-array (array &key (order :row-major))
  " 
  Syntax
  ======
  (MAKE-COMPLEX-MATRIX-ARRAY array {order :row-major})

  Purpose
  =======
  Creates a COMPLEX-MATRIX with the same contents as ARRAY,
  in row-major order by default.
"
  (let* ((n (array-dimension array 0))
	 (m (array-dimension array 1))
	 (size (* n m))
	 (store (allocate-complex-store size)))
    (declare (type fixnum n m size)
	     (type (complex-matrix-store-type *) store))
    (multiple-value-bind (row-stride col-stride)
	(ecase order
	  (:row-major (values m 1))
	  (:col-major (values 1 n)))
      (dotimes (i n)
	(declare (type fixnum i))
	(dotimes (j m)
	  (declare (type fixnum j))
	  (let* ((val (complex-coerce (aref array i j)))
		 (realpart (realpart val))
		 (imagpart (imagpart val))
		 (index (* 2 (store-indexing i j 0 row-stride col-stride))))
	    (declare (type complex-matrix-element-type realpart imagpart)
		     (type (complex complex-matrix-element-type) val)
		     (type fixnum index))
	    (setf (aref store index) realpart)
	    (setf (aref store (1+ index)) imagpart))))
      (make-instance 'complex-matrix
		     :nrows n :ncols m
		     :row-stride row-stride :col-stride col-stride
		     :store store))))

;;
(defun make-complex-matrix-seq-of-seq (seq &key (order :row-major))
  (let* ((n (length seq))
	 (m (length (elt seq 0)))
	 (size (* n m))
	 (store (allocate-complex-store size)))
    (declare (type fixnum n m size)
	     (type (complex-matrix-store-type *) store))
    (multiple-value-bind (row-stride col-stride)
	(ecase order
	  (:row-major (values m 1))
	  (:col-major (values 1 n)))
      (dotimes (i n)
	(declare (type fixnum i))
	(let ((this-row (elt seq i)))
	  (unless (= (length this-row) m)
	    (error "Number of columns is not the same for all rows!"))
	  (dotimes (j m)
	    (declare (type fixnum j))
	    (let* ((val (complex-coerce (elt this-row j)))
		   (realpart (realpart val))
		   (imagpart (imagpart val))
		   (index (* 2 (store-indexing i j 0 row-stride col-stride))))
	    (declare (type complex-matrix-element-type realpart imagpart)
		     (type (complex complex-matrix-element-type) val)
		     (type fixnum index))
	    (setf (aref store index) realpart)
	    (setf (aref store (1+ index)) imagpart)))))
      (make-instance 'complex-matrix
		     :nrows n :ncols m
		     :row-stride row-stride :col-stride col-stride
		     :store store))))

;;
(defun make-complex-matrix-seq (seq &key (order :row-major))
  (let* ((n (length seq))
	 (store (allocate-complex-store n)))
    (declare (type fixnum n)
	     (type (complex-matrix-store-type *) store))
    (dotimes (k n)
      (declare (type fixnum k))
      (let* ((val (complex-coerce (elt seq k)))
	     (realpart (realpart val))
	     (imagpart (imagpart val))
	     (index (* 2 k)))
	(declare (type complex-matrix-element-type realpart imagpart)
		 (type (complex complex-matrix-element-type) val)
		 (type fixnum index))
	(setf (aref store index) realpart)
	(setf (aref store (1+ index)) imagpart)))
    
    (ecase order
      (:row-major (make-instance 'complex-matrix
				 :nrows 1 :ncols n
				 :row-stride n :col-stride 1
				 :store store))
      (:col-major (make-instance 'complex-matrix
				 :nrows n :ncols 1
				 :row-stride 1 :col-stride n
				 :store store)))))

;;
(defun make-complex-matrix-sequence (seq &key (order :row-major))
  (cond ((or (listp seq) (vectorp seq))
	 (let ((peek (elt seq 0)))
	   (cond ((or (listp peek) (vectorp peek))
		  ;; We have a seq of seqs
		  (make-complex-matrix-seq-of-seq seq :order order))
		 (t
		  ;; Assume a simple sequence
		  (make-complex-matrix-seq seq :order order)))))
	((arrayp seq)
	 (make-complex-matrix-array seq :order order))))

;;
(defun make-complex-matrix (&rest args)
  "
 Syntax
 ======
 (MAKE-COMPLEX-MATRIX {arg}*)

 Purpose
 =======
 Create a FLOAT-MATRIX.

 Examples
 ========

 (make-complex-matrix n)
        square NxN matrix
 (make-complex-matrix n m)
        NxM matrix
 (make-complex-matrix '((1 2 3) (4 5 6)))
        2x3 matrix:

              1 2 3
              4 5 6

 (make-complex-matrix #((1 2 3) (4 5 6)))
        2x3 matrix:

              1 2 3
              4 5 6

 (make-complex-matrix #((1 2 3) #(4 5 6)))
        2x3 matrix:

              1 2 3
              4 5 6

 (make-complex-matrix #2a((1 2 3) (4 5 6)))
        2x3 matrix:

              1 2 3
              4 5 6

"
  (let ((nargs (length args)))
    (case nargs
      (1
       (let ((arg (first args)))
	 (typecase arg
	   (integer
	    (assert (not (minusp arg)) nil
		    "matrix dimension must be non-negative, not ~A" arg)
	    (make-complex-matrix-dim arg arg))
	   (sequence
	    (make-complex-matrix-sequence arg))
	   ((array * (* *))
	    (make-complex-matrix-array arg))
	   (t (error "don't know how to make matrix from ~a" arg)))))
      (2
       (destructuring-bind (n m)
	   args
	 (assert (and (typep n '(integer 0))
		      (typep n '(integer 0)))
		 nil
		 "cannot make a ~A x ~A matrix" n m)
	 (make-complex-matrix-dim n m)))
      (t
       (error "require 1 or 2 arguments to make a matrix")))))

;;

(defun mrealpart (mat)
  (typecase mat
    (real-matrix mat)
    (complex-matrix (make-instance 'sub-real-matrix
				   :parent mat :store (store mat)
				   :nrows (nrows mat) :ncols (ncols mat)
				   :row-stride (* 2 (row-stride mat)) :col-stride (* 2 (col-stride mat))
				   :head (* 2 (head mat))))
    (number (cl:realpart mat))))

(defun mimagpart (mat)
  (typecase mat
    (real-matrix nil)
    (complex-matrix (make-instance 'sub-real-matrix
				   :parent mat :store (store mat)
				   :nrows (nrows mat) :ncols (ncols mat)
				   :row-stride (* 2 (row-stride mat)) :col-stride (* 2 (col-stride mat))
				   :head (+ 1 (* 2 (head mat)))))
    (number (cl:imagpart mat))))

(defun mconjugate! (mat)
  (typecase mat
    (real-matrix mat)
    (complex-matrix (scal! -1d0 (mimagpart mat))))
  mat)