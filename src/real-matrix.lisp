;;; Definitions of REAL-MATRIX.

(in-package :expt)

(eval-when (load eval compile)
  (deftype real-matrix-element-type ()
    "The type of the elements stored in a REAL-MATRIX"
    'double-float)
  
  (deftype real-matrix-store-type (size)
    "The type of the storage structure for a REAL-MATRIX"
    `(simple-array double-float ,size))
  )

;;
(defclass real-matrix (standard-matrix)
  ((store
    :initform nil
    :type (simple-array real-matrix-element-type (*))))
  (:documentation "A class of matrices with real elements."))

;;
(defmethod initialize-instance ((matrix real-matrix) &rest initargs)
  (setf (store-size matrix) (length (get-arg :store initargs)))
  (call-next-method))

;;
(defmethod matrix-ref-1d ((matrix real-matrix) (store-idx fixnum))
  (let ((store (store matrix)))
    (declare (type (real-matrix-store-type (*)) store))
    (aref store store-idx)))


(defmethod (setf matrix-ref-1d) ((value cl:real) (matrix real-matrix) (store-idx fixnum))
  (let ((store (store matrix)))
    (declare (type (real-matrix-store-type (*)) store))
    (setf (aref store store-idx) value)))

;;
(declaim (inline allocate-real-store))
(defun allocate-real-store (size &optional (initial-element 0))
  (make-array size :element-type 'real-matrix-element-type
	      :initial-element (coerce initial-element 'real-matrix-element-type)))

;;
(defmethod fill-matrix ((matrix real-matrix) (fill cl:real))
  (copy! fill matrix))

(defmethod fill-matrix ((matrix real-matrix) (fill complex))
  (error "cannot fill a real matrix with a complex number,
don't know how to coerce COMPLEX to REAL"))

;;
(defun make-real-matrix-dim (n m &key (fill 0.0d0) (order :row-major))
  "
  Syntax
  ======
  (MAKE-REAL-MATRIX-DIM n m [fill-element])

  Purpose
  =======
  Creates an NxM REAL-MATRIX with initial contents FILL-ELEMENT,
  the default 0.0d0

  See MAKE-REAL-MATRIX.
"  
  (declare (type fixnum n m))
  (let ((casted-fill
	 (typecase fill
	   (real-matrix-element-type fill)
	   (cl:real (coerce fill 'real-matrix-element-type))
	   (t (error "argument FILL-ELEMENT to MAKE-REAL-MATRIX-DIM must be a REAL")))))
    (declare (type real-matrix-element-type casted-fill))
    (multiple-value-bind (row-stride col-stride)
	(ecase order
	  (:row-major (values n 1))
	  (:col-major (values 1 m)))
      (make-instance 'real-matrix
		     :nrows n :ncols m
		     :row-stride row-stride :col-stride col-stride
		     :store (allocate-real-store (* n m) casted-fill)))))

;;; Make a matrix from a 2-D Lisp array
(defun make-real-matrix-array (array &key (order :row-major))
  " 
  Syntax
  ======
  (MAKE-REAL-MATRIX-ARRAY array)

  Purpose
  =======
  Creates a REAL-MATRIX with the same contents as ARRAY.
"
  (let* ((n (array-dimension array 0))
	 (m (array-dimension array 1))
	 (size (* n m))
	 (store (allocate-real-store size)))
    (declare (type fixnum n m size)
	     (type (real-matrix-store-type (*)) store))
    (multiple-value-bind (row-stride col-stride)
	(ecase order
	  (:row-major (values m 1))
	  (:col-major (values 1 n)))
      (dotimes (i n)
	(declare (type fixnum i))
	(dotimes (j m)
	  (declare (type fixnum j))
	  (setf (aref store (store-indexing i j 0 row-stride col-stride))
		(coerce (aref array i j) 'real-matrix-element-type))))
      (make-instance 'real-matrix
		     :nrows n :ncols m
		     :row-stride row-stride :col-stride col-stride
		     :store store))))

;;
(defun make-real-matrix-seq-of-seq (seq &key (order :row-major))
  (let* ((n (length seq))
	 (m (length (elt seq 0)))
	 (size (* n m))
	 (store (allocate-real-store size)))
    (declare (type fixnum n m size)
	     (type (real-matrix-store-type (*)) store))
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
	    (setf (aref store (store-indexing i j 0 row-stride col-stride))
		  (coerce (elt this-row j) 'real-matrix-element-type)))))
            (make-instance 'real-matrix
		     :nrows n :ncols m
		     :row-stride row-stride :col-stride col-stride
		     :store store))))

;;
(defun make-real-matrix-seq (seq &key (order :row-major))
  (let* ((n (length seq))
	 (store (allocate-real-store n)))
    (declare (type fixnum n))
    (dotimes (k n)
      (declare (type fixnum k))
      (setf (aref store k) (coerce (elt seq k) 'real-matrix-element-type)))
    (ecase order
      (:row-major (make-instance 'real-matrix
				 :nrows 1 :ncols n
				 :row-stride n :col-stride 1
				 :store store))
      (:col-major (make-instance 'real-matrix
				 :nrows n :ncols 1
				 :row-stride 1 :col-stride n
				 :store store)))))
		  

;;
(defun make-real-matrix-sequence (seq &key (order :row-major))
  (cond ((or (listp seq) (vectorp seq))
	 (let ((peek (elt seq 0)))
	   (cond ((or (listp peek) (vectorp peek))
		  ;; We have a seq of seqs
		  (make-real-matrix-seq-of-seq seq :order order))
		 (t
		  ;; Assume a simple sequence
		  (make-real-matrix-seq seq :order order)))))
	((arrayp seq)
	 (make-real-matrix-array seq :order order))))

;;
(defun make-real-matrix (&rest args)
  "
 Syntax
 ======
 (MAKE-REAL-MATRIX {arg}*)

 Purpose
 =======
 Create a REAL-MATRIX.

 Examples
 ========

 (make-real-matrix n)
        square NxN matrix
 (make-real-matrix n m)
        NxM matrix
 (make-real-matrix '((1 2 3) (4 5 6)))
        2x3 matrix:

              1 2 3
              4 5 6

 (make-real-matrix #((1 2 3) (4 5 6)))
        2x3 matrix:

              1 2 3
              4 5 6

 (make-real-matrix #((1 2 3) #(4 5 6)))
        2x3 matrix:

              1 2 3
              4 5 6

 (make-real-matrix #2a((1 2 3) (4 5 6)))
        2x3 matrix:

              1 2 3
              4 5 6
 (make-real-matrix #(1 2 3 4))
        4x1 matrix (column vector)

          1
          2
          3
          4

 (make-real-matrix #((1 2 3 4))
        1x4 matrix (row vector)

          1 2 3 4
"
  (let ((nargs (length args)))
    (case nargs
      (1
       (let ((arg (first args)))
	 (typecase arg
	   (integer
	    (assert (not (minusp arg)) nil
		    "matrix dimension must be positive, not ~A" arg)
	    (make-real-matrix-dim arg arg))
	   (sequence
	    (make-real-matrix-sequence arg))
	   ((array * (* *))
	    (make-real-matrix-array arg))
	   (t (error "don't know how to make matrix from ~a" arg)))))
      (2
       (destructuring-bind (n m)
	   args
	 (assert (and (typep n '(integer 0))
		      (typep n '(integer 0)))
		 nil
		 "cannot make a ~A x ~A matrix" n m)
	 (make-real-matrix-dim n m)))
      (t
       (error "require 1 or 2 arguments to make a matrix")))))