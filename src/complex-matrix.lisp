;;; Definitions of COMPLEX-MATRIX.

(in-package "MATLISP")

(eval-when (load eval compile)
(deftype complex-matrix-element-type ()
  "The type of the elements stored in a COMPLEX-MATRIX"
  'double-float)

(deftype complex-matrix-store-type (size)
  "The type of the storage structure for a COMPLEX-MATRIX"
  `(simple-array double-float ,size))
)

;;
(defclass complex-matrix (standard-matrix)
  ((store
    :type (simple-array complex-matrix-element-type (*))))
  (:documentation "A class of matrices with complex elements."))

;;
(declaim (ftype (function (real-matrix) fixnum)
		store-size))
(declaim (ftype (function (real-matrix) (simple-array double-float (*)))
		 store))

;;
(defmethod fill-matrix ((matrix complex-matrix) (fill number))
  (copy! fill matrix))

;;
(declaim (inline allocate-complex-store))
(defun allocate-complex-store (size)
  (make-array (* 2 size) :element-type 'complex-matrix-element-type
	      :initial-element (coerce 0 'complex-matrix-element-type)))

;;
(defun make-complex-matrix-dim (n m &optional (fill #c(0.0d0 0.0d0)))
  "
  Syntax
  ======
  (MAKE-COMPLEX-MATRIX-DIM n m [fill-element])

  Purpose
  =======
  Creates an NxM COMPLEX-MATRIX with initial contents FILL-ELEMENT,
  the default #c(0.0d0 0.0d0)

  See MAKE-COMPLEX-MATRIX.
"
  (declare (type fixnum n m))
  (let* ((size (* n m))
	 (store (allocate-complex-store size))
	 (matrix (make-instance 'complex-matrix :nrows n :ncols m :store store)))

    (fill-matrix matrix fill)
    matrix))

;;
(defun make-complex-matrix-array (array)
  " 
  Syntax
  ======
  (MAKE-COMPLEX-MATRIX-ARRAY array)

  Purpose
  =======
  Creates a COMPLEX-MATRIX with the same contents as ARRAY.
"
  (let* ((n (array-dimension array 0))
	 (m (array-dimension array 1))
	 (size (* n m))
	 (store (allocate-complex-store size)))
    (declare (type fixnum n m size)
	     (type (complex-matrix-store-type (*)) store))
    (dotimes (i n)
      (declare (type fixnum i))
      (dotimes (j m)
	(declare (type fixnum j))
	(let* ((val (complex-coerce (aref array i j)))
	       (realpart (realpart val))
	       (imagpart (imagpart val))
	       (index (fortran-complex-matrix-indexing i j n)))
	    (declare (type complex-matrix-element-type realpart imagpart)
		     (type (complex complex-matrix-element-type) val)
		     (type fixnum index))
	  (setf (aref store index) realpart)
	  (setf (aref store (1+ index)) imagpart))))
    
    (make-instance 'complex-matrix :nrows n :ncols m :store store)))

;;
(defun make-complex-matrix-seq-of-seq (seq)
  (let* ((n (length seq))
	 (m (length (elt seq 0)))
	 (size (* n m))
	 (store (allocate-complex-store size)))
    (declare (type fixnum n m size)
	     (type (complex-matrix-store-type (*)) store))
    
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
		 (index (fortran-complex-matrix-indexing i j n)))
	    (declare (type complex-matrix-element-type realpart imagpart)
		     (type (complex complex-matrix-element-type) val)
		     (type fixnum index))
	    (setf (aref store index) realpart)
	    (setf (aref store (1+ index)) imagpart)))))
    
    (make-instance 'complex-matrix :nrows n :ncols m :store store)))

;;
(defun make-complex-matrix-seq (seq)
  (let* ((n (length seq))
	 (store (allocate-complex-store n)))
    (declare (type fixnum n)
	     (type (complex-matrix-store-type (*)) store))
    
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
    
    (make-instance 'complex-matrix :nrows n :ncols 1 :store store)))

;;
(defun make-complex-matrix-sequence (seq)
  (cond ((or (listp seq) (vectorp seq))
	 (let ((peek (elt seq 0)))
	   (cond ((or (listp peek) (vectorp peek))
		  ;; We have a seq of seqs
		  (make-complex-matrix-seq-of-seq seq))
		 (t
		  ;; Assume a simple sequence
		  (make-complex-matrix-seq seq)))))
	((arrayp seq)
	 (make-complex-matrix-array seq))))

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