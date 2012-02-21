;;; Definitions of STANDARD-MATRIX

(in-package "MATLISP")

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
(declaim (inline fortran-matrix-indexing))
(defun fortran-matrix-indexing (row col nrows)
  (declare (type (and fixnum (integer 0)) row col nrows))
  (the fixnum (+ row (the fixnum (* col nrows)))))

(declaim (inline fortran-complex-matrix-indexing))
(defun fortran-complex-matrix-indexing (row col nrows)
  (declare (type (and fixnum (integer 0)) row col nrows))
  (the fixnum (* 2 (the fixnum (+ row (the fixnum (* col nrows)))))))

;;
(defclass standard-matrix ()
  ((number-of-rows
    :initarg :nrows
    :initform 0
    :accessor nrows
    :type fixnum
    :documentation "Number of rows in the matrix")
   (number-of-cols
    :initarg :ncols
    :initform 0
    :accessor ncols
    :type fixnum
    :documentation "Number of columns in the matrix")
   (number-of-elements
    :initarg :nels
    :initform 0
    :accessor number-of-elements
    :type fixnum
    :documentation "Total number of elements in the matrix (nrows * ncols)")
   (store-size
    :initarg :store-size
    :initform 0
    :accessor store-size
    :type fixnum
    :documentation "Total number of elements needed to store the matrix.  (Usually
the same as nels, but not necessarily so!")
   (store
    :initarg :store
    :accessor store
    :documentation "The actual storage for the matrix.  It is typically a one dimensional
array but not necessarily so.  The float and complex matrices do use
1-D arrays.  The complex matrix actually stores the real and imaginary
parts in successive elements of the matrix because Fortran stores them
that way."))
  (:documentation "Basic matrix class."))

(declaim (ftype (function (standard-matrix) fixnum)
		store-size))

#+(and (or cmu sbcl) gerds-pcl)
(declaim (ext:slots (slot-boundp real-matrix complex-matrix)
		    (inline standard-matrix real-matrix complex-matrix)))

;;
(declaim (ftype (function (standard-matrix) boolean)
		row-vector-p))
(defgeneric row-vector-p (matrix)
  (:documentation "
  Syntax
  ======
  (ROW-VECTOR-P x)

  Purpose
  =======
  Return T if X is a row vector (number of columns is 1)"))

(declaim (inline row-vector-p))
(defmethod row-vector-p ((matrix standard-matrix))
  (= (nrows matrix) 1))

;;
(declaim (ftype (function (standard-matrix) boolean)
		col-vector-p))
(defgeneric col-vector-p (matrix)
  (:documentation "
  Syntax
  ======
  (COL-VECTOR-P x)
 
  Purpose
  =======
  Return T if X is a column vector (number of rows is 1)"))

(declaim (inline col-vector-p))
(defmethod col-vector-p ((matrix standard-matrix))
  (= (ncols matrix) 1))

;;
(declaim (ftype (function (standard-matrix) boolean)
		row-or-col-vector-p))
(defgeneric row-or-col-vector-p (matrix)
  (:documentation "
  Syntax
  ======
  (ROW-OR-COL-VECTOR-P x)

  Purpose
  =======
  Return T if X is either a row or a column vector"))

(declaim (inline row-or-col-vector-p))
(defmethod row-or-col-vector-p ((matrix standard-matrix))
  (or (row-vector-p matrix) (col-vector-p matrix)))

;;
(declaim (ftype (function (standard-matrix) boolean)
		square-matrix-p))
(defgeneric square-matrix-p (matrix)
  (:documentation "
  Syntax
  ======
  (SQUARE-MATRIX-P x)

  Purpose
  =======
  Return T if X is square matrix"))

(declaim (inline square-matrix-p))
(defmethod square-matrix-p ((matrix standard-matrix))
  (= (nrows matrix) (ncols matrix)))

;;
(defgeneric size (matrix)
  (:documentation "
  Syntax
  ======
  (SIZE x)

  Purpose
  =======
  Return the number of rows and columns of the matrix X as a list"))

(defmethod size ((matrix standard-matrix))
  (list (nrows matrix) (ncols matrix)))

;;
(defgeneric fill-matrix (matrix fill-element)
  (:documentation 
   "
   Syntax
   ======
   (FILL-MATRIX matrix fill-element)
  
   Purpose
   =======
   Fill MATRIX with FILL-ELEMENT.
"))

(defmethod fill-matrix ((matrix t) (fill t))
  (error "arguments MATRIX and FILL to FILL-MATRIX must be a
matrix and a number"))

;;
(defmethod make-load-form ((matrix standard-matrix) &optional env)
  "MAKE-LOAD-FORM allows us to determine a load time value for
   matrices, for example #.(make-matrix ...)"
  (make-load-form-saving-slots matrix :environment env))

;;
(defmethod initialize-instance :after ((matrix standard-matrix) &rest initargs)
  (declare (ignore initargs))
  (let* ((n (nrows matrix))
	 (m (ncols matrix))
	 (nxm (* n m)))
    (declare (type fixnum n m nxm))
    (setf (number-of-elements matrix) nxm)
    (setf (store-size matrix) nxm)))

;;
(defgeneric matrix-ref-1d (matrix row)
  (:documentation "
  Syntax
  ======
  (MATRIX-REF matrix rows [cols])

  Purpose
  =======
  Return the element(s) of the matrix MAT, specified by the ROWS and COLS.
  If ROWS and/or COLS are matrices or sequences then the submatrix indexed
  by them will be returned.

  The indices are 0-based."))

(defgeneric (setf matrix-ref-1d) (value matrix rows))

;;
(defgeneric matrix-ref-2d (matrix row cols)
  (:documentation "
  Syntax
  ======
  (MATRIX-REF matrix rows cols)

  Purpose
  =======
  Return the element(s) of the matrix MAT, specified by the ROWS and COLS.
  If ROWS and/or COLS are matrices or sequences then the submatrix indexed
  by them will be returned.

  The indices are 0-based."))

(defgeneric (setf matrix-ref-2d) (value matrix rows cols))