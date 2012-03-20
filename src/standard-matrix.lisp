;; Definitions of STANDARD-MATRIX
(in-package :matlisp)

;;
(declaim (inline allocate-integer4-store))

(eval-when (load eval compile)
  (deftype integer4-matrix-element-type ()
    '(signed-byte 32))
  )

(defun allocate-integer4-store (size &optional (initial-element 0))
  "(ALLOCATE-INTEGER-STORE SIZE [INITIAL-ELEMENT]).  Allocates
integer storage.  Default INITIAL-ELEMENT = 0."
  (make-array size
	      :element-type 'integer4-matrix-element-type
	      :initial-element initial-element))

;;
(declaim (inline store-indexing))
(defun store-indexing (row col head row-stride col-stride)
  (declare (type (and fixnum (integer 0)) row col head row-stride col-stride))
  (the fixnum (+ head (the fixnum (* row row-stride)) (the fixnum (* col col-stride)))))

(defun blas-copyable-p (matrix)
  (declare (optimize (safety 0) (speed 3))
	   (type (or real-matrix complex-matrix) matrix))
  (mlet* ((nr (nrows matrix) :type fixnum)
	  (nc (ncols matrix) :type fixnum)
	  (rs (row-stride matrix) :type fixnum)
	  (cs (col-stride matrix) :type fixnum)
	  (ne (number-of-elements matrix) :type fixnum))
	 (cond
	   ((or (= nc 1) (= cs (* nr rs))) (values t rs ne))
	   ((or (= nr 1) (= rs (* nc cs))) (values t cs ne))
	   (t (values  nil -1 -1)))))

(defun blas-matrix-compatible-p (matrix &optional (op :n))
  (declare (optimize (safety 0) (speed 3))
	   (type (or real-matrix complex-matrix) matrix))
  (mlet* (((rs cs) (slot-values matrix '(row-stride col-stride))
	   :type (fixnum fixnum)))
	 (cond
	   ((= cs 1) (values :row-major rs (fortran-nop op)))
	   ((= rs 1) (values :col-major cs (fortran-op op)))
	   ;;Lets not confound lisp's type declaration.
	   (t (values nil -1 "?")))))

(declaim (inline fortran-op))
(defun fortran-op (op)
  (ecase op (:n "N") (:t "T")))

(declaim (inline fortran-nop))
(defun fortran-nop (op)
  (ecase op (:t "N") (:n "T")))

(defun fortran-snop (sop)
  (cond
    ((string= sop "N") "T")
    ((string= sop "T") "N")
    (t (error "Unrecognised fortran-op."))))

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
    :initform 0
    :accessor number-of-elements
    :type fixnum
    :documentation "Total number of elements in the matrix (nrows * ncols)")
   ;;
   (head
    :initarg :head
    :initform 0
    :accessor head
    :type fixnum
    :documentation "Head for the store's accessor.")
   (row-stride
    :initarg :row-stride
    :accessor row-stride
    :type fixnum
    :documentation "Row stride for the store's accessor.")
   (col-stride
    :initarg :col-stride
    :accessor col-stride
    :type fixnum
    :documentation "Column stride for the store's accessor.")
   (store-size
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

;;
(defmethod initialize-instance :after ((matrix standard-matrix) &rest initargs)
  (declare (ignore initargs))
  (mlet*
   (((nr nc hd ss) (slot-values matrix '(number-of-rows number-of-cols head store-size))
     :type (fixnum fixnum fixnum fixnum)))
   ;;Row-ordered by default.
   (unless (and (slot-boundp matrix 'row-stride) (slot-boundp matrix 'col-stride))
     (setf (row-stride matrix) nc)
     (setf (col-stride matrix) 1))
   (let* ((rs (row-stride matrix))
	  (cs (col-stride matrix))
	  (l-idx (store-indexing (- nr 1) (- nc 1) hd rs cs)))
     (declare (type fixnum rs cs))
     ;;Error checking is good if we use foreign-pointers as store types.
     (cond
       ((<= nr 0) (error "Number of rows must be > 0. Initialized with ~A." nr))
       ((<= nc 0) (error "Number of columns must be > 0. Initialized with ~A." nc))
       ;;
       ((< hd 0) (error "Head of the store must be >= 0. Initialized with ~A." hd))
       ((< rs 0) (error "Row-stride of the store must be >= 0. Initialized with ~A." rs))
       ((< cs 0) (error "Column-stride of the store must be >= 0. Initialized with ~A." cs))
       ((<= ss l-idx) (error "Store is not large enough to hold the matrix.
Initialized with ~A, but the largest possible index is ~A." ss l-idx))))
    ;;
   (setf (number-of-elements matrix) (* nr nc))))

;;
(defmacro matrix-ref (matrix row &optional col)
  (if col
      `(matrix-ref-2d ,matrix ,row ,col)
      `(matrix-ref-1d ,matrix ,row)))

;;
(defgeneric matrix-ref-1d (matrix store-idx)
  (:documentation "
  Syntax
  ======
  (matrix-REF-1d store store-idx)

  Purpose
  =======
  Return the element store-idx of the matrix store."))

#+nil(defmethod matrix-ref-1d :before ((matrix standard-matrix) (idx fixnum))
  (unless (< -1 (- idx (head matrix)) (number-of-elements matrix))
    (error "Requested index ~A is out of bounds.
Matrix only has ~A elements." idx (number-of-elements matrix))))

;;
(defgeneric (setf matrix-ref-1d) (value matrix idx))

#+nil(defmethod (setf matrix-ref-1d) :before ((value t) (matrix standard-matrix) (idx fixnum))
       (unless (< -1 idx (number-of-elements matrix))
	 (error "Requested index ~A is out of bounds.
Matrix only has ~A elements." idx (number-of-elements matrix))))

;;
(defgeneric matrix-ref-2d (matrix rows cols)
  (:documentation "
  Syntax
  ======
  (MATRIX-REF-2d store i j)

  Purpose
  =======
  Return the element
  (+
    (* (row-stride store) i)
    (* (col-stride store) j))
  of the store "))

(defmethod matrix-ref-2d :before ((matrix standard-matrix) (rows fixnum) (cols fixnum))
  (unless (and (< -1 rows (nrows matrix))
	       (< -1 cols (ncols matrix)))
    (error "Requested index (~A ~A) is out of bounds." rows cols)))

(defmethod matrix-ref-2d ((matrix standard-matrix) (rows fixnum) (cols fixnum))
  (matrix-ref-1d matrix (store-indexing rows cols (head matrix) (row-stride matrix) (col-stride matrix))))

;;
(defgeneric (setf matrix-ref-2d) (value matrix rows cols))

(defmethod (setf matrix-ref-2d) ((value t) (matrix standard-matrix) (rows fixnum) (cols fixnum))
  (setf (matrix-ref-1d matrix (store-indexing rows cols (head matrix) (row-stride matrix) (col-stride matrix))) value))

;;
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
#+nil(defmethod print-object ((matrix standard-matrix) stream)
  (dotimes (i (nrows matrix))
    (dotimes (j (ncols matrix))
      (format stream "~A    " (matrix-ref-2d matrix i j)))
    (format stream "~%")))

;;
(defun transpose! (matrix)
"
   Syntax
   ======
   (transpose! matrix)

   Purpose
   =======
   Exchange row and column strides so that effectively
   the matrix is destructively transposed in place
   (without much effort).
"
  (cond
    ((typep matrix 'standard-matrix)
     (progn
       (rotatef (nrows matrix) (ncols matrix))
       (rotatef (row-stride matrix) (col-stride matrix))
       matrix))
    ((typep matrix 'number) matrix)
    (t (error "Don't know how to take the transpose of ~A." matrix))))

(defmacro with-transpose! (matlst &rest body)
  `(progn
     ,@(mapcar #'(lambda (mat) `(transpose! ,mat)) matlst)
     ,@body
     ,@(mapcar #'(lambda (mat) `(transpose! ,mat)) matlst)))

;;
(defgeneric transpose (matrix)
  (:documentation
"
   Syntax
   ======
   (transpose matrix)

   Purpose
   =======
   Create a new matrix object which represents the transpose of the
   the given matrix.

   Store is shared with \"matrix\".

   Settable
   ========
   (setf (transpose matrix) value)

   is basically the same as

   (copy! value (transpose matrix))
"))

(defun (setf transpose) (value matrix)
  (copy! value (transpose matrix)))

(defmethod transpose ((matrix number))
  matrix)

;;
(defgeneric sub-matrix (matrix origin dim)
  (:documentation
"
   Syntax
   ======
   (sub-matrix matrix origin dimensions)

   Purpose
   =======
   Create a block sub-matrix of \"matrix\" starting at \"origin\"
   of dimension \"dim\", sharing the store.

   origin, dim are lists with two elements.

   Store is shared with \"matrix\"

   Settable
   ========
   (setf (sub-matrix matrix origin dim) value)

   is basically the same as

   (copy! value (sub-matrix matrix origin dim))
"))

(defun (setf sub-matrix) (value matrix origin dim)
  (copy! value (sub-matrix matrix origin dim)))

;;
(defgeneric row (matrix i)
  (:documentation
"
   Syntax
   ======
   (row matrix i)

   Purpose
   =======
   Returns the i'th row of the matrix.
   Store is shared with \"matrix\".

   Settable
   ========
   (setf (row matrix i) value)

   is basically the same as

   (copy! value (row matrix i))
"))

(defun (setf row) (value matrix i)
  (copy! value (row matrix i)))

;;
(defgeneric col (matrix j)
  (:documentation
"
   Syntax
   ======
   (col matrix j)

   Purpose
   =======
   Returns the j'th column of the matrix.
   Store is shared with \"matrix\".

   Settable
   ========
   (setf (col matrix j) value)

   is basically the same as

   (copy! value (col matrix j))
"))

(defun (setf col) (value matrix j)
  (copy! value (col matrix j)))

;;
(defgeneric diag (matrix &optional d)
  (:documentation
"
   Syntax
   ======
   (diag matrix &optional (d 0))

   Purpose
   =======
   Returns a row-vector representing the d'th diagonal of the matrix.
   [a_{ij} : j - i = d]

   Store is shared with \"matrix\".

   Settable
   ========
   (setf (diag matrix d) value)

   is basically the same as

   (copy! value (diag matrix d))
"))

(defun (setf diag) (value matrix &optional (d 0))
  (copy! value (diag matrix d)))