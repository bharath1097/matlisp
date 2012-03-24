;; Definitions of STANDARD-MATRIX
;;(in-package :matlisp)

;;
(declaim (inline allocate-integer4-store))

(eval-when (load eval compile)
  (deftype integer4-matrix-element-type ()
    '(signed-byte 32))
  
  (deftype index-type ()
    'fixnum)
  (deftype index-array-type (size)
    '(simple-array index-type (,size)))
  )

(defun allocate-integer4-store (size &optional (initial-element 0))
  "(ALLOCATE-INTEGER-STORE SIZE [INITIAL-ELEMENT]).  Allocates
integer storage.  Default INITIAL-ELEMENT = 0."
  (make-array size
	      :element-type 'integer4-matrix-element-type
	      :initial-element initial-element))

;;
(defclass standard-tensor ()
  ((rank
    :accessor rank
    :type index-type
    :documentation "Rank of the matrix: number of arguments for the tensor")
   (dimensions
    :accessor dimensions
    :initarg :dimensions
    :type (index-array-type *)
    :documentation "Dimensions of the vector spaces in which the tensor's arguments reside.")
   (number-of-elements
    :accessor number-of-elements
    :type fixnum
    :documentation "Total number of elements in the tensor.")
   ;;
   (head
    :initarg :head
    :initform 0
    :accessor head
    :type index-type
    :documentation "Head for the store's accessor.")
   (strides
    :initarg :strides
    :accessor strides
    :type (index-array-type *)
    :documentation "Strides for accesing elements of the tensor.")
   (store-size
    :accessor store-size
    :type fixnum
    :documentation "Size of the store.")
   (store
    :initarg :store
    :accessor store
    :documentation "The actual storage for the tensor."))
  (:documentation "Basic matrix class."))

;;
(defmethod initialize-instance :after ((tensor standard-tensor) &rest initargs)
  (declare (ignore initargs))
  (mlet*
   (((dimensions hd) (slot-values tensor '(dimensions head))
     :type ((index-array-type *) fixnum fixnum)))
   (unless (slot-boundp tensor 'rank)
     (setf (rank tensor) (len
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