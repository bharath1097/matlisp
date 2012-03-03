;; Definitions of STANDARD-MATRIX
;;(in-package "MATLISP")

(defpackage matlisp-experimental
  (:nicknames :expt)
  (:use "COMMON-LISP")
;  (:shadowing-import-from "MATLISP" "REAL"))
  )

(in-package :expt)

(defun get-arg (sym arglist)
  (check-type sym symbol)
  (locally
      (declare (optimize (speed 3) (safety 0)))
    (labels ((get-sym (sym arglist)
	       (cond	       
		 ((null arglist) nil)
		 ((eq (car arglist) sym) (cadr arglist))
		 (t (get-sym sym (cddr arglist))))))
      (get-sym sym arglist))))

;;
(defmacro if-ret (form &rest else-body)
  "if-ret (form &rest else-body)
Evaluate form, and if the form is not nil, then return it,
else run else-body"
  (let ((ret (gensym)))
    `(let ((,ret ,form))
       (or ,ret
	   (progn
	     ,@else-body)))))

;;
(defun cut-cons-chain! (lst test)
  (check-type lst cons)
  (labels ((cut-cons-chain-tin (lst test parent-lst)
	     (cond
	       ((null lst) nil)
	       ((funcall test (cadr lst))
		(let ((keys (cdr lst)))
		  (setf (cdr lst) nil)
		  (values parent-lst keys)))
	       (t (cut-cons-chain! (cdr lst) test parent-lst)))))
    (cut-cons-chain-tin lst test lst)))

;;
(declaim (inline store-indexing))
(defun store-indexing (row col head row-stride col-stride)
  (declare (type (and fixnum (integer 0)) row col head row-stride col-stride))
  (the fixnum (+ head (the fixnum (* row row-stride)) (the fixnum (* col col-stride)))))

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
  (let* ((n (nrows matrix))
	 (m (ncols matrix))
	 (h (head matrix))
	 (rs (row-stride matrix))
	 (cs (col-stride matrix))
	 (ss (store-size matrix))
	 (nxm (* n m)))
    (declare (type fixnum n m h rs cs ss nxm))
    ;;Error checking is good if we use foreign-pointers as store types.
    (cond
      ((<= n 0) (error "Number of rows must be > 0. Initialized with ~A." n))
      ((<= m 0) (error "Number of columns must be > 0. Initialized with ~A." m))
      ;;
      ((< h 0) (error "Head of the store must be >= 0. Initialized with ~A." h))
      ((< rs 0) (error "Row-stride of the store must be >= 0. Initialized with ~A." rs))
      ((< cs 0) (error "Column-stride of the store must be >= 0. Initialized with ~A." cs))
      ;;
      ((>= (store-indexing (- n 1) (- m 1) h rs cs) ss) (error "Store is not large enough to accomodate the matrix.")))
    (setf (number-of-elements matrix) nxm)))

;;
(defgeneric matrix-ref-1d (matrix store-idx)
  (:documentation "
  Syntax
  ======
  (matrix-REF-1d store store-idx)

  Purpose
  =======
  Return the element store-idx of the matrix store."))

(defmethod matrix-ref-1d :before ((matrix standard-matrix) (store-idx fixnum))
  (when (> store-idx (store-size matrix))
    (error "Requested index ~A is out of bounds.
Matrix store is only of size: ~A." store-idx (store-size matrix))))

;;
(defgeneric (setf matrix-ref-1d) (value matrix store-idx))

(defmethod (setf matrix-ref-1d) :before ((value t) (matrix standard-matrix) (store-idx fixnum))
	    (when (> store-idx (store-size matrix))
	      (error "Requested index ~A is out of bounds.
Matrix store is only of size: ~A." store-idx (store-size matrix))))

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


(defmethod print-object ((matrix standard-matrix) stream)
  (dotimes (i (nrows matrix))
    (dotimes (j (ncols matrix))
      (format stream "~A    " (matrix-ref-2d matrix i j)))
    (format stream "~%")))
      