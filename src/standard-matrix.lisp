(in-package :matlisp)

;;
(defclass standard-matrix (standard-tensor)  
  ((rank
    :accessor rank
    :type index-type
    :initform 2
    :documentation "For a matrix, rank = 2."))
  (:documentation "Basic matrix class."))

(defun nrows (matrix)
  (declare (type standard-matrix matrix))
  (let ((dims (dimensions matrix)))
    (declare (type (index-array 2) dims))
    (aref dims 0)))

(defun ncols (matrix)
  (declare (type standard-matrix matrix))
  (let ((dims (dimensions matrix)))
    (declare (type (index-array 2) dims))
    (aref dims 1)))

(defun row-stride (matrix)
  (declare (type standard-matrix matrix))
  (let ((stds (strides matrix)))
    (declare (type (index-array 2) stds))
    (aref stds 0)))

(defun col-stride (matrix)
  (declare (type standard-matrix matrix))
  (let ((stds (strides matrix)))
    (declare (type (index-array 2) stds))    
    (aref stds 1)))

(defun size (matrix)
  (declare (type standard-matrix matrix))
  (let ((dims (dimensions matrix)))
    (declare (type (index-array 2) dims))
    (list (aref dims 0) (aref dims 1))))
;;

(defmethod initialize-instance :after ((matrix standard-matrix) &rest initargs)
  (declare (ignore initargs))
  (mlet*
   ((rank (rank matrix) :type index-type))
   (unless (= rank 2)
     (error 'tensor-not-matrix :rank rank :tensor matrix))))

;;
(defmacro matrix-ref (matrix row &optional col)
  (if col
      `(matrix-ref-2d ,matrix ,row ,col)
      `(matrix-ref-1d ,matrix ,row)))

;;
(defun row-vector-p (matrix)
  "
  Syntax
  ======
  (ROW-VECTOR-P x)

  Purpose
  =======
  Return T if X is a row vector (number of columns is 1)"
  (tensor-type-p '(1 t)))

(defun col-vector-p (matrix)
  "
  Syntax
  ======
  (COL-VECTOR-P x)
 
  Purpose
  =======
  Return T if X is a column vector (number of rows is 1)"
  (tensor-type-p '(t 1)))

(defun row-or-col-vector-p (matrix)
"
  Syntax
  ======
  (ROW-OR-COL-VECTOR-P x)

  Purpose
  =======
  Return T if X is either a row or a column vector"
  (or (row-vector-p matrix) (col-vector-p matrix)))

(defun square-matrix-p (matrix)
  (and (square-p matrix) (matrix-p matrix)))

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