(in-package :matlisp)

;;
(defclass standard-matrix (standard-tensor)  
  ((rank
    :accessor rank
    :type index-type
    :initform 2
    :documentation "For a matrix, rank = 2."))
  (:documentation "Basic matrix class."))

(defmethod print-object ((tensor standard-matrix) stream)
  (print-unreadable-object (tensor stream :type t)
    (format stream "~A x ~A~%" (nrows tensor) (ncols tensor))
    (print-tensor tensor stream)))

(definline nrows (matrix)
  (declare (type standard-matrix matrix))
  (aref (dimensions matrix) 0))

(definline ncols (matrix)
  (declare (type standard-matrix matrix))
  (aref (dimensions matrix) 1))

(definline row-stride (matrix)
  (declare (type standard-matrix matrix))
  (aref (strides matrix) 0))

(definline col-stride (matrix)
  (declare (type standard-matrix matrix))
  (aref (strides matrix) 1))

(definline size (matrix)
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
(definline row-matrix-p (matrix)
  "
  Syntax
  ======
  (ROW-MATRIX-P x)

  Purpose
  =======
  Return T if X is a row matrix (number of columns is 1)"
  (tensor-type-p matrix '(1 *)))

(definline col-matrix-p (matrix)
  "
  Syntax
  ======
  (COL-MATRIX-P x)
 
  Purpose
  =======
  Return T if X is a column matrix (number of rows is 1)"
  (tensor-type-p matrix '(* 1)))

(definline row-or-col-matrix-p (matrix)
"
  Syntax
  ======
  (ROW-OR-COL-matrix-P x)

  Purpose
  =======
  Return T if X is either a row or a column matrix."
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

;;
(defclass real-matrix (standard-matrix real-tensor)
  ()
  (:documentation "A class of matrices with real elements."))

(defclass real-sub-matrix (real-matrix standard-sub-tensor)
  ()
  (:documentation "Sub-matrix class with real elements."))

(setf (gethash 'real-matrix *sub-tensor-counterclass*) 'real-sub-matrix
      (gethash 'real-sub-matrix *sub-tensor-counterclass*) 'real-sub-matrix
      ;;
      (gethash 'real-matrix *tensor-class-optimizations*) 'real-tensor
      (gethash 'real-sub-matrix *tensor-class-optimizations*) 'real-tensor)
;;

(defclass complex-matrix (standard-matrix complex-tensor)
  ()
  (:documentation "A class of matrices with complex elements."))

(defclass complex-sub-matrix (complex-matrix standard-sub-tensor)
  ()
  (:documentation "Sub-matrix class with complex elements."))

(setf (gethash 'complex-matrix *sub-tensor-counterclass*) 'complex-sub-matrix
      (gethash 'complex-sub-matrix *sub-tensor-counterclass*) 'complex-sub-matrix
      ;;
      (gethash 'complex-matrix *tensor-class-optimizations*) 'complex-tensor
      (gethash 'complex-sub-matrix *tensor-class-optimizations*) 'complex-tensor)

;;

(definline matrix-ref (matrix row &optional col)
  (declare (type standard-matrix matrix))
  (tensor-ref matrix `(,row ,col)))

