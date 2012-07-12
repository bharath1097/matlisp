(in-package #:matlisp)

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
  (or (row-matrix-p matrix) (col-matrix-p matrix)))

(definline square-matrix-p (matrix)
  (and (square-p matrix) (matrix-p matrix)))

;; ;;
;; (defgeneric fill-matrix (matrix fill-element)
;;   (:documentation 
;;    "
;;    Syntax
;;    ======
;;    (FILL-MATRIX matrix fill-element)
  
;;    Purpose
;;    =======
;;    Fill MATRIX with FILL-ELEMENT.
;; "))

;; (defmethod fill-matrix ((matrix t) (fill t))
;;   (error "arguments MATRIX and FILL to FILL-MATRIX must be a
;; matrix and a number"))

;; ;;
;; ;;

;; ;;

;; (definline matrix-ref (matrix row &optional col)
;;   (declare (type standard-matrix matrix))
;;   (tensor-ref matrix `(,row ,col)))

