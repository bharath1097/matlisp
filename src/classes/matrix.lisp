(in-package #:matlisp)

(definline nrows (matrix)
  (aref (the index-store-vector (dimensions matrix)) 0))

(definline ncols (matrix)
  (aref (the index-store-vector (dimensions matrix)) 1))

(definline row-stride (matrix)
  (aref (the index-store-vector (strides matrix)) 0))

(definline col-stride (matrix)
  (aref (the index-store-vector (strides matrix)) 1))

;;
#+nil
(definline row-matrix-p (matrix)
  "
  Syntax
  ======
  (ROW-MATRIX-P x)

  Purpose
  =======
  Return T if X is a row matrix (number of columns is 1)"
  (tensor-typep matrix '(1 *)))

#+nil
(definline col-matrix-p (matrix)
  "
  Syntax
  ======
  (COL-MATRIX-P x)
 
  Purpose
  =======
  Return T if X is a column matrix (number of rows is 1)"
  (tensor-typep matrix '(* 1)))

#+nil
(definline row-or-col-matrix-p (matrix)
"
  Syntax
  ======
  (ROW-OR-COL-matrix-P x)

  Purpose
  =======
  Return T if X is either a row or a column matrix."
  (or (row-matrix-p matrix) (col-matrix-p matrix)))

(definline tensor-square-matrixp (matrix)
  (and (tensor-matrixp matrix) (tensor-squarep matrix)))

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

