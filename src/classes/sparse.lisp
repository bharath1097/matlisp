(in-package #:matlisp)

(defleaf real-coordinate-sparse-tensor (coordinate-sparse-tensor) ())
(deft/method t/field-type (sym real-coordinate-sparse-tensor) ()
  'double-float)

;;
(defleaf real-compressed-sparse-matrix (compressed-sparse-matrix) ())
(deft/method t/field-type (sym real-compressed-sparse-matrix) ()
  'double-float)
