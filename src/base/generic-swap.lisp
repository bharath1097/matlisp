(in-package #:matlisp)

(defgeneric swap! (x y)
  (:documentation
"
  Sytnax
  ======
  (SWAP! x y)

  Purpose
  =======
  Given tensors X,Y, performs:

              X <-> Y

  and returns Y.

  X, Y must have the same dimensions.
"))
