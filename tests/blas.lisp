(in-package #:matlisp-user)

(asdf:oos 'asdf:load-op :rt)

(defmethod max-matrix-diff ((actual standard-matrix) (expected standard-matrix) &key (allowed-error 0d0))
  (let ((max-error (reduce #'max
			   (map 'list
				#'(lambda (x y)
				    (abs (- x y)))
				(matlisp::store actual)
				(matlisp::store expected)))))
    (if (<= max-error allowed-error)
	t
	(list max-error actual expected))))

(rt:deftest blas.zdotu.1.1
    (let ((x (matlisp::store [#c(1d0 0) #c(2d0 0) #c(3d0 0)])))
      (blas:zdotu 2 x 1 x 1))
  #c(5d0 0d0))

(rt:deftest blas.zdotu.1.2
    (let ((x (matlisp::store [#c(1d0 1d0) #c(2d0 2d0) #c(3d0 3d0)])))
      (blas:zdotu 2 x 1 x 1))
  #c(0d0 10d0))

(rt:deftest blas.zdotc.1.1
    (let ((x (matlisp::store [#c(1d0 0) #c(2d0 0) #c(3d0 0)])))
      (blas:zdotc 2 x 1 x 1))
  #c(5d0 0d0))

(rt:deftest blas.zdotc.1.2
    (let ((x (matlisp::store [#c(1d0 1d0) #c(2d0 2d0) #c(3d0 3d0)])))
      (blas:zdotc 2 x 1 x 1))
  #c(10d0 0d0))

(rt:deftest blas.axpy.1
    (let ((x [1 2 3]))
      (max-matrix-diff (axpy 2 x x) [3 6 9]))
  t)
