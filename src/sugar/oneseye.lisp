(in-package #:matlisp)

(defun ones (dims &optional (type *default-tensor-type*))
  (zeros dims type 1))

(defun eye! (tensor)
  (tricopy! 1 tensor :d))

(defun eye (dims &optional (type *default-tensor-type*))
  (tricopy! 1 (zeros dims type) :d))

(defun diag (tens &optional (order 2))
  (let ((ret (zeros (make-list order :initial-element (dimensions tens 0)) (type-of tens))))
    (tricopy! tens ret :d)))

(defun diag~ (a)
  (declare (type standard-tensor a))
  (let ((str (lvec-foldr #'(lambda (x y) (declare (type index-type x y)) (the index-type (+ x y))) (strides a)))
	(dim (lvec-min (dimensions a))))
    (with-no-init-checks (make-instance (class-of a)
					:parent-tensor a
					:store (store a)
					:dimensions (make-index-store (list dim))
					:head (head a)
					:strides (make-index-store (list str))))))
