(in-package #:matlisp)

(defun ones (dims &optional (type *default-tensor-type*))
  (zeros dims type 1))

(defun eye (dims &optional (type *default-tensor-type*))
  (tricopy! 1 (zeros dims type) :d))

(defun diag (tens &optional (order 2))
  (let ((ret (zeros (make-list order :initial-element (aref (dimensions tens) 0)) (type-of tens))))
    (tricopy! tens ret :d)))
