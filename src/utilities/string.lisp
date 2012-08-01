(in-package #:matlisp-utilities)

(declaim (inline string+))
(defun string+ (&rest strings)
  (apply #'concatenate (cons 'string strings)))

(defun format-to-string (fmt &rest args)
  (let ((ret (make-array 0 :element-type 'character :adjustable t :fill-pointer t)))
    (with-output-to-string (ostr ret)
      (apply #'format (append `(,ostr ,fmt) args)))
    ret))
