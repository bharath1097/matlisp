(in-package #:matlisp-utilities)

(declaim (inline string+))
(defun string+ (&rest strings)
  (apply #'concatenate (cons 'string strings)))

(defun format-to-string (fmt &rest args)
  (apply #'format (append (list nil fmt) args)))
