(in-package #:matlisp)

(defun ones (dims &optional (type *default-tensor-type*))
  (copy! 1 (zeros dims type)))
;;
(defun eye (dims &optional (type *default-tensor-type*))
  ;;Not optimized, takes nearly as much time as ones.
  (let* ((ret (zeros dims type))
	 (ref (make-list (order ret) :initial-element 0)))
    (loop :for i :from 0 :below (lvec-min (dimensions ret))
       :do (setf (ref ret (copy! i ref)) 1))
    ret))
