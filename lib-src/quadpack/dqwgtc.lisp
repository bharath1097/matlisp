;;; Compiled by f2cl version 2.0 beta on 2001/02/23 at 10:08:38
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type 'simple-array)
;;;           (:array-slicing t))

(in-package "QUADPACK")
(use-package :f2cl)

(defun dqwgtc (x c p2 p3 p4 kp)
  (declare (type integer4 kp) (type double-float p4 p3 p2 c x))
  (prog ((dqwgtc 0.0d0))
    (declare (type double-float dqwgtc))
    (setf dqwgtc (/ 1.0d0 (- x c)))
    (go end_label)
   end_label
    (return (values dqwgtc x c p2 p3 p4 kp))))

