;;; Compiled by f2cl version 2.0 beta on 2001/02/23 at 10:08:29
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type 'simple-array)
;;;           (:array-slicing t))

(in-package "QUADPACK")
(use-package :f2cl)

(defun dqwgtf (x omega p2 p3 p4 integr)
  (declare (type integer4 integr) (type double-float p4 p3 p2 omega x))
  (prog ((omx 0.0d0) (dqwgtf 0.0d0))
    (declare (type double-float dqwgtf omx))
    (declare (ftype (function (double-float) (values double-float)) dcos))
    (declare (ftype (function (double-float) (values double-float)) dsin))
    (setf omx (* omega x))
    (computed-goto (label10 label20) integr)
   label10
    (setf dqwgtf (dcos omx))
    (go label30)
   label20
    (setf dqwgtf (dsin omx))
   label30
    (go end_label)
   end_label
    (return (values dqwgtf x omega p2 p3 p4 integr))))

