;;; Compiled by f2cl version 2.0 beta on 2001/02/23 at 10:08:38
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type 'simple-array)
;;;           (:array-slicing t))

(in-package "QUADPACK")
(use-package :f2cl)

(defun dqwgts (x a b alfa beta integr)
  (declare (type integer4 integr) (type double-float beta alfa b a x))
  (prog ((bmx 0.0d0) (xma 0.0d0) (dqwgts 0.0d0))
    (declare (type double-float dqwgts xma bmx))
    (declare (ftype (function (double-float) (values double-float)) dlog))
    (setf xma (- x a))
    (setf bmx (- b x))
    (setf dqwgts (* (expt xma alfa) (expt bmx beta)))
    (computed-goto (label40 label10 label20 label30) integr)
   label10
    (setf dqwgts (* dqwgts (dlog xma)))
    (go label40)
   label20
    (setf dqwgts (* dqwgts (dlog bmx)))
    (go label40)
   label30
    (setf dqwgts (* dqwgts (dlog xma) (dlog bmx)))
   label40
    (go end_label)
   end_label
    (return (values dqwgts x a b alfa beta integr))))

