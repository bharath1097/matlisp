;;; Compiled by f2cl version 2.0 beta on 2001/02/23 at 10:08:39
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type 'simple-array)
;;;           (:array-slicing t))


(use-package :f2cl)

(defun dgtsl (n c d e b info)
  (declare (type (array double-float (*)) b e d c) (type integer4 info n))
  (prog ((k 0) (kb 0) (kp1 0) (nm1 0) (nm2 0) (t_ 0.0d0))
    (declare (type double-float t_) (type integer4 nm2 nm1 kp1 kb k))
    (declare
     (ftype (function (array-double-float) (values double-float)) dabs))
    (setf info 0)
    (fset (fref c (1) ((1 1))) (fref d (1) ((1 1))))
    (setf nm1 (- n 1))
    (if (< nm1 1) (go label40))
    (fset (fref d (1) ((1 1))) (fref e (1) ((1 1))))
    (fset (fref e (1) ((1 1))) 0.0d0)
    (fset (fref e (n) ((1 1))) 0.0d0)
    (fdo (k 1 (+ k 1))
         ((> k nm1) nil)
         (tagbody
           (setf kp1 (+ k 1))
           (if (< (dabs (fref c (kp1) ((1 1)))) (dabs (fref c (k) ((1 1)))))
               (go label10))
           (setf t_ (fref c (kp1) ((1 1))))
           (fset (fref c (kp1) ((1 1))) (fref c (k) ((1 1))))
           (fset (fref c (k) ((1 1))) t_)
           (setf t_ (fref d (kp1) ((1 1))))
           (fset (fref d (kp1) ((1 1))) (fref d (k) ((1 1))))
           (fset (fref d (k) ((1 1))) t_)
           (setf t_ (fref e (kp1) ((1 1))))
           (fset (fref e (kp1) ((1 1))) (fref e (k) ((1 1))))
           (fset (fref e (k) ((1 1))) t_)
           (setf t_ (fref b (kp1) ((1 1))))
           (fset (fref b (kp1) ((1 1))) (fref b (k) ((1 1))))
           (fset (fref b (k) ((1 1))) t_)
          label10
           (if (/= (fref c (k) ((1 1))) 0.0d0) (go label20))
           (setf info k)
           (go label100)
          label20
           (setf t_ (/ (- (fref c (kp1) ((1 1)))) (fref c (k) ((1 1)))))
           (fset (fref c (kp1) ((1 1)))
                 (+ (fref d (kp1) ((1 1))) (* t_ (fref d (k) ((1 1))))))
           (fset (fref d (kp1) ((1 1)))
                 (+ (fref e (kp1) ((1 1))) (* t_ (fref e (k) ((1 1))))))
           (fset (fref e (kp1) ((1 1))) 0.0d0)
           (fset (fref b (kp1) ((1 1)))
                 (+ (fref b (kp1) ((1 1))) (* t_ (fref b (k) ((1 1))))))
          label30))
   label40
    (if (/= (fref c (n) ((1 1))) 0.0d0) (go label50))
    (setf info n)
    (go label90)
   label50
    (setf nm2 (- n 2))
    (fset (fref b (n) ((1 1))) (/ (fref b (n) ((1 1))) (fref c (n) ((1 1)))))
    (if (= n 1) (go label80))
    (fset (fref b (nm1) ((1 1)))
          (/
           (- (fref b (nm1) ((1 1)))
              (* (fref d (nm1) ((1 1))) (fref b (n) ((1 1)))))
           (fref c (nm1) ((1 1)))))
    (if (< nm2 1) (go label70))
    (fdo (kb 1 (+ kb 1))
         ((> kb nm2) nil)
         (tagbody
           (setf k (+ (- nm2 kb) 1))
           (fset (fref b (k) ((1 1)))
                 (/
                  (- (fref b (k) ((1 1)))
                     (* (fref d (k) ((1 1))) (fref b ((+ k 1)) ((1 1))))
                     (* (fref e (k) ((1 1))) (fref b ((+ k 2)) ((1 1)))))
                  (fref c (k) ((1 1)))))
          label60))
   label70
   label80
   label90
   label100
    (go end_label)
   end_label
    (return (values n c d e b info))))

