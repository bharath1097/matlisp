;;; Compiled by f2cl version 2.0 beta on 2001/02/23 at 10:09:25
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type 'simple-array)
;;;           (:array-slicing t))

(in-package "QUADPACK")
(use-package :f2cl)

(let ((x (make-array 11 :element-type 'double-float)))
  (declare (type (array double-float (11)) x))
  (fset (fref x (1) ((1 11))) 0.9914448613738104d0)
  (fset (fref x (2) ((1 11))) 0.9659258262890683d0)
  (fset (fref x (3) ((1 11))) 0.9238795325112867d0)
  (fset (fref x (4) ((1 11))) 0.8660254037844386d0)
  (fset (fref x (5) ((1 11))) 0.7933533402912352d0)
  (fset (fref x (6) ((1 11))) 0.7071067811865476d0)
  (fset (fref x (7) ((1 11))) 0.6087614290087207d0)
  (fset (fref x (8) ((1 11))) 0.5d0)
  (fset (fref x (9) ((1 11))) 0.3826834323650898d0)
  (fset (fref x (10) ((1 11))) 0.25881904510252074d0)
  (fset (fref x (11) ((1 11))) 0.1305261922200516d0)
  (defun dqc25c (f a b c result abserr krul neval)
    (declare (type double-float a b c result abserr)
             (type integer4 krul neval)
             (type (function (double-float) (values double-float &rest t)) f))
    (prog ((i 0) (isym 0) (k 0) (kp 0) (u 0.0d0) (res24 0.0d0) (res12 0.0d0)
           (resasc 0.0d0) (resabs 0.0d0) (p4 0.0d0) (p3 0.0d0) (p2 0.0d0)
           (hlgth 0.0d0) (fval (make-array 25 :element-type 'double-float))
           (cheb24 (make-array 25 :element-type 'double-float))
           (cheb12 (make-array 13 :element-type 'double-float)) (centr 0.0d0)
           (cc 0.0d0) (amom2 0.0d0) (amom1 0.0d0) (amom0 0.0d0) (ak22 0.0d0))
      (declare (type (array double-float (13)) cheb12)
               (type (array double-float (25)) fval cheb24)
               (type double-float ak22 amom0 amom1 amom2 cc centr hlgth p2 p3
                p4 resabs resasc res12 res24 u)
               (type integer4 kp k isym i))
      (declare (ftype (function (double-float) (values double-float)) dabs))
      (declare
       (ftype
        (function
         (double-float double-float double-float double-float double-float
          double-float integer4 double-float double-float double-float
          double-float double-float double-float)
         (values &rest t))
        dqk15w))
      (declare
       (ftype
        (function
         (array-double-float array-double-float array-double-float
          array-double-float)
         (values &rest t))
        dqcheb))
      (declare (ftype (function (double-float) (values double-float)) dlog))
      (setf cc (/ (- (* 2.0d0 c) b a) (- b a)))
      (if (< (dabs cc) 1.1d0) (go label10))
      (setf krul (- krul 1))
      (multiple-value-bind
          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10
           var-11 var-12)
          (dqk15w f #'dqwgtc c p2 p3 p4 kp a b result abserr resabs resasc)
        (declare (ignore var-0 var-1))
        (when var-2 (setf c var-2))
        (when var-3 (setf p2 var-3))
        (when var-4 (setf p3 var-4))
        (when var-5 (setf p4 var-5))
        (when var-6 (setf kp var-6))
        (when var-7 (setf a var-7))
        (when var-8 (setf b var-8))
        (when var-9 (setf result var-9))
        (when var-10 (setf abserr var-10))
        (when var-11 (setf resabs var-11))
        (when var-12 (setf resasc var-12)))
      (setf neval 15)
      (if (= resasc abserr) (setf krul (+ krul 1)))
      (go label50)
     label10
      (setf hlgth (* 0.5d0 (- b a)))
      (setf centr (* 0.5d0 (+ b a)))
      (setf neval 25)
      (fset (fref fval (1) ((1 25))) (* 0.5d0 (funcall f (+ hlgth centr))))
      (fset (fref fval (13) ((1 25)))
            (coerce
             (multiple-value-bind
                 (ret-val var-0)
                 (funcall f centr)
               (declare (ignore))
               (when var-0 (setf centr var-0))
               ret-val)
             'double-float))
      (fset (fref fval (25) ((1 25))) (* 0.5d0 (funcall f (- centr hlgth))))
      (fdo (i 2 (+ i 1))
           ((> i 12) nil)
           (tagbody
             (setf u (* hlgth (fref x ((- i 1)) ((1 11)))))
             (setf isym (- 26 i))
             (fset (fref fval (i) ((1 25))) (funcall f (+ u centr)))
             (fset (fref fval (isym) ((1 25))) (funcall f (- centr u)))
            label20))
      (dqcheb x fval cheb12 cheb24)
      (setf amom0 (dlog (dabs (/ (- 1.0d0 cc) (+ 1.0d0 cc)))))
      (setf amom1 (+ 2.0d0 (* cc amom0)))
      (setf res12
              (+ (* (fref cheb12 (1) ((1 13))) amom0)
                 (* (fref cheb12 (2) ((1 13))) amom1)))
      (setf res24
              (+ (* (fref cheb24 (1) ((1 25))) amom0)
                 (* (fref cheb24 (2) ((1 25))) amom1)))
      (fdo (k 3 (+ k 1))
           ((> k 13) nil)
           (tagbody
             (setf amom2 (- (* 2.0d0 cc amom1) amom0))
             (setf ak22 (coerce (* (- k 2) (- k 2)) 'double-float))
             (if (= (* (truncate k 2) 2) k)
                 (setf amom2 (+ amom2 (/ -4.0d0 (- ak22 1.0d0)))))
             (setf res12 (+ res12 (* (fref cheb12 (k) ((1 13))) amom2)))
             (setf res24 (+ res24 (* (fref cheb24 (k) ((1 25))) amom2)))
             (setf amom0 amom1)
             (setf amom1 amom2)
            label30))
      (fdo (k 14 (+ k 1))
           ((> k 25) nil)
           (tagbody
             (setf amom2 (- (* 2.0d0 cc amom1) amom0))
             (setf ak22 (coerce (* (- k 2) (- k 2)) 'double-float))
             (if (= (* (truncate k 2) 2) k)
                 (setf amom2 (+ amom2 (/ -4.0d0 (- ak22 1.0d0)))))
             (setf res24 (+ res24 (* (fref cheb24 (k) ((1 25))) amom2)))
             (setf amom0 amom1)
             (setf amom1 amom2)
            label40))
      (setf result res24)
      (setf abserr (dabs (- res24 res12)))
     label50
      (go end_label)
     end_label
      (return (values f a b c result abserr krul neval)))))

