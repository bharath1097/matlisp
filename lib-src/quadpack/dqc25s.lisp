;;; Compiled by f2cl version 2.0 beta on 2001/02/23 at 10:09:10
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type 'simple-array)
;;;           (:array-slicing t))


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
  (defun dqc25s
         (f a b bl br alfa beta ri rj rg rh result abserr resasc integr nev)
    (declare (type (array double-float (*)) ri rj rg rh)
             (type double-float a b bl br alfa beta result abserr resasc)
             (type integer4 integr nev)
             (type (function (double-float) (values double-float &rest t)) f))
    (prog ((i 0) (isym 0) (u 0.0d0) (res24 0.0d0) (res12 0.0d0) (resabs 0.0d0)
           (hlgth 0.0d0) (fval (make-array 25 :element-type 'double-float))
           (fix 0.0d0) (factor 0.0d0) (dc 0.0d0)
           (cheb24 (make-array 25 :element-type 'double-float))
           (cheb12 (make-array 13 :element-type 'double-float)) (centr 0.0d0))
      (declare (type (array double-float (13)) cheb12)
               (type (array double-float (25)) cheb24 fval)
               (type double-float centr dc factor fix hlgth resabs res12 res24
                u)
               (type integer4 isym i))
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
      (declare (ftype (function (double-float) (values double-float)) dabs))
      (setf nev 25)
      (if (and (= bl a) (or (/= alfa 0.0d0) (= integr 2) (= integr 4)))
          (go label10))
      (if (and (= br b) (or (/= beta 0.0d0) (= integr 3) (= integr 4)))
          (go label140))
      (multiple-value-bind
          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10
           var-11 var-12)
          (dqk15w f #'dqwgts a b alfa beta integr bl br result abserr resabs
           resasc)
        (declare (ignore var-0 var-1))
        (when var-2 (setf a var-2))
        (when var-3 (setf b var-3))
        (when var-4 (setf alfa var-4))
        (when var-5 (setf beta var-5))
        (when var-6 (setf integr var-6))
        (when var-7 (setf bl var-7))
        (when var-8 (setf br var-8))
        (when var-9 (setf result var-9))
        (when var-10 (setf abserr var-10))
        (when var-11 (setf resabs var-11))
        (when var-12 (setf resasc var-12)))
      (setf nev 15)
      (go label270)
     label10
      (setf hlgth (* 0.5d0 (- br bl)))
      (setf centr (* 0.5d0 (+ br bl)))
      (setf fix (- b centr))
      (fset (fref fval (1) ((1 25)))
            (* 0.5d0 (funcall f (+ hlgth centr)) (expt (- fix hlgth) beta)))
      (fset (fref fval (13) ((1 25)))
            (*
             (multiple-value-bind
                 (ret-val var-0)
                 (funcall f centr)
               (declare (ignore))
               (when var-0 (setf centr var-0))
               ret-val)
             (expt fix beta)))
      (fset (fref fval (25) ((1 25)))
            (* 0.5d0 (funcall f (- centr hlgth)) (expt (+ fix hlgth) beta)))
      (fdo (i 2 (+ i 1))
           ((> i 12) nil)
           (tagbody
             (setf u (* hlgth (fref x ((- i 1)) ((1 11)))))
             (setf isym (- 26 i))
             (fset (fref fval (i) ((1 25)))
                   (* (funcall f (+ u centr)) (expt (- fix u) beta)))
             (fset (fref fval (isym) ((1 25)))
                   (* (funcall f (- centr u)) (expt (+ fix u) beta)))
            label20))
      (setf factor (expt hlgth (+ alfa 1.0d0)))
      (setf result 0.0d0)
      (setf abserr 0.0d0)
      (setf res12 0.0d0)
      (setf res24 0.0d0)
      (if (> integr 2) (go label70))
      (dqcheb x fval cheb12 cheb24)
      (fdo (i 1 (+ i 1))
           ((> i 13) nil)
           (tagbody
             (setf res12
                     (+ res12
                        (* (fref cheb12 (i) ((1 13))) (fref ri (i) ((1 25))))))
             (setf res24
                     (+ res24
                        (* (fref cheb24 (i) ((1 25))) (fref ri (i) ((1 25))))))
            label30))
      (fdo (i 14 (+ i 1))
           ((> i 25) nil)
           (tagbody
             (setf res24
                     (+ res24
                        (* (fref cheb24 (i) ((1 25))) (fref ri (i) ((1 25))))))
            label40))
      (if (= integr 1) (go label130))
      (setf dc (dlog (- br bl)))
      (setf result (* res24 dc))
      (setf abserr (dabs (* (- res24 res12) dc)))
      (setf res12 0.0d0)
      (setf res24 0.0d0)
      (fdo (i 1 (+ i 1))
           ((> i 13) nil)
           (tagbody
             (setf res12
                     (+ res12
                        (* (fref cheb12 (i) ((1 13))) (fref rg (i) ((1 25))))))
             (setf res24
                     (+ res12
                        (* (fref cheb24 (i) ((1 25))) (fref rg (i) ((1 25))))))
            label50))
      (fdo (i 14 (+ i 1))
           ((> i 25) nil)
           (tagbody
             (setf res24
                     (+ res24
                        (* (fref cheb24 (i) ((1 25))) (fref rg (i) ((1 25))))))
            label60))
      (go label130)
     label70
      (fset (fref fval (1) ((1 25)))
            (* (fref fval (1) ((1 25))) (dlog (- fix hlgth))))
      (fset (fref fval (13) ((1 25))) (* (fref fval (13) ((1 25))) (dlog fix)))
      (fset (fref fval (25) ((1 25)))
            (* (fref fval (25) ((1 25))) (dlog (+ fix hlgth))))
      (fdo (i 2 (+ i 1))
           ((> i 12) nil)
           (tagbody
             (setf u (* hlgth (fref x ((- i 1)) ((1 11)))))
             (setf isym (- 26 i))
             (fset (fref fval (i) ((1 25)))
                   (* (fref fval (i) ((1 25))) (dlog (- fix u))))
             (fset (fref fval (isym) ((1 25)))
                   (* (fref fval (isym) ((1 25))) (dlog (+ fix u))))
            label80))
      (dqcheb x fval cheb12 cheb24)
      (fdo (i 1 (+ i 1))
           ((> i 13) nil)
           (tagbody
             (setf res12
                     (+ res12
                        (* (fref cheb12 (i) ((1 13))) (fref ri (i) ((1 25))))))
             (setf res24
                     (+ res24
                        (* (fref cheb24 (i) ((1 25))) (fref ri (i) ((1 25))))))
            label90))
      (fdo (i 14 (+ i 1))
           ((> i 25) nil)
           (tagbody
             (setf res24
                     (+ res24
                        (* (fref cheb24 (i) ((1 25))) (fref ri (i) ((1 25))))))
            label100))
      (if (= integr 3) (go label130))
      (setf dc (dlog (- br bl)))
      (setf result (* res24 dc))
      (setf abserr (dabs (* (- res24 res12) dc)))
      (setf res12 0.0d0)
      (setf res24 0.0d0)
      (fdo (i 1 (+ i 1))
           ((> i 13) nil)
           (tagbody
             (setf res12
                     (+ res12
                        (* (fref cheb12 (i) ((1 13))) (fref rg (i) ((1 25))))))
             (setf res24
                     (+ res24
                        (* (fref cheb24 (i) ((1 25))) (fref rg (i) ((1 25))))))
            label110))
      (fdo (i 14 (+ i 1))
           ((> i 25) nil)
           (tagbody
             (setf res24
                     (+ res24
                        (* (fref cheb24 (i) ((1 25))) (fref rg (i) ((1 25))))))
            label120))
     label130
      (setf result (* (+ result res24) factor))
      (setf abserr (* (+ abserr (dabs (- res24 res12))) factor))
      (go label270)
     label140
      (setf hlgth (* 0.5d0 (- br bl)))
      (setf centr (* 0.5d0 (+ br bl)))
      (setf fix (- centr a))
      (fset (fref fval (1) ((1 25)))
            (* 0.5d0 (funcall f (+ hlgth centr)) (expt (+ fix hlgth) alfa)))
      (fset (fref fval (13) ((1 25)))
            (*
             (multiple-value-bind
                 (ret-val var-0)
                 (funcall f centr)
               (declare (ignore))
               (when var-0 (setf centr var-0))
               ret-val)
             (expt fix alfa)))
      (fset (fref fval (25) ((1 25)))
            (* 0.5d0 (funcall f (- centr hlgth)) (expt (- fix hlgth) alfa)))
      (fdo (i 2 (+ i 1))
           ((> i 12) nil)
           (tagbody
             (setf u (* hlgth (fref x ((- i 1)) ((1 11)))))
             (setf isym (- 26 i))
             (fset (fref fval (i) ((1 25)))
                   (* (funcall f (+ u centr)) (expt (+ fix u) alfa)))
             (fset (fref fval (isym) ((1 25)))
                   (* (funcall f (- centr u)) (expt (- fix u) alfa)))
            label150))
      (setf factor (expt hlgth (+ beta 1.0d0)))
      (setf result 0.0d0)
      (setf abserr 0.0d0)
      (setf res12 0.0d0)
      (setf res24 0.0d0)
      (if (or (= integr 2) (= integr 4)) (go label200))
      (dqcheb x fval cheb12 cheb24)
      (fdo (i 1 (+ i 1))
           ((> i 13) nil)
           (tagbody
             (setf res12
                     (+ res12
                        (* (fref cheb12 (i) ((1 13))) (fref rj (i) ((1 25))))))
             (setf res24
                     (+ res24
                        (* (fref cheb24 (i) ((1 25))) (fref rj (i) ((1 25))))))
            label160))
      (fdo (i 14 (+ i 1))
           ((> i 25) nil)
           (tagbody
             (setf res24
                     (+ res24
                        (* (fref cheb24 (i) ((1 25))) (fref rj (i) ((1 25))))))
            label170))
      (if (= integr 1) (go label260))
      (setf dc (dlog (- br bl)))
      (setf result (* res24 dc))
      (setf abserr (dabs (* (- res24 res12) dc)))
      (setf res12 0.0d0)
      (setf res24 0.0d0)
      (fdo (i 1 (+ i 1))
           ((> i 13) nil)
           (tagbody
             (setf res12
                     (+ res12
                        (* (fref cheb12 (i) ((1 13))) (fref rh (i) ((1 25))))))
             (setf res24
                     (+ res24
                        (* (fref cheb24 (i) ((1 25))) (fref rh (i) ((1 25))))))
            label180))
      (fdo (i 14 (+ i 1))
           ((> i 25) nil)
           (tagbody
             (setf res24
                     (+ res24
                        (* (fref cheb24 (i) ((1 25))) (fref rh (i) ((1 25))))))
            label190))
      (go label260)
     label200
      (fset (fref fval (1) ((1 25)))
            (* (fref fval (1) ((1 25))) (dlog (+ hlgth fix))))
      (fset (fref fval (13) ((1 25))) (* (fref fval (13) ((1 25))) (dlog fix)))
      (fset (fref fval (25) ((1 25)))
            (* (fref fval (25) ((1 25))) (dlog (- fix hlgth))))
      (fdo (i 2 (+ i 1))
           ((> i 12) nil)
           (tagbody
             (setf u (* hlgth (fref x ((- i 1)) ((1 11)))))
             (setf isym (- 26 i))
             (fset (fref fval (i) ((1 25)))
                   (* (fref fval (i) ((1 25))) (dlog (+ u fix))))
             (fset (fref fval (isym) ((1 25)))
                   (* (fref fval (isym) ((1 25))) (dlog (- fix u))))
            label210))
      (dqcheb x fval cheb12 cheb24)
      (fdo (i 1 (+ i 1))
           ((> i 13) nil)
           (tagbody
             (setf res12
                     (+ res12
                        (* (fref cheb12 (i) ((1 13))) (fref rj (i) ((1 25))))))
             (setf res24
                     (+ res24
                        (* (fref cheb24 (i) ((1 25))) (fref rj (i) ((1 25))))))
            label220))
      (fdo (i 14 (+ i 1))
           ((> i 25) nil)
           (tagbody
             (setf res24
                     (+ res24
                        (* (fref cheb24 (i) ((1 25))) (fref rj (i) ((1 25))))))
            label230))
      (if (= integr 2) (go label260))
      (setf dc (dlog (- br bl)))
      (setf result (* res24 dc))
      (setf abserr (dabs (* (- res24 res12) dc)))
      (setf res12 0.0d0)
      (setf res24 0.0d0)
      (fdo (i 1 (+ i 1))
           ((> i 13) nil)
           (tagbody
             (setf res12
                     (+ res12
                        (* (fref cheb12 (i) ((1 13))) (fref rh (i) ((1 25))))))
             (setf res24
                     (+ res24
                        (* (fref cheb24 (i) ((1 25))) (fref rh (i) ((1 25))))))
            label240))
      (fdo (i 14 (+ i 1))
           ((> i 25) nil)
           (tagbody
             (setf res24
                     (+ res24
                        (* (fref cheb24 (i) ((1 25))) (fref rh (i) ((1 25))))))
            label250))
     label260
      (setf result (* (+ result res24) factor))
      (setf abserr (* (+ abserr (dabs (- res24 res12))) factor))
     label270
      (go end_label)
     end_label
      (return
       (values f
               a
               b
               bl
               br
               alfa
               beta
               ri
               rj
               rg
               rh
               result
               abserr
               resasc
               integr
               nev)))))

