;;; Compiled by f2cl version 2.0 beta on 2001/02/23 at 10:09:28
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
  (defun dqc25f
         (f a b omega integr nrmom maxp1 ksave result abserr neval resabs
          resasc momcom chebmo)
    (declare (type double-float a b omega result abserr resabs resasc)
             (type integer4 integr nrmom maxp1 ksave neval momcom)
             (type (array double-float (*)) chebmo)
             (type (function (double-float) (values double-float &rest t)) f))
    (prog ((i 0) (iers 0) (isym 0) (j 0) (k 0) (m 0) (noequ 0) (noeq1 0)
           (v (make-array 28 :element-type 'double-float)) (sinpar 0.0d0)
           (ress24 0.0d0) (ress12 0.0d0) (resc24 0.0d0) (resc12 0.0d0)
           (p4 0.0d0) (p3 0.0d0) (p2 0.0d0) (par22 0.0d0) (par2 0.0d0)
           (parint 0.0d0) (oflow 0.0d0) (hlgth 0.0d0)
           (fval (make-array 25 :element-type 'double-float)) (ests 0.0d0)
           (estc 0.0d0) (d2 (make-array 25 :element-type 'double-float))
           (d1 (make-array 25 :element-type 'double-float))
           (d (make-array 25 :element-type 'double-float)) (cospar 0.0d0)
           (cons 0.0d0) (conc 0.0d0)
           (cheb24 (make-array 25 :element-type 'double-float))
           (cheb12 (make-array 13 :element-type 'double-float)) (centr 0.0d0)
           (ass 0.0d0) (asap 0.0d0) (as 0.0d0) (an2 0.0d0) (an 0.0d0)
           (ac 0.0d0))
      (declare (type (array double-float (13)) cheb12)
               (type (array double-float (25)) cheb24 d d1 d2 fval)
               (type (array double-float (28)) v)
               (type double-float ac an an2 as asap ass centr conc cons cospar
                estc ests hlgth oflow parint par2 par22 p2 p3 p4 resc12 resc24
                ress12 ress24 sinpar)
               (type integer4 noeq1 noequ m k j isym iers i))
      (declare
       (ftype (function (integer4) (values double-float &rest t)) d1mach))
      (declare
       (ftype
        (function (or array-double-float double-float) (values double-float))
        dabs))
      (declare
       (ftype
        (function
         (double-float double-float double-float double-float double-float
          double-float integer4 double-float double-float double-float
          double-float double-float double-float)
         (values &rest t))
        dqk15w))
      (declare (ftype (function (double-float) (values double-float)) dcos))
      (declare (ftype (function (double-float) (values double-float)) dsin))
      (declare
       (ftype
        (function
         (integer4 array-double-float array-double-float array-double-float
          array-double-float integer4)
         (values &rest t))
        dgtsl))
      (declare
       (ftype
        (function
         (array-double-float array-double-float array-double-float
          array-double-float)
         (values &rest t))
        dqcheb))
      (setf oflow (d1mach 2))
      (setf centr (* 0.5d0 (+ b a)))
      (setf hlgth (* 0.5d0 (- b a)))
      (setf parint (* omega hlgth))
      (if (> (dabs parint) 2.0d0) (go label10))
      (multiple-value-bind
          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10
           var-11 var-12)
          (dqk15w f #'dqwgtf omega p2 p3 p4 integr a b result abserr resabs
           resasc)
        (declare (ignore var-0 var-1))
        (when var-2 (setf omega var-2))
        (when var-3 (setf p2 var-3))
        (when var-4 (setf p3 var-4))
        (when var-5 (setf p4 var-5))
        (when var-6 (setf integr var-6))
        (when var-7 (setf a var-7))
        (when var-8 (setf b var-8))
        (when var-9 (setf result var-9))
        (when var-10 (setf abserr var-10))
        (when var-11 (setf resabs var-11))
        (when var-12 (setf resasc var-12)))
      (setf neval 15)
      (go label170)
     label10
      (setf conc (* hlgth (dcos (* centr omega))))
      (setf cons (* hlgth (dsin (* centr omega))))
      (setf resasc oflow)
      (setf neval 25)
      (if (or (< nrmom momcom) (= ksave 1)) (go label120))
      (setf m (+ momcom 1))
      (setf par2 (* parint parint))
      (setf par22 (+ par2 2.0d0))
      (setf sinpar (dsin parint))
      (setf cospar (dcos parint))
      (fset (fref v (1) ((1 28))) (/ (* 2.0d0 sinpar) parint))
      (fset (fref v (2) ((1 28)))
            (/
             (+ (* 8.0d0 cospar) (/ (* (- (+ par2 par2) 8.0d0) sinpar) parint))
             par2))
      (fset (fref v (3) ((1 28)))
            (/
             (+ (* 32.0d0 (- par2 12.0d0) cospar)
                (/ (* 2.0d0 (+ (* (- par2 80.0d0) par2) 192.0d0) sinpar)
                   parint))
             (* par2 par2)))
      (setf ac (* 8.0d0 cospar))
      (setf as (* 24.0d0 parint sinpar))
      (if (> (dabs parint) 24.0d0) (go label30))
      (setf noequ 25)
      (setf noeq1 (- noequ 1))
      (setf an 6.0d0)
      (fdo (k 1 (+ k 1))
           ((> k noeq1) nil)
           (tagbody
             (setf an2 (* an an))
             (fset (fref d (k) ((1 25)))
                   (* -2.0d0 (- an2 4.0d0) (- par22 an2 an2)))
             (fset (fref d2 (k) ((1 25))) (* (- an 1.0d0) (- an 2.0d0) par2))
             (fset (fref d1 ((+ k 1)) ((1 25)))
                   (* (+ an 3.0d0) (+ an 4.0d0) par2))
             (fset (fref v ((+ k 3)) ((1 28))) (- as (* (- an2 4.0d0) ac)))
             (setf an (+ an 2.0d0))
            label20))
      (setf an2 (* an an))
      (fset (fref d (noequ) ((1 25)))
            (* -2.0d0 (- an2 4.0d0) (- par22 an2 an2)))
      (fset (fref v ((+ noequ 3)) ((1 28))) (- as (* (- an2 4.0d0) ac)))
      (fset (fref v (4) ((1 28)))
            (+ (fref v (4) ((1 28)))
               (* -56.00000000000001d0 par2 (fref v (3) ((1 28))))))
      (setf ass (* parint sinpar))
      (setf asap
              (/
               (-
                (/
                 (+
                  (-
                   (/
                    (+
                     (-
                      (/
                       (- (* (- (* 210.0d0 par2) 1.0d0) cospar)
                          (* (- (* 105.0d0 par2) 63.0d0) ass))
                       an2)
                      (* (+ 1.0d0 (* -1 15.0d0 par2)) cospar))
                     (* 15.0d0 ass))
                    an2)
                   cospar)
                  (* 3.0d0 ass))
                 an2)
                cospar)
               an2))
      (fset (fref v ((+ noequ 3)) ((1 28)))
            (+ (fref v ((+ noequ 3)) ((1 28)))
               (* -2.0d0 asap par2 (- an 1.0d0) (- an 2.0d0))))
      (multiple-value-bind
          (var-0 var-1 var-2 var-3 var-4 var-5)
          (dgtsl noequ d1 d d2 (array-slice v double-float (4) ((1 28))) iers)
        (declare (ignore var-1 var-2 var-3 var-4))
        (when var-0 (setf noequ var-0))
        (when var-5 (setf iers var-5)))
      (go label50)
     label30
      (setf an 4.0d0)
      (fdo (i 4 (+ i 1))
           ((> i 13) nil)
           (tagbody
             (setf an2 (* an an))
             (fset (fref v (i) ((1 28)))
                   (/
                    (+
                     (* (- an2 4.0d0)
                        (-
                         (* 2.0d0
                            (- par22 an2 an2)
                            (fref v ((- i 1)) ((1 28))))
                         ac))
                     as
                     (* (- par2)
                        (+ an 1.0d0)
                        (+ an 2.0d0)
                        (fref v ((- i 2)) ((1 28)))))
                    (* par2 (- an 1.0d0) (- an 2.0d0))))
             (setf an (+ an 2.0d0))
            label40))
     label50
      (fdo (j 1 (+ j 1))
           ((> j 13) nil)
           (tagbody
             (fset (fref chebmo (m (- (* 2 j) 1)) ((1 maxp1) (1 25)))
                   (fref v (j) ((1 28))))
            label60))
      (fset (fref v (1) ((1 28)))
            (/ (* 2.0d0 (- sinpar (* parint cospar))) par2))
      (fset (fref v (2) ((1 28)))
            (+ (/ (* (+ 18.0d0 (/ -48.0d0 par2)) sinpar) par2)
               (/ (* (- (/ 48.0d0 par2) 2.0d0) cospar) parint)))
      (setf ac (* -24.0d0 parint cospar))
      (setf as (* -8.0d0 sinpar))
      (if (> (dabs parint) 24.0d0) (go label80))
      (setf an 5.0d0)
      (fdo (k 1 (+ k 1))
           ((> k noeq1) nil)
           (tagbody
             (setf an2 (* an an))
             (fset (fref d (k) ((1 25)))
                   (* -2.0d0 (- an2 4.0d0) (- par22 an2 an2)))
             (fset (fref d2 (k) ((1 25))) (* (- an 1.0d0) (- an 2.0d0) par2))
             (fset (fref d1 ((+ k 1)) ((1 25)))
                   (* (+ an 3.0d0) (+ an 4.0d0) par2))
             (fset (fref v ((+ k 2)) ((1 28))) (+ ac (* (- an2 4.0d0) as)))
             (setf an (+ an 2.0d0))
            label70))
      (setf an2 (* an an))
      (fset (fref d (noequ) ((1 25)))
            (* -2.0d0 (- an2 4.0d0) (- par22 an2 an2)))
      (fset (fref v ((+ noequ 2)) ((1 28))) (+ ac (* (- an2 4.0d0) as)))
      (fset (fref v (3) ((1 28)))
            (+ (fref v (3) ((1 28))) (* -42.0d0 par2 (fref v (2) ((1 28))))))
      (setf ass (* parint cospar))
      (setf asap
              (/
               (-
                (/
                 (-
                  (/
                   (-
                    (+
                     (/
                      (+ (* (- (* 105.0d0 par2) 63.0d0) ass)
                         (* (- (* 210.0d0 par2) 1.0d0) sinpar))
                      an2)
                     (* (- (* 15.0d0 par2) 1.0d0) sinpar))
                    (* 15.0d0 ass))
                   an2)
                  (* 3.0d0 ass)
                  sinpar)
                 an2)
                sinpar)
               an2))
      (fset (fref v ((+ noequ 2)) ((1 28)))
            (+ (fref v ((+ noequ 2)) ((1 28)))
               (* -2.0d0 asap par2 (- an 1.0d0) (- an 2.0d0))))
      (multiple-value-bind
          (var-0 var-1 var-2 var-3 var-4 var-5)
          (dgtsl noequ d1 d d2 (array-slice v double-float (3) ((1 28))) iers)
        (declare (ignore var-1 var-2 var-3 var-4))
        (when var-0 (setf noequ var-0))
        (when var-5 (setf iers var-5)))
      (go label100)
     label80
      (setf an 3.0d0)
      (fdo (i 3 (+ i 1))
           ((> i 12) nil)
           (tagbody
             (setf an2 (* an an))
             (fset (fref v (i) ((1 28)))
                   (/
                    (+
                     (* (- an2 4.0d0)
                        (+
                         (* 2.0d0
                            (- par22 an2 an2)
                            (fref v ((- i 1)) ((1 28))))
                         as))
                     ac
                     (* (- par2)
                        (+ an 1.0d0)
                        (+ an 2.0d0)
                        (fref v ((- i 2)) ((1 28)))))
                    (* par2 (- an 1.0d0) (- an 2.0d0))))
             (setf an (+ an 2.0d0))
            label90))
     label100
      (fdo (j 1 (+ j 1))
           ((> j 12) nil)
           (tagbody
             (fset (fref chebmo (m (* 2 j)) ((1 maxp1) (1 25)))
                   (fref v (j) ((1 28))))
            label110))
     label120
      (if (< nrmom momcom) (setf m (+ nrmom 1)))
      (if (and (< momcom (- maxp1 1)) (>= nrmom momcom))
          (setf momcom (+ momcom 1)))
      (fset (fref fval (1) ((1 25))) (* 0.5d0 (funcall f (+ centr hlgth))))
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
             (setf isym (- 26 i))
             (fset (fref fval (i) ((1 25)))
                   (funcall f (+ (* hlgth (fref x ((- i 1)) ((1 11)))) centr)))
             (fset (fref fval (isym) ((1 25)))
                   (funcall f (- centr (* hlgth (fref x ((- i 1)) ((1 11)))))))
            label130))
      (dqcheb x fval cheb12 cheb24)
      (setf resc12
              (* (fref cheb12 (13) ((1 13)))
                 (fref chebmo (m 13) ((1 maxp1) (1 25)))))
      (setf ress12 0.0d0)
      (setf k 11)
      (fdo (j 1 (+ j 1))
           ((> j 6) nil)
           (tagbody
             (setf resc12
                     (+ resc12
                        (* (fref cheb12 (k) ((1 13)))
                           (fref chebmo (m k) ((1 maxp1) (1 25))))))
             (setf ress12
                     (+ ress12
                        (* (fref cheb12 ((+ k 1)) ((1 13)))
                           (fref chebmo (m (+ k 1)) ((1 maxp1) (1 25))))))
             (setf k (- k 2))
            label140))
      (setf resc24
              (* (fref cheb24 (25) ((1 25)))
                 (fref chebmo (m 25) ((1 maxp1) (1 25)))))
      (setf ress24 0.0d0)
      (setf resabs (dabs (fref cheb24 (25) ((1 25)))))
      (setf k 23)
      (fdo (j 1 (+ j 1))
           ((> j 12) nil)
           (tagbody
             (setf resc24
                     (+ resc24
                        (* (fref cheb24 (k) ((1 25)))
                           (fref chebmo (m k) ((1 maxp1) (1 25))))))
             (setf ress24
                     (+ ress24
                        (* (fref cheb24 ((+ k 1)) ((1 25)))
                           (fref chebmo (m (+ k 1)) ((1 maxp1) (1 25))))))
             (setf resabs
                     (+ (dabs (fref cheb24 (k) ((1 25))))
                        (dabs (fref cheb24 ((+ k 1)) ((1 25))))))
             (setf k (- k 2))
            label150))
      (setf estc (dabs (- resc24 resc12)))
      (setf ests (dabs (- ress24 ress12)))
      (setf resabs (* resabs (dabs hlgth)))
      (if (= integr 2) (go label160))
      (setf result (- (* conc resc24) (* cons ress24)))
      (setf abserr (+ (dabs (* conc estc)) (dabs (* cons ests))))
      (go label170)
     label160
      (setf result (+ (* conc ress24) (* cons resc24)))
      (setf abserr (+ (dabs (* conc ests)) (dabs (* cons estc))))
     label170
      (go end_label)
     end_label
      (return
       (values f
               a
               b
               omega
               integr
               nrmom
               maxp1
               ksave
               result
               abserr
               neval
               resabs
               resasc
               momcom
               chebmo)))))

