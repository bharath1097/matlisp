;;; Compiled by f2cl version 2.0 beta on 2001/02/23 at 10:10:08
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type 'simple-array)
;;;           (:array-slicing t))

(in-package "QUADPACK")
(use-package :f2cl)

(defun dqawce
       (f a b c epsabs epsrel limit result abserr neval ier alist blist rlist
        elist iord last)
  (declare (type double-float a b c epsabs epsrel result abserr)
           (type (array double-float (*)) alist blist rlist elist)
           (type (array integer4 (*)) iord)
           (type integer4 limit neval ier last))
  (prog ((iroff1 0) (iroff2 0) (k 0) (krule 0) (maxerr 0) (nev 0) (nrmax 0)
         (uflow 0.0d0) (errsum 0.0d0) (error2 0.0d0) (erro12 0.0d0)
         (error1 0.0d0) (errmax 0.0d0) (errbnd 0.0d0) (epmach 0.0d0) (b2 0.0d0)
         (b1 0.0d0) (bb 0.0d0) (a2 0.0d0) (a1 0.0d0) (area2 0.0d0)
         (area12 0.0d0) (area1 0.0d0) (area 0.0d0) (aa 0.0d0))
    (declare
     (type double-float aa area area1 area12 area2 a1 a2 bb b1 b2 epmach errbnd
      errmax error1 erro12 error2 errsum uflow)
     (type integer4 nrmax nev maxerr krule k iroff2 iroff1))
    (declare
     (ftype (function (integer4) (values double-float &rest t)) d1mach))
    (declare
     (ftype (function (double-float double-float) (values double-float))
      dmax1))
    (declare
     (ftype
      (function
       (double-float double-float double-float double-float double-float
        double-float integer4 integer4)
       (values &rest t))
      dqc25c))
    (declare (ftype (function (double-float) (values double-float)) dabs))
    (declare
     (ftype (function (double-float double-float) (values double-float))
      dmin1))
    (declare
     (ftype
      (function
       (integer4 integer4 integer4 double-float array-double-float
        array-integer4 integer4)
       (values &rest t))
      dqpsrt))
    (setf epmach (d1mach 4))
    (setf uflow (d1mach 1))
    (setf ier 6)
    (setf neval 0)
    (setf last 0)
    (fset (fref alist (1) ((1 limit))) a)
    (fset (fref blist (1) ((1 limit))) b)
    (fset (fref rlist (1) ((1 limit))) 0.0d0)
    (fset (fref elist (1) ((1 limit))) 0.0d0)
    (fset (fref iord (1) ((1 limit))) 0)
    (setf result 0.0d0)
    (setf abserr 0.0d0)
    (if
     (or (= c a)
         (= c b)
         (and (<= epsabs 0.0d0) (< epsrel (dmax1 (* 50.0d0 epmach) 5.0d-29))))
     (go label999))
    (setf aa a)
    (setf bb b)
    (if (<= a b) (go label10))
    (setf aa b)
    (setf bb a)
   label10
    (setf ier 0)
    (setf krule 1)
    (multiple-value-bind
        (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
        (dqc25c f aa bb c result abserr krule neval)
      (declare (ignore var-0))
      (when var-1 (setf aa var-1))
      (when var-2 (setf bb var-2))
      (when var-3 (setf c var-3))
      (when var-4 (setf result var-4))
      (when var-5 (setf abserr var-5))
      (when var-6 (setf krule var-6))
      (when var-7 (setf neval var-7)))
    (setf last 1)
    (fset (fref rlist (1) ((1 limit))) result)
    (fset (fref elist (1) ((1 limit))) abserr)
    (fset (fref iord (1) ((1 limit))) 1)
    (fset (fref alist (1) ((1 limit))) a)
    (fset (fref blist (1) ((1 limit))) b)
    (setf errbnd (dmax1 epsabs (* epsrel (dabs result))))
    (if (= limit 1) (setf ier 1))
    (if
     (or (< abserr (dmin1 (* 0.010000000000000002d0 (dabs result)) errbnd))
         (= ier 1))
     (go label70))
    (fset (fref alist (1) ((1 limit))) aa)
    (fset (fref blist (1) ((1 limit))) bb)
    (fset (fref rlist (1) ((1 limit))) result)
    (setf errmax abserr)
    (setf maxerr 1)
    (setf area result)
    (setf errsum abserr)
    (setf nrmax 1)
    (setf iroff1 0)
    (setf iroff2 0)
    (fdo (last 2 (+ last 1))
         ((> last limit) nil)
         (tagbody
           (setf a1 (fref alist (maxerr) ((1 limit))))
           (setf b1
                   (* 0.5d0
                      (+ (fref alist (maxerr) ((1 limit)))
                         (fref blist (maxerr) ((1 limit))))))
           (setf b2 (fref blist (maxerr) ((1 limit))))
           (if (and (<= c b1) (> c a1)) (setf b1 (* 0.5d0 (+ c b2))))
           (if (and (> c b1) (< c b2)) (setf b1 (* 0.5d0 (+ a1 c))))
           (setf a2 b1)
           (setf krule 2)
           (multiple-value-bind
               (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
               (dqc25c f a1 b1 c area1 error1 krule nev)
             (declare (ignore var-0))
             (when var-1 (setf a1 var-1))
             (when var-2 (setf b1 var-2))
             (when var-3 (setf c var-3))
             (when var-4 (setf area1 var-4))
             (when var-5 (setf error1 var-5))
             (when var-6 (setf krule var-6))
             (when var-7 (setf nev var-7)))
           (setf neval (+ neval nev))
           (multiple-value-bind
               (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
               (dqc25c f a2 b2 c area2 error2 krule nev)
             (declare (ignore var-0))
             (when var-1 (setf a2 var-1))
             (when var-2 (setf b2 var-2))
             (when var-3 (setf c var-3))
             (when var-4 (setf area2 var-4))
             (when var-5 (setf error2 var-5))
             (when var-6 (setf krule var-6))
             (when var-7 (setf nev var-7)))
           (setf neval (+ neval nev))
           (setf area12 (+ area1 area2))
           (setf erro12 (+ error1 error2))
           (setf errsum (- (+ errsum erro12) errmax))
           (setf area (- (+ area area12) (fref rlist (maxerr) ((1 limit)))))
           (if
            (and
             (< (dabs (- (fref rlist (maxerr) ((1 limit))) area12))
                (* 1.0d-5 (dabs area12)))
             (>= erro12 (* 0.99d0 errmax))
             (= krule 0))
            (setf iroff1 (+ iroff1 1)))
           (if (and (> last 10) (> erro12 errmax) (= krule 0))
               (setf iroff2 (+ iroff2 1)))
           (fset (fref rlist (maxerr) ((1 limit))) area1)
           (fset (fref rlist (last) ((1 limit))) area2)
           (setf errbnd (dmax1 epsabs (* epsrel (dabs area))))
           (if (<= errsum errbnd) (go label15))
           (if (and (>= iroff1 6) (> iroff2 20)) (setf ier 2))
           (if (= last limit) (setf ier 1))
           (if
            (<= (dmax1 (dabs a1) (dabs b2))
                (* (+ 1.0d0 (* 100.0d0 epmach))
                   (+ (dabs a2) (* 1000.0d0 uflow))))
            (setf ier 3))
          label15
           (if (> error2 error1) (go label20))
           (fset (fref alist (last) ((1 limit))) a2)
           (fset (fref blist (maxerr) ((1 limit))) b1)
           (fset (fref blist (last) ((1 limit))) b2)
           (fset (fref elist (maxerr) ((1 limit))) error1)
           (fset (fref elist (last) ((1 limit))) error2)
           (go label30)
          label20
           (fset (fref alist (maxerr) ((1 limit))) a2)
           (fset (fref alist (last) ((1 limit))) a1)
           (fset (fref blist (last) ((1 limit))) b1)
           (fset (fref rlist (maxerr) ((1 limit))) area2)
           (fset (fref rlist (last) ((1 limit))) area1)
           (fset (fref elist (maxerr) ((1 limit))) error2)
           (fset (fref elist (last) ((1 limit))) error1)
          label30
           (multiple-value-bind
               (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
               (dqpsrt limit last maxerr errmax elist iord nrmax)
             (declare (ignore var-4 var-5))
             (when var-0 (setf limit var-0))
             (when var-1 (setf last var-1))
             (when var-2 (setf maxerr var-2))
             (when var-3 (setf errmax var-3))
             (when var-6 (setf nrmax var-6)))
           (if (or (/= ier 0) (<= errsum errbnd)) (go label50))
          label40))
   label50
    (setf result 0.0d0)
    (fdo (k 1 (+ k 1))
         ((> k last) nil)
         (tagbody
           (setf result (+ result (fref rlist (k) ((1 limit)))))
          label60))
    (setf abserr errsum)
   label70
    (if (= aa b) (setf result (- result)))
   label999
    (go end_label)
   end_label
    (return
     (values f
             a
             b
             c
             epsabs
             epsrel
             limit
             result
             abserr
             neval
             ier
             alist
             blist
             rlist
             elist
             iord
             last))))

