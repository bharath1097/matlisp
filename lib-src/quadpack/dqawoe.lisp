;;; Compiled by f2cl version 2.0 beta on 2001/02/23 at 10:09:57
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type 'simple-array)
;;;           (:array-slicing t))

(in-package "QUADPACK")
(use-package :f2cl)

(defun dqawoe
       (f a b omega integr epsabs epsrel limit icall maxp1 result abserr neval
        ier last alist blist rlist elist iord nnlog momcom chebmo)
  (declare (type double-float a b omega epsabs epsrel result abserr)
           (type (array integer4 (*)) iord nnlog)
           (type integer4 integr limit icall maxp1 neval ier last momcom)
           (type (array double-float (*)) alist blist rlist elist chebmo))
  (prog ((extrap nil) (noext nil) (extall nil) (abseps 0.0d0) (area 0.0d0)
         (area1 0.0d0) (area12 0.0d0) (area2 0.0d0) (a1 0.0d0) (a2 0.0d0)
         (b1 0.0d0) (b2 0.0d0) (correc 0.0d0) (defab1 0.0d0) (defab2 0.0d0)
         (defabs 0.0d0) (domega 0.0d0) (dres 0.0d0) (epmach 0.0d0)
         (erlarg 0.0d0) (erlast 0.0d0) (errbnd 0.0d0) (errmax 0.0d0)
         (error1 0.0d0) (erro12 0.0d0) (error2 0.0d0) (errsum 0.0d0)
         (ertest 0.0d0) (oflow 0.0d0) (resabs 0.0d0) (reseps 0.0d0)
         (res3la (make-array 3 :element-type 'double-float))
         (rlist2 (make-array 52 :element-type 'double-float)) (small 0.0d0)
         (uflow 0.0d0) (width 0.0d0) (numrl2 0) (nrmom 0) (nrmax 0) (nres 0)
         (nev 0) (maxerr 0) (ktmin 0) (ksgn 0) (k 0) (jupbnd 0) (iroff3 0)
         (iroff2 0) (iroff1 0) (ierro 0) (id 0))
    (declare (type (array double-float (52)) rlist2)
             (type (array double-float (3)) res3la)
             (type integer4 id ierro iroff1 iroff2 iroff3 jupbnd k ksgn ktmin
              maxerr nev nres nrmax nrmom numrl2)
             (type double-float width uflow small reseps resabs oflow ertest
              errsum error2 erro12 error1 errmax errbnd erlast erlarg epmach
              dres domega defabs defab2 defab1 correc b2 b1 a2 a1 area2 area12
              area1 area abseps)
             (type logical extall noext extrap))
    (declare
     (ftype (function (integer4) (values double-float &rest t)) d1mach))
    (declare
     (ftype (function (double-float double-float) (values double-float))
      dmax1))
    (declare (ftype (function (double-float) (values double-float)) dabs))
    (declare
     (ftype
      (function
       (double-float double-float double-float double-float integer4 integer4
        integer4 integer4 double-float double-float integer4 double-float
        double-float integer4 array-double-float)
       (values &rest t))
      dqc25f))
    (declare
     (ftype
      (function
       (integer4 integer4 integer4 double-float array-double-float
        array-integer4 integer4)
       (values &rest t))
      dqpsrt))
    (declare
     (ftype
      (function
       (integer4 array-double-float double-float double-float
        array-double-float integer4)
       (values &rest t))
      dqelg))
    (setf epmach (d1mach 4))
    (setf ier 0)
    (setf neval 0)
    (setf last 0)
    (setf result 0.0d0)
    (setf abserr 0.0d0)
    (fset (fref alist (1) ((1 limit))) a)
    (fset (fref blist (1) ((1 limit))) b)
    (fset (fref rlist (1) ((1 limit))) 0.0d0)
    (fset (fref elist (1) ((1 limit))) 0.0d0)
    (fset (fref iord (1) ((1 limit))) 0)
    (fset (fref nnlog (1) ((1 limit))) 0)
    (if
     (or (and (/= integr 1) (/= integr 2))
         (and (<= epsabs 0.0d0) (< epsrel (dmax1 (* 50.0d0 epmach) 5.0d-29)))
         (< icall 1)
         (< maxp1 1))
     (setf ier 6))
    (if (= ier 6) (go label999))
    (setf domega (dabs omega))
    (setf nrmom 0)
    (if (> icall 1) (go label5))
    (setf momcom 0)
   label5
    (multiple-value-bind
        (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10
         var-11 var-12 var-13 var-14)
        (dqc25f f a b domega integr nrmom maxp1 0 result abserr neval defabs
         resabs momcom chebmo)
      (declare (ignore var-0 var-7 var-14))
      (when var-1 (setf a var-1))
      (when var-2 (setf b var-2))
      (when var-3 (setf domega var-3))
      (when var-4 (setf integr var-4))
      (when var-5 (setf nrmom var-5))
      (when var-6 (setf maxp1 var-6))
      (when var-8 (setf result var-8))
      (when var-9 (setf abserr var-9))
      (when var-10 (setf neval var-10))
      (when var-11 (setf defabs var-11))
      (when var-12 (setf resabs var-12))
      (when var-13 (setf momcom var-13)))
    (setf dres (dabs result))
    (setf errbnd (dmax1 epsabs (* epsrel dres)))
    (fset (fref rlist (1) ((1 limit))) result)
    (fset (fref elist (1) ((1 limit))) abserr)
    (fset (fref iord (1) ((1 limit))) 1)
    (if (and (<= abserr (* 100.0d0 epmach defabs)) (> abserr errbnd))
        (setf ier 2))
    (if (= limit 1) (setf ier 1))
    (if (or (/= ier 0) (<= abserr errbnd)) (go label200))
    (setf uflow (d1mach 1))
    (setf oflow (d1mach 2))
    (setf errmax abserr)
    (setf maxerr 1)
    (setf area result)
    (setf errsum abserr)
    (setf abserr oflow)
    (setf nrmax 1)
    (setf extrap %false%)
    (setf noext %false%)
    (setf ierro 0)
    (setf iroff1 0)
    (setf iroff2 0)
    (setf iroff3 0)
    (setf ktmin 0)
    (setf small (* (dabs (- b a)) 0.75d0))
    (setf nres 0)
    (setf numrl2 0)
    (setf extall %false%)
    (if (> (* 0.5d0 (dabs (- b a)) domega) 2.0d0) (go label10))
    (setf numrl2 1)
    (setf extall %true%)
    (fset (fref rlist2 (1) ((1 52))) result)
   label10
    (if (<= (* 0.25d0 (dabs (- b a)) domega) 2.0d0) (setf extall %true%))
    (setf ksgn -1)
    (if (>= dres (* (- 1.0d0 (* 50.0d0 epmach)) defabs)) (setf ksgn 1))
    (fdo (last 2 (+ last 1))
         ((> last limit) nil)
         (tagbody
           (setf nrmom (+ (fref nnlog (maxerr) ((1 limit))) 1))
           (setf a1 (fref alist (maxerr) ((1 limit))))
           (setf b1
                   (* 0.5d0
                      (+ (fref alist (maxerr) ((1 limit)))
                         (fref blist (maxerr) ((1 limit))))))
           (setf a2 b1)
           (setf b2 (fref blist (maxerr) ((1 limit))))
           (setf erlast errmax)
           (multiple-value-bind
               (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                var-10 var-11 var-12 var-13 var-14)
               (dqc25f f a1 b1 domega integr nrmom maxp1 0 area1 error1 nev
                resabs defab1 momcom chebmo)
             (declare (ignore var-0 var-7 var-14))
             (when var-1 (setf a1 var-1))
             (when var-2 (setf b1 var-2))
             (when var-3 (setf domega var-3))
             (when var-4 (setf integr var-4))
             (when var-5 (setf nrmom var-5))
             (when var-6 (setf maxp1 var-6))
             (when var-8 (setf area1 var-8))
             (when var-9 (setf error1 var-9))
             (when var-10 (setf nev var-10))
             (when var-11 (setf resabs var-11))
             (when var-12 (setf defab1 var-12))
             (when var-13 (setf momcom var-13)))
           (setf neval (+ neval nev))
           (multiple-value-bind
               (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                var-10 var-11 var-12 var-13 var-14)
               (dqc25f f a2 b2 domega integr nrmom maxp1 1 area2 error2 nev
                resabs defab2 momcom chebmo)
             (declare (ignore var-0 var-7 var-14))
             (when var-1 (setf a2 var-1))
             (when var-2 (setf b2 var-2))
             (when var-3 (setf domega var-3))
             (when var-4 (setf integr var-4))
             (when var-5 (setf nrmom var-5))
             (when var-6 (setf maxp1 var-6))
             (when var-8 (setf area2 var-8))
             (when var-9 (setf error2 var-9))
             (when var-10 (setf nev var-10))
             (when var-11 (setf resabs var-11))
             (when var-12 (setf defab2 var-12))
             (when var-13 (setf momcom var-13)))
           (setf neval (+ neval nev))
           (setf area12 (+ area1 area2))
           (setf erro12 (+ error1 error2))
           (setf errsum (- (+ errsum erro12) errmax))
           (setf area (- (+ area area12) (fref rlist (maxerr) ((1 limit)))))
           (if (or (= defab1 error1) (= defab2 error2)) (go label25))
           (if
            (or
             (> (dabs (- (fref rlist (maxerr) ((1 limit))) area12))
                (* 1.0d-5 (dabs area12)))
             (< erro12 (* 0.99d0 errmax)))
            (go label20))
           (if extrap (setf iroff2 (+ iroff2 1)))
           (if (not extrap) (setf iroff1 (+ iroff1 1)))
          label20
           (if (and (> last 10) (> erro12 errmax)) (setf iroff3 (+ iroff3 1)))
          label25
           (fset (fref rlist (maxerr) ((1 limit))) area1)
           (fset (fref rlist (last) ((1 limit))) area2)
           (fset (fref nnlog (maxerr) ((1 limit))) nrmom)
           (fset (fref nnlog (last) ((1 limit))) nrmom)
           (setf errbnd (dmax1 epsabs (* epsrel (dabs area))))
           (if (or (>= (+ iroff1 iroff2) 10) (>= iroff3 20)) (setf ier 2))
           (if (>= iroff2 5) (setf ierro 3))
           (if (= last limit) (setf ier 1))
           (if
            (<= (dmax1 (dabs a1) (dabs b2))
                (* (+ 1.0d0 (* 100.0d0 epmach))
                   (+ (dabs a2) (* 1000.0d0 uflow))))
            (setf ier 4))
           (if (> error2 error1) (go label30))
           (fset (fref alist (last) ((1 limit))) a2)
           (fset (fref blist (maxerr) ((1 limit))) b1)
           (fset (fref blist (last) ((1 limit))) b2)
           (fset (fref elist (maxerr) ((1 limit))) error1)
           (fset (fref elist (last) ((1 limit))) error2)
           (go label40)
          label30
           (fset (fref alist (maxerr) ((1 limit))) a2)
           (fset (fref alist (last) ((1 limit))) a1)
           (fset (fref blist (last) ((1 limit))) b1)
           (fset (fref rlist (maxerr) ((1 limit))) area2)
           (fset (fref rlist (last) ((1 limit))) area1)
           (fset (fref elist (maxerr) ((1 limit))) error2)
           (fset (fref elist (last) ((1 limit))) error1)
          label40
           (multiple-value-bind
               (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
               (dqpsrt limit last maxerr errmax elist iord nrmax)
             (declare (ignore var-4 var-5))
             (when var-0 (setf limit var-0))
             (when var-1 (setf last var-1))
             (when var-2 (setf maxerr var-2))
             (when var-3 (setf errmax var-3))
             (when var-6 (setf nrmax var-6)))
           (if (<= errsum errbnd) (go label170))
           (if (/= ier 0) (go label150))
           (if (and (= last 2) extall) (go label120))
           (if noext (go label140))
           (if (not extall) (go label50))
           (setf erlarg (- erlarg erlast))
           (if (> (dabs (- b1 a1)) small) (setf erlarg (+ erlarg erro12)))
           (if extrap (go label70))
          label50
           (setf width
                   (dabs
                    (- (fref blist (maxerr) ((1 limit)))
                       (fref alist (maxerr) ((1 limit))))))
           (if (> width small) (go label140))
           (if extall (go label60))
           (setf small (* small 0.5d0))
           (if (> (* 0.25d0 width domega) 2.0d0) (go label140))
           (setf extall %true%)
           (go label130)
          label60
           (setf extrap %true%)
           (setf nrmax 2)
          label70
           (if (or (= ierro 3) (<= erlarg ertest)) (go label90))
           (setf jupbnd last)
           (if (> last (+ (truncate limit 2) 2))
               (setf jupbnd (- (+ limit 3) last)))
           (setf id nrmax)
           (fdo (k id (+ k 1))
                ((> k jupbnd) nil)
                (tagbody
                  (setf maxerr (fref iord (nrmax) ((1 limit))))
                  (setf errmax (fref elist (maxerr) ((1 limit))))
                  (if
                   (>
                    (dabs
                     (- (fref blist (maxerr) ((1 limit)))
                        (fref alist (maxerr) ((1 limit)))))
                    small)
                   (go label140))
                  (setf nrmax (+ nrmax 1))
                 label80))
          label90
           (setf numrl2 (+ numrl2 1))
           (fset (fref rlist2 (numrl2) ((1 52))) area)
           (if (< numrl2 3) (go label110))
           (multiple-value-bind
               (var-0 var-1 var-2 var-3 var-4 var-5)
               (dqelg numrl2 rlist2 reseps abseps res3la nres)
             (declare (ignore var-1 var-4))
             (when var-0 (setf numrl2 var-0))
             (when var-2 (setf reseps var-2))
             (when var-3 (setf abseps var-3))
             (when var-5 (setf nres var-5)))
           (setf ktmin (+ ktmin 1))
           (if (and (> ktmin 5) (< abserr (* 0.001d0 errsum))) (setf ier 5))
           (if (>= abseps abserr) (go label100))
           (setf ktmin 0)
           (setf abserr abseps)
           (setf result reseps)
           (setf correc erlarg)
           (setf ertest (dmax1 epsabs (* epsrel (dabs reseps))))
           (if (<= abserr ertest) (go label150))
          label100
           (if (= numrl2 1) (setf noext %true%))
           (if (= ier 5) (go label150))
          label110
           (setf maxerr (fref iord (1) ((1 limit))))
           (setf errmax (fref elist (maxerr) ((1 limit))))
           (setf nrmax 1)
           (setf extrap %false%)
           (setf small (* small 0.5d0))
           (setf erlarg errsum)
           (go label140)
          label120
           (setf small (* small 0.5d0))
           (setf numrl2 (+ numrl2 1))
           (fset (fref rlist2 (numrl2) ((1 52))) area)
          label130
           (setf ertest errbnd)
           (setf erlarg errsum)
          label140))
   label150
    (if (or (= abserr oflow) (= nres 0)) (go label170))
    (if (= (+ ier ierro) 0) (go label165))
    (if (= ierro 3) (setf abserr (+ abserr correc)))
    (if (= ier 0) (setf ier 3))
    (if (and (/= result 0.0d0) (/= area 0.0d0)) (go label160))
    (if (> abserr errsum) (go label170))
    (if (= area 0.0d0) (go label190))
    (go label165)
   label160
    (if (> (/ abserr (dabs result)) (/ errsum (dabs area))) (go label170))
   label165
    (if
     (and (= ksgn -1)
          (<= (dmax1 (dabs result) (dabs area))
              (* defabs 0.010000000000000002d0)))
     (go label190))
    (if
     (or (> 0.010000000000000002d0 (/ result area))
         (> (/ result area) 100.0d0)
         (>= errsum (dabs area)))
     (setf ier 6))
    (go label190)
   label170
    (setf result 0.0d0)
    (fdo (k 1 (+ k 1))
         ((> k last) nil)
         (tagbody
           (setf result (+ result (fref rlist (k) ((1 limit)))))
          label180))
    (setf abserr errsum)
   label190
    (if (> ier 2) (setf ier (- ier 1)))
   label200
    (if (and (= integr 2) (< omega 0.0d0)) (setf result (- result)))
   label999
    (go end_label)
   end_label
    (return
     (values f
             a
             b
             omega
             integr
             epsabs
             epsrel
             limit
             icall
             maxp1
             result
             abserr
             neval
             ier
             last
             alist
             blist
             rlist
             elist
             iord
             nnlog
             momcom
             chebmo))))

