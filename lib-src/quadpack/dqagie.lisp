;;; Compiled by f2cl version 2.0 beta on 2001/02/23 at 10:09:37
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type 'simple-array)
;;;           (:array-slicing t))

(in-package "QUADPACK")
(use-package :f2cl)

(defun dqagie
       (f bound inf epsabs epsrel limit result abserr neval ier alist blist
        rlist elist iord last)
  (declare (type double-float bound epsabs epsrel result abserr)
           (type (array double-float (*)) alist blist rlist elist)
           (type (array integer4 (*)) iord)
           (type integer4 inf limit neval ier last))
  (prog ((extrap nil) (noext nil) (abseps 0.0d0) (area 0.0d0) (area1 0.0d0)
         (area12 0.0d0) (area2 0.0d0) (a1 0.0d0) (a2 0.0d0) (boun 0.0d0)
         (b1 0.0d0) (b2 0.0d0) (correc 0.0d0) (defabs 0.0d0) (defab1 0.0d0)
         (defab2 0.0d0) (dres 0.0d0) (epmach 0.0d0) (erlarg 0.0d0)
         (erlast 0.0d0) (errbnd 0.0d0) (errmax 0.0d0) (error1 0.0d0)
         (error2 0.0d0) (erro12 0.0d0) (errsum 0.0d0) (ertest 0.0d0)
         (oflow 0.0d0) (resabs 0.0d0) (reseps 0.0d0)
         (res3la (make-array 3 :element-type 'double-float))
         (rlist2 (make-array 52 :element-type 'double-float)) (small 0.0d0)
         (uflow 0.0d0) (numrl2 0) (nrmax 0) (nres 0) (maxerr 0) (ktmin 0)
         (ksgn 0) (k 0) (jupbnd 0) (iroff3 0) (iroff2 0) (iroff1 0) (ierro 0)
         (id 0))
    (declare (type (array double-float (3)) res3la)
             (type (array double-float (52)) rlist2)
             (type integer4 id ierro iroff1 iroff2 iroff3 jupbnd k ksgn ktmin
              maxerr nres nrmax numrl2)
             (type double-float uflow small reseps resabs oflow ertest errsum
              erro12 error2 error1 errmax errbnd erlast erlarg epmach dres
              defab2 defab1 defabs correc b2 b1 boun a2 a1 area2 area12 area1
              area abseps)
             (type logical noext extrap))
    (declare
     (ftype (function (integer4) (values double-float &rest t)) d1mach))
    (declare
     (ftype (function (double-float double-float) (values double-float))
      dmax1))
    (declare
     (ftype
      (function
       (double-float double-float integer4 double-float double-float
        double-float double-float double-float double-float)
       (values &rest t))
      dqk15i))
    (declare (ftype (function (double-float) (values double-float)) dabs))
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
    (fset (fref alist (1) ((1 limit))) 0.0d0)
    (fset (fref blist (1) ((1 limit))) 1.0d0)
    (fset (fref rlist (1) ((1 limit))) 0.0d0)
    (fset (fref elist (1) ((1 limit))) 0.0d0)
    (fset (fref iord (1) ((1 limit))) 0)
    (if (and (<= epsabs 0.0d0) (< epsrel (dmax1 (* 50.0d0 epmach) 5.0d-29)))
        (setf ier 6))
    (if (= ier 6) (go label999))
    (setf boun bound)
    (if (= inf 2) (setf boun 0.0d0))
    (multiple-value-bind
        (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
        (dqk15i f boun inf 0.0d0 1.0d0 result abserr defabs resabs)
      (declare (ignore var-0 var-3 var-4))
      (when var-1 (setf boun var-1))
      (when var-2 (setf inf var-2))
      (when var-5 (setf result var-5))
      (when var-6 (setf abserr var-6))
      (when var-7 (setf defabs var-7))
      (when var-8 (setf resabs var-8)))
    (setf last 1)
    (fset (fref rlist (1) ((1 limit))) result)
    (fset (fref elist (1) ((1 limit))) abserr)
    (fset (fref iord (1) ((1 limit))) 1)
    (setf dres (dabs result))
    (setf errbnd (dmax1 epsabs (* epsrel dres)))
    (if (and (<= abserr (* 100.0d0 epmach defabs)) (> abserr errbnd))
        (setf ier 2))
    (if (= limit 1) (setf ier 1))
    (if
     (or (/= ier 0)
         (and (<= abserr errbnd) (/= abserr resabs))
         (= abserr 0.0d0))
     (go label130))
    (setf uflow (d1mach 1))
    (setf oflow (d1mach 2))
    (fset (fref rlist2 (1) ((1 52))) result)
    (setf errmax abserr)
    (setf maxerr 1)
    (setf area result)
    (setf errsum abserr)
    (setf abserr oflow)
    (setf nrmax 1)
    (setf nres 0)
    (setf ktmin 0)
    (setf numrl2 2)
    (setf extrap %false%)
    (setf noext %false%)
    (setf ierro 0)
    (setf iroff1 0)
    (setf iroff2 0)
    (setf iroff3 0)
    (setf ksgn -1)
    (if (>= dres (* (- 1.0d0 (* 50.0d0 epmach)) defabs)) (setf ksgn 1))
    (fdo (last 2 (+ last 1))
         ((> last limit) nil)
         (tagbody
           (setf a1 (fref alist (maxerr) ((1 limit))))
           (setf b1
                   (* 0.5d0
                      (+ (fref alist (maxerr) ((1 limit)))
                         (fref blist (maxerr) ((1 limit))))))
           (setf a2 b1)
           (setf b2 (fref blist (maxerr) ((1 limit))))
           (setf erlast errmax)
           (multiple-value-bind
               (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
               (dqk15i f boun inf a1 b1 area1 error1 resabs defab1)
             (declare (ignore var-0))
             (when var-1 (setf boun var-1))
             (when var-2 (setf inf var-2))
             (when var-3 (setf a1 var-3))
             (when var-4 (setf b1 var-4))
             (when var-5 (setf area1 var-5))
             (when var-6 (setf error1 var-6))
             (when var-7 (setf resabs var-7))
             (when var-8 (setf defab1 var-8)))
           (multiple-value-bind
               (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
               (dqk15i f boun inf a2 b2 area2 error2 resabs defab2)
             (declare (ignore var-0))
             (when var-1 (setf boun var-1))
             (when var-2 (setf inf var-2))
             (when var-3 (setf a2 var-3))
             (when var-4 (setf b2 var-4))
             (when var-5 (setf area2 var-5))
             (when var-6 (setf error2 var-6))
             (when var-7 (setf resabs var-7))
             (when var-8 (setf defab2 var-8)))
           (setf area12 (+ area1 area2))
           (setf erro12 (+ error1 error2))
           (setf errsum (- (+ errsum erro12) errmax))
           (setf area (- (+ area area12) (fref rlist (maxerr) ((1 limit)))))
           (if (or (= defab1 error1) (= defab2 error2)) (go label15))
           (if
            (or
             (> (dabs (- (fref rlist (maxerr) ((1 limit))) area12))
                (* 1.0d-5 (dabs area12)))
             (< erro12 (* 0.99d0 errmax)))
            (go label10))
           (if extrap (setf iroff2 (+ iroff2 1)))
           (if (not extrap) (setf iroff1 (+ iroff1 1)))
          label10
           (if (and (> last 10) (> erro12 errmax)) (setf iroff3 (+ iroff3 1)))
          label15
           (fset (fref rlist (maxerr) ((1 limit))) area1)
           (fset (fref rlist (last) ((1 limit))) area2)
           (setf errbnd (dmax1 epsabs (* epsrel (dabs area))))
           (if (or (>= (+ iroff1 iroff2) 10) (>= iroff3 20)) (setf ier 2))
           (if (>= iroff2 5) (setf ierro 3))
           (if (= last limit) (setf ier 1))
           (if
            (<= (dmax1 (dabs a1) (dabs b2))
                (* (+ 1.0d0 (* 100.0d0 epmach))
                   (+ (dabs a2) (* 1000.0d0 uflow))))
            (setf ier 4))
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
           (if (<= errsum errbnd) (go label115))
           (if (/= ier 0) (go label100))
           (if (= last 2) (go label80))
           (if noext (go label90))
           (setf erlarg (- erlarg erlast))
           (if (> (dabs (- b1 a1)) small) (setf erlarg (+ erlarg erro12)))
           (if extrap (go label40))
           (if
            (>
             (dabs
              (- (fref blist (maxerr) ((1 limit)))
                 (fref alist (maxerr) ((1 limit)))))
             small)
            (go label90))
           (setf extrap %true%)
           (setf nrmax 2)
          label40
           (if (or (= ierro 3) (<= erlarg ertest)) (go label60))
           (setf id nrmax)
           (setf jupbnd last)
           (if (> last (+ 2 (truncate limit 2)))
               (setf jupbnd (- (+ limit 3) last)))
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
                   (go label90))
                  (setf nrmax (+ nrmax 1))
                 label50))
          label60
           (setf numrl2 (+ numrl2 1))
           (fset (fref rlist2 (numrl2) ((1 52))) area)
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
           (if (>= abseps abserr) (go label70))
           (setf ktmin 0)
           (setf abserr abseps)
           (setf result reseps)
           (setf correc erlarg)
           (setf ertest (dmax1 epsabs (* epsrel (dabs reseps))))
           (if (<= abserr ertest) (go label100))
          label70
           (if (= numrl2 1) (setf noext %true%))
           (if (= ier 5) (go label100))
           (setf maxerr (fref iord (1) ((1 limit))))
           (setf errmax (fref elist (maxerr) ((1 limit))))
           (setf nrmax 1)
           (setf extrap %false%)
           (setf small (* small 0.5d0))
           (setf erlarg errsum)
           (go label90)
          label80
           (setf small 0.375d0)
           (setf erlarg errsum)
           (setf ertest errbnd)
           (fset (fref rlist2 (2) ((1 52))) area)
          label90))
   label100
    (if (= abserr oflow) (go label115))
    (if (= (+ ier ierro) 0) (go label110))
    (if (= ierro 3) (setf abserr (+ abserr correc)))
    (if (= ier 0) (setf ier 3))
    (if (and (/= result 0.0d0) (/= area 0.0d0)) (go label105))
    (if (> abserr errsum) (go label115))
    (if (= area 0.0d0) (go label130))
    (go label110)
   label105
    (if (> (/ abserr (dabs result)) (/ errsum (dabs area))) (go label115))
   label110
    (if
     (and (= ksgn -1)
          (<= (dmax1 (dabs result) (dabs area))
              (* defabs 0.010000000000000002d0)))
     (go label130))
    (if
     (or (> 0.010000000000000002d0 (/ result area))
         (> (/ result area) 100.0d0)
         (> errsum (dabs area)))
     (setf ier 6))
    (go label130)
   label115
    (setf result 0.0d0)
    (fdo (k 1 (+ k 1))
         ((> k last) nil)
         (tagbody
           (setf result (+ result (fref rlist (k) ((1 limit)))))
          label120))
    (setf abserr errsum)
   label130
    (setf neval (- (* 30 last) 15))
    (if (= inf 2) (setf neval (* 2 neval)))
    (if (> ier 2) (setf ier (- ier 1)))
   label999
    (go end_label)
   end_label
    (return
     (values f
             bound
             inf
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

