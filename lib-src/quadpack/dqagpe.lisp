;;; Compiled by f2cl version 2.0 beta on 2001/02/23 at 10:09:41
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type 'simple-array)
;;;           (:array-slicing t))

(in-package "QUADPACK")
(use-package :f2cl)

(defun dqagpe
       (f a b npts2 points epsabs epsrel limit result abserr neval ier alist
        blist rlist elist pts iord level ndin last)
  (declare (type double-float a b epsabs epsrel result abserr)
           (type (array double-float (*)) points alist blist rlist elist pts)
           (type (array integer4 (*)) iord level ndin)
           (type integer4 npts2 limit neval ier last))
  (prog ((extrap nil) (noext nil) (abseps 0.0d0) (area 0.0d0) (area1 0.0d0)
         (area12 0.0d0) (area2 0.0d0) (a1 0.0d0) (a2 0.0d0) (b1 0.0d0)
         (b2 0.0d0) (correc 0.0d0) (defabs 0.0d0) (defab1 0.0d0) (defab2 0.0d0)
         (dres 0.0d0) (epmach 0.0d0) (erlarg 0.0d0) (erlast 0.0d0)
         (errbnd 0.0d0) (errmax 0.0d0) (error1 0.0d0) (erro12 0.0d0)
         (error2 0.0d0) (errsum 0.0d0) (ertest 0.0d0) (oflow 0.0d0)
         (resa 0.0d0) (resabs 0.0d0) (reseps 0.0d0)
         (res3la (make-array 3 :element-type 'double-float))
         (rlist2 (make-array 52 :element-type 'double-float)) (temp 0.0d0)
         (uflow 0.0d0) (numrl2 0) (nrmax 0) (nres 0) (npts 0) (nintp1 0)
         (maxerr 0) (levmax 0) (levcur 0) (ktmin 0) (ksgn 0) (k 0) (jupbnd 0)
         (jlow 0) (j 0) (iroff3 0) (iroff2 0) (iroff1 0) (ip1 0) (ind2 0)
         (ind1 0) (ierro 0) (id 0) (i 0) (nint 0) (sign 0.0))
    (declare (type single-float sign)
             (type (array double-float (3)) res3la)
             (type (array double-float (52)) rlist2)
             (type integer4 nint i id ierro ind1 ind2 ip1 iroff1 iroff2 iroff3
              j jlow jupbnd k ksgn ktmin levcur levmax maxerr nintp1 npts nres
              nrmax numrl2)
             (type double-float uflow temp reseps resabs resa oflow ertest
              errsum error2 erro12 error1 errmax errbnd erlast erlarg epmach
              dres defab2 defab1 defabs correc b2 b1 a2 a1 area2 area12 area1
              area abseps)
             (type logical noext extrap))
    (declare
     (ftype (function (integer4) (values double-float &rest t)) d1mach))
    (declare
     (ftype (function (double-float double-float) (values double-float))
      dmax1))
    (declare
     (ftype (function (double-float double-float) (values double-float))
      dmin1))
    (declare
     (ftype
      (function
       (double-float double-float double-float double-float double-float
        double-float double-float)
       (values &rest t))
      dqk21))
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
    (fset (fref alist (1) ((1 limit))) a)
    (fset (fref blist (1) ((1 limit))) b)
    (fset (fref rlist (1) ((1 limit))) 0.0d0)
    (fset (fref elist (1) ((1 limit))) 0.0d0)
    (fset (fref iord (1) ((1 limit))) 0)
    (fset (fref level (1) ((1 limit))) 0)
    (setf npts (- npts2 2))
    (if
     (or (< npts2 2)
         (<= limit npts)
         (and (<= epsabs 0.0d0) (< epsrel (dmax1 (* 50.0d0 epmach) 5.0d-29))))
     (setf ier 6))
    (if (= ier 6) (go label999))
    (setf sign (coerce 1.0d0 'single-float))
    (if (> a b) (setf sign (coerce -1.0d0 'single-float)))
    (fset (fref pts (1) ((1 npts2))) (dmin1 a b))
    (if (= npts 0) (go label15))
    (fdo (i 1 (+ i 1))
         ((> i npts) nil)
         (tagbody
           (fset (fref pts ((+ i 1)) ((1 npts2)))
                 (fref points (i) ((1 npts2))))
          label10))
   label15
    (fset (fref pts ((+ npts 2)) ((1 npts2))) (dmax1 a b))
    (setf nint (+ npts 1))
    (setf a1 (fref pts (1) ((1 npts2))))
    (if (= npts 0) (go label40))
    (setf nintp1 (+ nint 1))
    (fdo (i 1 (+ i 1))
         ((> i nint) nil)
         (tagbody
           (setf ip1 (+ i 1))
           (fdo (j ip1 (+ j 1))
                ((> j nintp1) nil)
                (tagbody
                  (if
                   (<= (fref pts (i) ((1 npts2))) (fref pts (j) ((1 npts2))))
                   (go label20))
                  (setf temp (fref pts (i) ((1 npts2))))
                  (fset (fref pts (i) ((1 npts2))) (fref pts (j) ((1 npts2))))
                  (fset (fref pts (j) ((1 npts2))) temp)))))
   label20
    (if
     (or (/= (fref pts (1) ((1 npts2))) (dmin1 a b))
         (/= (fref pts (nintp1) ((1 npts2))) (dmax1 a b)))
     (setf ier 6))
    (if (= ier 6) (go label999))
   label40
    (setf resabs 0.0d0)
    (fdo (i 1 (+ i 1))
         ((> i nint) nil)
         (tagbody
           (setf b1 (fref pts ((+ i 1)) ((1 npts2))))
           (multiple-value-bind
               (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
               (dqk21 f a1 b1 area1 error1 defabs resa)
             (declare (ignore var-0))
             (when var-1 (setf a1 var-1))
             (when var-2 (setf b1 var-2))
             (when var-3 (setf area1 var-3))
             (when var-4 (setf error1 var-4))
             (when var-5 (setf defabs var-5))
             (when var-6 (setf resa var-6)))
           (setf abserr (+ abserr error1))
           (setf result (+ result area1))
           (fset (fref ndin (i) ((1 npts2))) 0)
           (if (and (= error1 resa) (/= error1 0.0d0))
               (fset (fref ndin (i) ((1 npts2))) 1))
           (setf resabs (+ resabs defabs))
           (fset (fref level (i) ((1 limit))) 0)
           (fset (fref elist (i) ((1 limit))) error1)
           (fset (fref alist (i) ((1 limit))) a1)
           (fset (fref blist (i) ((1 limit))) b1)
           (fset (fref rlist (i) ((1 limit))) area1)
           (fset (fref iord (i) ((1 limit))) i)
           (setf a1 b1)
          label50))
    (setf errsum 0.0d0)
    (fdo (i 1 (+ i 1))
         ((> i nint) nil)
         (tagbody
           (if (= (fref ndin (i) ((1 npts2))) 1)
               (fset (fref elist (i) ((1 limit))) abserr))
           (setf errsum (+ errsum (fref elist (i) ((1 limit)))))
          label55))
    (setf last nint)
    (setf neval (* 21 nint))
    (setf dres (dabs result))
    (setf errbnd (dmax1 epsabs (* epsrel dres)))
    (if (and (<= abserr (* 100.0d0 epmach resabs)) (> abserr errbnd))
        (setf ier 2))
    (if (= nint 1) (go label80))
    (fdo (i 1 (+ i 1))
         ((> i npts) nil)
         (tagbody
           (setf jlow (+ i 1))
           (setf ind1 (fref iord (i) ((1 limit))))
           (fdo (j jlow (+ j 1))
                ((> j nint) nil)
                (tagbody
                  (setf ind2 (fref iord (j) ((1 limit))))
                  (if
                   (> (fref elist (ind1) ((1 limit)))
                      (fref elist (ind2) ((1 limit))))
                   (go label60))
                  (setf ind1 ind2)
                  (setf k j)
                 label60))
           (if (= ind1 (fref iord (i) ((1 limit)))) (go label70))
           (fset (fref iord (k) ((1 limit))) (fref iord (i) ((1 limit))))
           (fset (fref iord (i) ((1 limit))) ind1)
          label70))
    (if (< limit npts2) (setf ier 1))
   label80
    (if (or (/= ier 0) (<= abserr errbnd)) (go label210))
    (fset (fref rlist2 (1) ((1 52))) result)
    (setf maxerr (fref iord (1) ((1 limit))))
    (setf errmax (fref elist (maxerr) ((1 limit))))
    (setf area result)
    (setf nrmax 1)
    (setf nres 0)
    (setf numrl2 1)
    (setf ktmin 0)
    (setf extrap %false%)
    (setf noext %false%)
    (setf erlarg errsum)
    (setf ertest errbnd)
    (setf levmax 1)
    (setf iroff1 0)
    (setf iroff2 0)
    (setf iroff3 0)
    (setf ierro 0)
    (setf uflow (d1mach 1))
    (setf oflow (d1mach 2))
    (setf abserr oflow)
    (setf ksgn -1)
    (if (>= dres (* (- 1.0d0 (* 50.0d0 epmach)) resabs)) (setf ksgn 1))
    (fdo (last npts2 (+ last 1))
         ((> last limit) nil)
         (tagbody
           (setf levcur (+ (fref level (maxerr) ((1 limit))) 1))
           (setf a1 (fref alist (maxerr) ((1 limit))))
           (setf b1
                   (* 0.5d0
                      (+ (fref alist (maxerr) ((1 limit)))
                         (fref blist (maxerr) ((1 limit))))))
           (setf a2 b1)
           (setf b2 (fref blist (maxerr) ((1 limit))))
           (setf erlast errmax)
           (multiple-value-bind
               (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
               (dqk21 f a1 b1 area1 error1 resa defab1)
             (declare (ignore var-0))
             (when var-1 (setf a1 var-1))
             (when var-2 (setf b1 var-2))
             (when var-3 (setf area1 var-3))
             (when var-4 (setf error1 var-4))
             (when var-5 (setf resa var-5))
             (when var-6 (setf defab1 var-6)))
           (multiple-value-bind
               (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
               (dqk21 f a2 b2 area2 error2 resa defab2)
             (declare (ignore var-0))
             (when var-1 (setf a2 var-1))
             (when var-2 (setf b2 var-2))
             (when var-3 (setf area2 var-3))
             (when var-4 (setf error2 var-4))
             (when var-5 (setf resa var-5))
             (when var-6 (setf defab2 var-6)))
           (setf neval (+ neval 42))
           (setf area12 (+ area1 area2))
           (setf erro12 (+ error1 error2))
           (setf errsum (- (+ errsum erro12) errmax))
           (setf area (- (+ area area12) (fref rlist (maxerr) ((1 limit)))))
           (if (or (= defab1 error1) (= defab2 error2)) (go label95))
           (if
            (or
             (> (dabs (- (fref rlist (maxerr) ((1 limit))) area12))
                (* 1.0d-5 (dabs area12)))
             (< erro12 (* 0.99d0 errmax)))
            (go label90))
           (if extrap (setf iroff2 (+ iroff2 1)))
           (if (not extrap) (setf iroff1 (+ iroff1 1)))
          label90
           (if (and (> last 10) (> erro12 errmax)) (setf iroff3 (+ iroff3 1)))
          label95
           (fset (fref level (maxerr) ((1 limit))) levcur)
           (fset (fref level (last) ((1 limit))) levcur)
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
           (if (> error2 error1) (go label100))
           (fset (fref alist (last) ((1 limit))) a2)
           (fset (fref blist (maxerr) ((1 limit))) b1)
           (fset (fref blist (last) ((1 limit))) b2)
           (fset (fref elist (maxerr) ((1 limit))) error1)
           (fset (fref elist (last) ((1 limit))) error2)
           (go label110)
          label100
           (fset (fref alist (maxerr) ((1 limit))) a2)
           (fset (fref alist (last) ((1 limit))) a1)
           (fset (fref blist (last) ((1 limit))) b1)
           (fset (fref rlist (maxerr) ((1 limit))) area2)
           (fset (fref rlist (last) ((1 limit))) area1)
           (fset (fref elist (maxerr) ((1 limit))) error2)
           (fset (fref elist (last) ((1 limit))) error1)
          label110
           (multiple-value-bind
               (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
               (dqpsrt limit last maxerr errmax elist iord nrmax)
             (declare (ignore var-4 var-5))
             (when var-0 (setf limit var-0))
             (when var-1 (setf last var-1))
             (when var-2 (setf maxerr var-2))
             (when var-3 (setf errmax var-3))
             (when var-6 (setf nrmax var-6)))
           (if (<= errsum errbnd) (go label190))
           (if (/= ier 0) (go label170))
           (if noext (go label160))
           (setf erlarg (- erlarg erlast))
           (if (<= (+ levcur 1) levmax) (setf erlarg (+ erlarg erro12)))
           (if extrap (go label120))
           (if (<= (+ (fref level (maxerr) ((1 limit))) 1) levmax)
               (go label160))
           (setf extrap %true%)
           (setf nrmax 2)
          label120
           (if (or (= ierro 3) (<= erlarg ertest)) (go label140))
           (setf id nrmax)
           (setf jupbnd last)
           (if (> last (+ 2 (truncate limit 2)))
               (setf jupbnd (- (+ limit 3) last)))
           (fdo (k id (+ k 1))
                ((> k jupbnd) nil)
                (tagbody
                  (setf maxerr (fref iord (nrmax) ((1 limit))))
                  (setf errmax (fref elist (maxerr) ((1 limit))))
                  (if (<= (+ (fref level (maxerr) ((1 limit))) 1) levmax)
                      (go label160))
                  (setf nrmax (+ nrmax 1))
                 label130))
          label140
           (setf numrl2 (+ numrl2 1))
           (fset (fref rlist2 (numrl2) ((1 52))) area)
           (if (<= numrl2 2) (go label155))
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
           (if (>= abseps abserr) (go label150))
           (setf ktmin 0)
           (setf abserr abseps)
           (setf result reseps)
           (setf correc erlarg)
           (setf ertest (dmax1 epsabs (* epsrel (dabs reseps))))
           (if (< abserr ertest) (go label170))
          label150
           (if (= numrl2 1) (setf noext %true%))
           (if (>= ier 5) (go label170))
          label155
           (setf maxerr (fref iord (1) ((1 limit))))
           (setf errmax (fref elist (maxerr) ((1 limit))))
           (setf nrmax 1)
           (setf extrap %false%)
           (setf levmax (+ levmax 1))
           (setf erlarg errsum)
          label160))
   label170
    (if (= abserr oflow) (go label190))
    (if (= (+ ier ierro) 0) (go label180))
    (if (= ierro 3) (setf abserr (+ abserr correc)))
    (if (= ier 0) (setf ier 3))
    (if (and (/= result 0.0d0) (/= area 0.0d0)) (go label175))
    (if (> abserr errsum) (go label190))
    (if (= area 0.0d0) (go label210))
    (go label180)
   label175
    (if (> (/ abserr (dabs result)) (/ errsum (dabs area))) (go label190))
   label180
    (if
     (and (= ksgn -1)
          (<= (dmax1 (dabs result) (dabs area))
              (* resabs 0.010000000000000002d0)))
     (go label210))
    (if
     (or (> 0.010000000000000002d0 (/ result area))
         (> (/ result area) 100.0d0)
         (> errsum (dabs area)))
     (setf ier 6))
    (go label210)
   label190
    (setf result 0.0d0)
    (fdo (k 1 (+ k 1))
         ((> k last) nil)
         (tagbody
           (setf result (+ result (fref rlist (k) ((1 limit)))))
          label200))
    (setf abserr errsum)
   label210
    (if (> ier 2) (setf ier (- ier 1)))
    (setf result (* result sign))
   label999
    (go end_label)
   end_label
    (return
     (values f
             a
             b
             npts2
             points
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
             pts
             iord
             level
             ndin
             last))))

