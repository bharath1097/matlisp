;;; Compiled by f2cl version 2.0 beta on 2001/02/23 at 10:10:05
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type 'simple-array)
;;;           (:array-slicing t))

(in-package "QUADPACK")
(use-package :f2cl)

(defun dqawse
       (f a b alfa beta integr epsabs epsrel limit result abserr neval ier
        alist blist rlist elist iord last)
  (declare (type double-float a b alfa beta epsabs epsrel result abserr)
           (type (array double-float (*)) alist blist rlist elist)
           (type (array integer4 (*)) iord)
           (type integer4 integr limit neval ier last))
  (prog ((iroff1 0) (iroff2 0) (k 0) (maxerr 0) (nev 0) (nrmax 0) (uflow 0.0d0)
         (rj (make-array 25 :element-type 'double-float))
         (ri (make-array 25 :element-type 'double-float))
         (rh (make-array 25 :element-type 'double-float))
         (rg (make-array 25 :element-type 'double-float)) (resas2 0.0d0)
         (resas1 0.0d0) (errsum 0.0d0) (error2 0.0d0) (erro12 0.0d0)
         (error1 0.0d0) (errmax 0.0d0) (errbnd 0.0d0) (epmach 0.0d0)
         (centre 0.0d0) (b2 0.0d0) (b1 0.0d0) (a2 0.0d0) (a1 0.0d0)
         (area2 0.0d0) (area12 0.0d0) (area1 0.0d0) (area 0.0d0))
    (declare (type (array double-float (25)) ri rj rh rg)
             (type double-float area area1 area12 area2 a1 a2 b1 b2 centre
              epmach errbnd errmax error1 erro12 error2 errsum resas1 resas2
              uflow)
             (type integer4 nrmax nev maxerr k iroff2 iroff1))
    (declare
     (ftype (function (integer4) (values double-float &rest t)) d1mach))
    (declare
     (ftype (function (double-float double-float) (values double-float))
      dmax1))
    (declare
     (ftype
      (function
       (double-float double-float array-double-float array-double-float
        array-double-float array-double-float integer4)
       (values &rest t))
      dqmomo))
    (declare
     (ftype
      (function
       (double-float double-float double-float double-float double-float
        double-float double-float array-double-float array-double-float
        array-double-float array-double-float double-float double-float
        double-float integer4 integer4)
       (values &rest t))
      dqc25s))
    (declare (ftype (function (double-float) (values double-float)) dabs))
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
    (fset (fref rlist (1) ((1 limit))) 0.0d0)
    (fset (fref elist (1) ((1 limit))) 0.0d0)
    (fset (fref iord (1) ((1 limit))) 0)
    (setf result 0.0d0)
    (setf abserr 0.0d0)
    (if
     (or (<= b a)
         (and (= epsabs 0.0d0) (< epsrel (dmax1 (* 50.0d0 epmach) 5.0d-29)))
         (<= alfa -1.0d0)
         (<= beta -1.0d0)
         (< integr 1)
         (> integr 4)
         (< limit 2))
     (go label999))
    (setf ier 0)
    (multiple-value-bind
        (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
        (dqmomo alfa beta ri rj rg rh integr)
      (declare (ignore var-2 var-3 var-4 var-5))
      (when var-0 (setf alfa var-0))
      (when var-1 (setf beta var-1))
      (when var-6 (setf integr var-6)))
    (setf centre (* 0.5d0 (+ b a)))
    (multiple-value-bind
        (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10
         var-11 var-12 var-13 var-14 var-15)
        (dqc25s f a b a centre alfa beta ri rj rg rh area1 error1 resas1 integr
         nev)
      (declare (ignore var-0 var-7 var-8 var-9 var-10))
      (when var-1 (setf a var-1))
      (when var-2 (setf b var-2))
      (when var-3 (setf a var-3))
      (when var-4 (setf centre var-4))
      (when var-5 (setf alfa var-5))
      (when var-6 (setf beta var-6))
      (when var-11 (setf area1 var-11))
      (when var-12 (setf error1 var-12))
      (when var-13 (setf resas1 var-13))
      (when var-14 (setf integr var-14))
      (when var-15 (setf nev var-15)))
    (setf neval nev)
    (multiple-value-bind
        (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10
         var-11 var-12 var-13 var-14 var-15)
        (dqc25s f a b centre b alfa beta ri rj rg rh area2 error2 resas2 integr
         nev)
      (declare (ignore var-0 var-7 var-8 var-9 var-10))
      (when var-1 (setf a var-1))
      (when var-2 (setf b var-2))
      (when var-3 (setf centre var-3))
      (when var-4 (setf b var-4))
      (when var-5 (setf alfa var-5))
      (when var-6 (setf beta var-6))
      (when var-11 (setf area2 var-11))
      (when var-12 (setf error2 var-12))
      (when var-13 (setf resas2 var-13))
      (when var-14 (setf integr var-14))
      (when var-15 (setf nev var-15)))
    (setf last 2)
    (setf neval (+ neval nev))
    (setf result (+ area1 area2))
    (setf abserr (+ error1 error2))
    (setf errbnd (dmax1 epsabs (* epsrel (dabs result))))
    (if (> error2 error1) (go label10))
    (fset (fref alist (1) ((1 limit))) a)
    (fset (fref alist (2) ((1 limit))) centre)
    (fset (fref blist (1) ((1 limit))) centre)
    (fset (fref blist (2) ((1 limit))) b)
    (fset (fref rlist (1) ((1 limit))) area1)
    (fset (fref rlist (2) ((1 limit))) area2)
    (fset (fref elist (1) ((1 limit))) error1)
    (fset (fref elist (2) ((1 limit))) error2)
    (go label20)
   label10
    (fset (fref alist (1) ((1 limit))) centre)
    (fset (fref alist (2) ((1 limit))) a)
    (fset (fref blist (1) ((1 limit))) b)
    (fset (fref blist (2) ((1 limit))) centre)
    (fset (fref rlist (1) ((1 limit))) area2)
    (fset (fref rlist (2) ((1 limit))) area1)
    (fset (fref elist (1) ((1 limit))) error2)
    (fset (fref elist (2) ((1 limit))) error1)
   label20
    (fset (fref iord (1) ((1 limit))) 1)
    (fset (fref iord (2) ((1 limit))) 2)
    (if (= limit 2) (setf ier 1))
    (if (or (<= abserr errbnd) (= ier 1)) (go label999))
    (setf errmax (fref elist (1) ((1 limit))))
    (setf maxerr 1)
    (setf nrmax 1)
    (setf area result)
    (setf errsum abserr)
    (setf iroff1 0)
    (setf iroff2 0)
    (fdo (last 3 (+ last 1))
         ((> last limit) nil)
         (tagbody
           (setf a1 (fref alist (maxerr) ((1 limit))))
           (setf b1
                   (* 0.5d0
                      (+ (fref alist (maxerr) ((1 limit)))
                         (fref blist (maxerr) ((1 limit))))))
           (setf a2 b1)
           (setf b2 (fref blist (maxerr) ((1 limit))))
           (multiple-value-bind
               (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                var-10 var-11 var-12 var-13 var-14 var-15)
               (dqc25s f a b a1 b1 alfa beta ri rj rg rh area1 error1 resas1
                integr nev)
             (declare (ignore var-0 var-7 var-8 var-9 var-10))
             (when var-1 (setf a var-1))
             (when var-2 (setf b var-2))
             (when var-3 (setf a1 var-3))
             (when var-4 (setf b1 var-4))
             (when var-5 (setf alfa var-5))
             (when var-6 (setf beta var-6))
             (when var-11 (setf area1 var-11))
             (when var-12 (setf error1 var-12))
             (when var-13 (setf resas1 var-13))
             (when var-14 (setf integr var-14))
             (when var-15 (setf nev var-15)))
           (setf neval (+ neval nev))
           (multiple-value-bind
               (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                var-10 var-11 var-12 var-13 var-14 var-15)
               (dqc25s f a b a2 b2 alfa beta ri rj rg rh area2 error2 resas2
                integr nev)
             (declare (ignore var-0 var-7 var-8 var-9 var-10))
             (when var-1 (setf a var-1))
             (when var-2 (setf b var-2))
             (when var-3 (setf a2 var-3))
             (when var-4 (setf b2 var-4))
             (when var-5 (setf alfa var-5))
             (when var-6 (setf beta var-6))
             (when var-11 (setf area2 var-11))
             (when var-12 (setf error2 var-12))
             (when var-13 (setf resas2 var-13))
             (when var-14 (setf integr var-14))
             (when var-15 (setf nev var-15)))
           (setf neval (+ neval nev))
           (setf area12 (+ area1 area2))
           (setf erro12 (+ error1 error2))
           (setf errsum (- (+ errsum erro12) errmax))
           (setf area (- (+ area area12) (fref rlist (maxerr) ((1 limit)))))
           (if (or (= a a1) (= b b2)) (go label30))
           (if (or (= resas1 error1) (= resas2 error2)) (go label30))
           (if
            (and
             (< (dabs (- (fref rlist (maxerr) ((1 limit))) area12))
                (* 1.0d-5 (dabs area12)))
             (>= erro12 (* 0.99d0 errmax)))
            (setf iroff1 (+ iroff1 1)))
           (if (and (> last 10) (> erro12 errmax)) (setf iroff2 (+ iroff2 1)))
          label30
           (fset (fref rlist (maxerr) ((1 limit))) area1)
           (fset (fref rlist (last) ((1 limit))) area2)
           (setf errbnd (dmax1 epsabs (* epsrel (dabs area))))
           (if (<= errsum errbnd) (go label35))
           (if (= last limit) (setf ier 1))
           (if (or (>= iroff1 6) (>= iroff2 20)) (setf ier 2))
           (if
            (<= (dmax1 (dabs a1) (dabs b2))
                (* (+ 1.0d0 (* 100.0d0 epmach))
                   (+ (dabs a2) (* 1000.0d0 uflow))))
            (setf ier 3))
          label35
           (if (> error2 error1) (go label40))
           (fset (fref alist (last) ((1 limit))) a2)
           (fset (fref blist (maxerr) ((1 limit))) b1)
           (fset (fref blist (last) ((1 limit))) b2)
           (fset (fref elist (maxerr) ((1 limit))) error1)
           (fset (fref elist (last) ((1 limit))) error2)
           (go label50)
          label40
           (fset (fref alist (maxerr) ((1 limit))) a2)
           (fset (fref alist (last) ((1 limit))) a1)
           (fset (fref blist (last) ((1 limit))) b1)
           (fset (fref rlist (maxerr) ((1 limit))) area2)
           (fset (fref rlist (last) ((1 limit))) area1)
           (fset (fref elist (maxerr) ((1 limit))) error2)
           (fset (fref elist (last) ((1 limit))) error1)
          label50
           (multiple-value-bind
               (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
               (dqpsrt limit last maxerr errmax elist iord nrmax)
             (declare (ignore var-4 var-5))
             (when var-0 (setf limit var-0))
             (when var-1 (setf last var-1))
             (when var-2 (setf maxerr var-2))
             (when var-3 (setf errmax var-3))
             (when var-6 (setf nrmax var-6)))
           (if (or (/= ier 0) (<= errsum errbnd)) (go label70))
          label60))
   label70
    (setf result 0.0d0)
    (fdo (k 1 (+ k 1))
         ((> k last) nil)
         (tagbody
           (setf result (+ result (fref rlist (k) ((1 limit)))))
          label80))
    (setf abserr errsum)
   label999
    (go end_label)
   end_label
    (return
     (values f
             a
             b
             alfa
             beta
             integr
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

