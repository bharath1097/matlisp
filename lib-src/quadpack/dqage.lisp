;;; Compiled by f2cl version 2.0 beta on 2001/02/23 at 10:09:33
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type 'simple-array)
;;;           (:array-slicing t))


(use-package :f2cl)

(defun dqage
       (f a b epsabs epsrel key limit result abserr neval ier alist blist rlist
        elist iord last)
  (declare (type double-float a b epsabs epsrel result abserr)
           (type (array double-float (*)) alist blist rlist elist)
           (type (array integer4 (*)) iord)
           (type integer4 key limit neval ier last))
  (prog ((iroff1 0) (iroff2 0) (k 0) (keyf 0) (maxerr 0) (nrmax 0)
         (uflow 0.0d0) (resabs 0.0d0) (errsum 0.0d0) (erro12 0.0d0)
         (error2 0.0d0) (error1 0.0d0) (errmax 0.0d0) (errbnd 0.0d0)
         (epmach 0.0d0) (defab2 0.0d0) (defab1 0.0d0) (defabs 0.0d0) (b2 0.0d0)
         (b1 0.0d0) (a2 0.0d0) (a1 0.0d0) (area2 0.0d0) (area12 0.0d0)
         (area1 0.0d0) (area 0.0d0))
    (declare
     (type double-float area area1 area12 area2 a1 a2 b1 b2 defabs defab1
      defab2 epmach errbnd errmax error1 error2 erro12 errsum resabs uflow)
     (type integer4 nrmax maxerr keyf k iroff2 iroff1))
    (declare
     (ftype (function (integer4) (values double-float &rest t)) d1mach))
    (declare
     (ftype (function (double-float double-float) (values double-float))
      dmax1))
    (declare
     (ftype
      (function
       (double-float double-float double-float double-float double-float
        double-float double-float)
       (values &rest t))
      dqk15))
    (declare
     (ftype
      (function
       (double-float double-float double-float double-float double-float
        double-float double-float)
       (values &rest t))
      dqk21))
    (declare
     (ftype
      (function
       (double-float double-float double-float double-float double-float
        double-float double-float)
       (values &rest t))
      dqk31))
    (declare
     (ftype
      (function
       (double-float double-float double-float double-float double-float
        double-float double-float)
       (values &rest t))
      dqk41))
    (declare
     (ftype
      (function
       (double-float double-float double-float double-float double-float
        double-float double-float)
       (values &rest t))
      dqk51))
    (declare
     (ftype
      (function
       (double-float double-float double-float double-float double-float
        double-float double-float)
       (values &rest t))
      dqk61))
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
    (if (and (<= epsabs 0.0d0) (< epsrel (dmax1 (* 50.0d0 epmach) 5.0d-29)))
        (setf ier 6))
    (if (= ier 6) (go label999))
    (setf keyf key)
    (if (<= key 0) (setf keyf 1))
    (if (>= key 7) (setf keyf 6))
    (setf neval 0)
    (if (= keyf 1)
        (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
            (dqk15 f a b result abserr defabs resabs)
          (declare (ignore var-0))
          (when var-1 (setf a var-1))
          (when var-2 (setf b var-2))
          (when var-3 (setf result var-3))
          (when var-4 (setf abserr var-4))
          (when var-5 (setf defabs var-5))
          (when var-6 (setf resabs var-6))))
    (if (= keyf 2)
        (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
            (dqk21 f a b result abserr defabs resabs)
          (declare (ignore var-0))
          (when var-1 (setf a var-1))
          (when var-2 (setf b var-2))
          (when var-3 (setf result var-3))
          (when var-4 (setf abserr var-4))
          (when var-5 (setf defabs var-5))
          (when var-6 (setf resabs var-6))))
    (if (= keyf 3)
        (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
            (dqk31 f a b result abserr defabs resabs)
          (declare (ignore var-0))
          (when var-1 (setf a var-1))
          (when var-2 (setf b var-2))
          (when var-3 (setf result var-3))
          (when var-4 (setf abserr var-4))
          (when var-5 (setf defabs var-5))
          (when var-6 (setf resabs var-6))))
    (if (= keyf 4)
        (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
            (dqk41 f a b result abserr defabs resabs)
          (declare (ignore var-0))
          (when var-1 (setf a var-1))
          (when var-2 (setf b var-2))
          (when var-3 (setf result var-3))
          (when var-4 (setf abserr var-4))
          (when var-5 (setf defabs var-5))
          (when var-6 (setf resabs var-6))))
    (if (= keyf 5)
        (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
            (dqk51 f a b result abserr defabs resabs)
          (declare (ignore var-0))
          (when var-1 (setf a var-1))
          (when var-2 (setf b var-2))
          (when var-3 (setf result var-3))
          (when var-4 (setf abserr var-4))
          (when var-5 (setf defabs var-5))
          (when var-6 (setf resabs var-6))))
    (if (= keyf 6)
        (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
            (dqk61 f a b result abserr defabs resabs)
          (declare (ignore var-0))
          (when var-1 (setf a var-1))
          (when var-2 (setf b var-2))
          (when var-3 (setf result var-3))
          (when var-4 (setf abserr var-4))
          (when var-5 (setf defabs var-5))
          (when var-6 (setf resabs var-6))))
    (setf last 1)
    (fset (fref rlist (1) ((1 limit))) result)
    (fset (fref elist (1) ((1 limit))) abserr)
    (fset (fref iord (1) ((1 limit))) 1)
    (setf errbnd (dmax1 epsabs (* epsrel (dabs result))))
    (if (and (<= abserr (* 50.0d0 epmach defabs)) (> abserr errbnd))
        (setf ier 2))
    (if (= limit 1) (setf ier 1))
    (if
     (or (/= ier 0)
         (and (<= abserr errbnd) (/= abserr resabs))
         (= abserr 0.0d0))
     (go label60))
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
           (setf a2 b1)
           (setf b2 (fref blist (maxerr) ((1 limit))))
           (if (= keyf 1)
               (multiple-value-bind
                   (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
                   (dqk15 f a1 b1 area1 error1 resabs defab1)
                 (declare (ignore var-0))
                 (when var-1 (setf a1 var-1))
                 (when var-2 (setf b1 var-2))
                 (when var-3 (setf area1 var-3))
                 (when var-4 (setf error1 var-4))
                 (when var-5 (setf resabs var-5))
                 (when var-6 (setf defab1 var-6))))
           (if (= keyf 2)
               (multiple-value-bind
                   (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
                   (dqk21 f a1 b1 area1 error1 resabs defab1)
                 (declare (ignore var-0))
                 (when var-1 (setf a1 var-1))
                 (when var-2 (setf b1 var-2))
                 (when var-3 (setf area1 var-3))
                 (when var-4 (setf error1 var-4))
                 (when var-5 (setf resabs var-5))
                 (when var-6 (setf defab1 var-6))))
           (if (= keyf 3)
               (multiple-value-bind
                   (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
                   (dqk31 f a1 b1 area1 error1 resabs defab1)
                 (declare (ignore var-0))
                 (when var-1 (setf a1 var-1))
                 (when var-2 (setf b1 var-2))
                 (when var-3 (setf area1 var-3))
                 (when var-4 (setf error1 var-4))
                 (when var-5 (setf resabs var-5))
                 (when var-6 (setf defab1 var-6))))
           (if (= keyf 4)
               (multiple-value-bind
                   (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
                   (dqk41 f a1 b1 area1 error1 resabs defab1)
                 (declare (ignore var-0))
                 (when var-1 (setf a1 var-1))
                 (when var-2 (setf b1 var-2))
                 (when var-3 (setf area1 var-3))
                 (when var-4 (setf error1 var-4))
                 (when var-5 (setf resabs var-5))
                 (when var-6 (setf defab1 var-6))))
           (if (= keyf 5)
               (multiple-value-bind
                   (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
                   (dqk51 f a1 b1 area1 error1 resabs defab1)
                 (declare (ignore var-0))
                 (when var-1 (setf a1 var-1))
                 (when var-2 (setf b1 var-2))
                 (when var-3 (setf area1 var-3))
                 (when var-4 (setf error1 var-4))
                 (when var-5 (setf resabs var-5))
                 (when var-6 (setf defab1 var-6))))
           (if (= keyf 6)
               (multiple-value-bind
                   (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
                   (dqk61 f a1 b1 area1 error1 resabs defab1)
                 (declare (ignore var-0))
                 (when var-1 (setf a1 var-1))
                 (when var-2 (setf b1 var-2))
                 (when var-3 (setf area1 var-3))
                 (when var-4 (setf error1 var-4))
                 (when var-5 (setf resabs var-5))
                 (when var-6 (setf defab1 var-6))))
           (if (= keyf 1)
               (multiple-value-bind
                   (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
                   (dqk15 f a2 b2 area2 error2 resabs defab2)
                 (declare (ignore var-0))
                 (when var-1 (setf a2 var-1))
                 (when var-2 (setf b2 var-2))
                 (when var-3 (setf area2 var-3))
                 (when var-4 (setf error2 var-4))
                 (when var-5 (setf resabs var-5))
                 (when var-6 (setf defab2 var-6))))
           (if (= keyf 2)
               (multiple-value-bind
                   (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
                   (dqk21 f a2 b2 area2 error2 resabs defab2)
                 (declare (ignore var-0))
                 (when var-1 (setf a2 var-1))
                 (when var-2 (setf b2 var-2))
                 (when var-3 (setf area2 var-3))
                 (when var-4 (setf error2 var-4))
                 (when var-5 (setf resabs var-5))
                 (when var-6 (setf defab2 var-6))))
           (if (= keyf 3)
               (multiple-value-bind
                   (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
                   (dqk31 f a2 b2 area2 error2 resabs defab2)
                 (declare (ignore var-0))
                 (when var-1 (setf a2 var-1))
                 (when var-2 (setf b2 var-2))
                 (when var-3 (setf area2 var-3))
                 (when var-4 (setf error2 var-4))
                 (when var-5 (setf resabs var-5))
                 (when var-6 (setf defab2 var-6))))
           (if (= keyf 4)
               (multiple-value-bind
                   (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
                   (dqk41 f a2 b2 area2 error2 resabs defab2)
                 (declare (ignore var-0))
                 (when var-1 (setf a2 var-1))
                 (when var-2 (setf b2 var-2))
                 (when var-3 (setf area2 var-3))
                 (when var-4 (setf error2 var-4))
                 (when var-5 (setf resabs var-5))
                 (when var-6 (setf defab2 var-6))))
           (if (= keyf 5)
               (multiple-value-bind
                   (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
                   (dqk51 f a2 b2 area2 error2 resabs defab2)
                 (declare (ignore var-0))
                 (when var-1 (setf a2 var-1))
                 (when var-2 (setf b2 var-2))
                 (when var-3 (setf area2 var-3))
                 (when var-4 (setf error2 var-4))
                 (when var-5 (setf resabs var-5))
                 (when var-6 (setf defab2 var-6))))
           (if (= keyf 6)
               (multiple-value-bind
                   (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
                   (dqk61 f a2 b2 area2 error2 resabs defab2)
                 (declare (ignore var-0))
                 (when var-1 (setf a2 var-1))
                 (when var-2 (setf b2 var-2))
                 (when var-3 (setf area2 var-3))
                 (when var-4 (setf error2 var-4))
                 (when var-5 (setf resabs var-5))
                 (when var-6 (setf defab2 var-6))))
           (setf neval (+ neval 1))
           (setf area12 (+ area1 area2))
           (setf erro12 (+ error1 error2))
           (setf errsum (- (+ errsum erro12) errmax))
           (setf area (- (+ area area12) (fref rlist (maxerr) ((1 limit)))))
           (if (or (= defab1 error1) (= defab2 error2)) (go label5))
           (if
            (and
             (<= (dabs (- (fref rlist (maxerr) ((1 limit))) area12))
                 (* 1.0d-5 (dabs area12)))
             (>= erro12 (* 0.99d0 errmax)))
            (setf iroff1 (+ iroff1 1)))
           (if (and (> last 10) (> erro12 errmax)) (setf iroff2 (+ iroff2 1)))
          label5
           (fset (fref rlist (maxerr) ((1 limit))) area1)
           (fset (fref rlist (last) ((1 limit))) area2)
           (setf errbnd (dmax1 epsabs (* epsrel (dabs area))))
           (if (<= errsum errbnd) (go label8))
           (if (or (>= iroff1 6) (>= iroff2 20)) (setf ier 2))
           (if (= last limit) (setf ier 1))
           (if
            (<= (dmax1 (dabs a1) (dabs b2))
                (* (+ 1.0d0 (* 100.0d0 epmach))
                   (+ (dabs a2) (* 1000.0d0 uflow))))
            (setf ier 3))
          label8
           (if (> error2 error1) (go label10))
           (fset (fref alist (last) ((1 limit))) a2)
           (fset (fref blist (maxerr) ((1 limit))) b1)
           (fset (fref blist (last) ((1 limit))) b2)
           (fset (fref elist (maxerr) ((1 limit))) error1)
           (fset (fref elist (last) ((1 limit))) error2)
           (go label20)
          label10
           (fset (fref alist (maxerr) ((1 limit))) a2)
           (fset (fref alist (last) ((1 limit))) a1)
           (fset (fref blist (last) ((1 limit))) b1)
           (fset (fref rlist (maxerr) ((1 limit))) area2)
           (fset (fref rlist (last) ((1 limit))) area1)
           (fset (fref elist (maxerr) ((1 limit))) error2)
           (fset (fref elist (last) ((1 limit))) error1)
          label20
           (multiple-value-bind
               (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
               (dqpsrt limit last maxerr errmax elist iord nrmax)
             (declare (ignore var-4 var-5))
             (when var-0 (setf limit var-0))
             (when var-1 (setf last var-1))
             (when var-2 (setf maxerr var-2))
             (when var-3 (setf errmax var-3))
             (when var-6 (setf nrmax var-6)))
           (if (or (/= ier 0) (<= errsum errbnd)) (go label40))
          label30))
   label40
    (setf result 0.0d0)
    (fdo (k 1 (+ k 1))
         ((> k last) nil)
         (tagbody
           (setf result (+ result (fref rlist (k) ((1 limit)))))
          label50))
    (setf abserr errsum)
   label60
    (if (/= keyf 1) (setf neval (* (+ (* 10 keyf) 1) (+ (* 2 neval) 1))))
    (if (= keyf 1) (setf neval (+ (* 30 neval) 15)))
   label999
    (go end_label)
   end_label
    (return
     (values f
             a
             b
             epsabs
             epsrel
             key
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

