;;; Compiled by f2cl version 2.0 beta on 2001/02/23 at 10:10:03
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type 'array)
;;;           (:array-slicing nil))

(in-package "QUADPACK")
(use-package :f2cl)

(let ((p 0.9d0) (pi_ 3.141592653589793d0))
  (declare (type double-float pi_ p))
  (defun dqawfe
         (f a omega integr epsabs limlst limit maxp1 result abserr neval ier
          rslst erlst ierlst lst alist blist rlist elist iord nnlog chebmo)
    (declare (type double-float a omega epsabs result abserr)
             (type integer4 integr limlst limit maxp1 neval ier lst)
             (type (array integer4 (*)) ierlst iord nnlog)
             (type (array double-float (*)) rslst erlst alist blist rlist elist
              chebmo))
    (prog ((ktmin 0) (l 0) (last 0) (ll 0) (momcom 0) (nev 0) (nres 0)
           (numrl2 0) (uflow 0.0d0)
           (res3la (make-array 3 :element-type 'double-float)) (reseps 0.0d0)
           (psum (make-array 52 :element-type 'double-float)) (p1 0.0d0)
           (fact 0.0d0) (errsum 0.0d0) (epsa 0.0d0) (eps 0.0d0) (ep 0.0d0)
           (drl 0.0d0) (dla 0.0d0) (dl 0.0d0) (c2 0.0d0) (c1 0.0d0)
           (cycle 0.0d0) (correc 0.0d0) (abseps 0.0d0))
      (declare (type (array double-float (52)) psum)
               (type (array double-float (3)) res3la)
               (type double-float abseps correc cycle c1 c2 dl dla drl ep eps
                epsa errsum fact p1 reseps uflow)
               (type integer4 numrl2 nres nev momcom ll last l ktmin))
      (declare
       (ftype
        (function
         (double-float double-float integer4 double-float double-float integer4
          double-float double-float integer4 integer4 array-double-float
          array-double-float array-double-float array-double-float
          array-integer4 integer4)
         (values &rest t))
        dqagie))
      (declare
       (ftype
        (function (or array-double-float double-float) (values double-float))
        dabs))
      (declare
       (ftype (function (integer4) (values double-float &rest t)) d1mach))
      (declare
       (ftype
        (function
         (double-float double-float double-float double-float integer4
          double-float double-float integer4 integer4 integer4
          array-double-float array-double-float integer4 array-integer4
          integer4 array-double-float array-double-float array-double-float
          array-double-float array-integer4 array-integer4 integer4
          array-double-float)
         (values &rest t))
        dqawoe))
      (declare
       (ftype
        (function (double-float (or double-float array-double-float))
         (values double-float))
        dmax1))
      (declare
       (ftype
        (function
         (integer4 array-double-float double-float double-float
          array-double-float integer4)
         (values &rest t))
        dqelg))
      (setf result 0.0d0)
      (setf abserr 0.0d0)
      (setf neval 0)
      (setf lst 0)
      (setf ier 0)
      (if (or (and (/= integr 1) (/= integr 2)) (<= epsabs 0.0d0) (< limlst 3))
          (setf ier 6))
      (if (= ier 6) (go label999))
      (if (/= omega 0.0d0) (go label10))
      (if (= integr 1)
          (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
               var-10 var-11 var-12 var-13 var-14 var-15)
              (dqagie f 0.0d0 1 epsabs 0.0d0 limit result abserr neval ier
               alist blist rlist elist iord last)
            (declare
             (ignore var-0 var-1 var-2 var-4 var-10 var-11 var-12 var-13
              var-14))
            (when var-3 (setf epsabs var-3))
            (when var-5 (setf limit var-5))
            (when var-6 (setf result var-6))
            (when var-7 (setf abserr var-7))
            (when var-8 (setf neval var-8))
            (when var-9 (setf ier var-9))
            (when var-15 (setf last var-15))))
      (fset (fref rslst (1) ((1 limlst))) result)
      (fset (fref erlst (1) ((1 limlst))) abserr)
      (fset (fref ierlst (1) ((1 limlst))) ier)
      (setf lst 1)
      (go label999)
     label10
      (setf l (truncate (dabs omega)))
      (setf dl (coerce (+ (* 2 l) 1) 'double-float))
      (setf cycle (/ (* dl pi_) (dabs omega)))
      (setf ier 0)
      (setf ktmin 0)
      (setf neval 0)
      (setf numrl2 0)
      (setf nres 0)
      (setf c1 a)
      (setf c2 (+ cycle a))
      (setf p1 (- 1.0d0 p))
      (setf uflow (d1mach 1))
      (setf eps epsabs)
      (if (> epsabs (/ uflow p1)) (setf eps (* epsabs p1)))
      (setf ep eps)
      (setf fact 1.0d0)
      (setf correc 0.0d0)
      (setf abserr 0.0d0)
      (setf errsum 0.0d0)
      (fdo (lst 1 (+ lst 1))
           ((> lst limlst) nil)
           (tagbody
             (setf dla (coerce lst 'double-float))
             (setf epsa (* eps fact))
             (multiple-value-bind
                 (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                  var-10 var-11 var-12 var-13 var-14 var-15 var-16 var-17
                  var-18 var-19 var-20 var-21 var-22)
                 (dqawoe f c1 c2 omega integr epsa 0.0d0 limit lst maxp1
                  (fref rslst (lst) ((1 limlst)))
                  (fref erlst (lst) ((1 limlst))) nev
                  (fref ierlst (lst) ((1 limlst))) last alist blist rlist elist
                  iord nnlog momcom chebmo)
               (declare
                (ignore var-0 var-6 var-15 var-16 var-17 var-18 var-19 var-20
                 var-22))
               (when var-1 (setf c1 var-1))
               (when var-2 (setf c2 var-2))
               (when var-3 (setf omega var-3))
               (when var-4 (setf integr var-4))
               (when var-5 (setf epsa var-5))
               (when var-7 (setf limit var-7))
               (when var-8 (setf lst var-8))
               (when var-9 (setf maxp1 var-9))
               (when var-10 (fset (fref rslst (lst) ((1 limlst))) var-10))
               (when var-11 (fset (fref erlst (lst) ((1 limlst))) var-11))
               (when var-12 (setf nev var-12))
               (when var-13 (fset (fref ierlst (lst) ((1 limlst))) var-13))
               (when var-14 (setf last var-14))
               (when var-21 (setf momcom var-21)))
             (setf neval (+ neval nev))
             (setf fact (* fact p))
             (setf errsum (+ errsum (fref erlst (lst) ((1 limlst)))))
             (setf drl (* 50.0d0 (dabs (fref rslst (lst) ((1 limlst))))))
             (if (and (<= (+ errsum drl) epsabs) (>= lst 6)) (go label80))
             (setf correc (dmax1 correc (fref erlst (lst) ((1 limlst)))))
             (if (/= (fref ierlst (lst) ((1 limlst))) 0)
                 (setf eps (dmax1 ep (* correc p1))))
             (if (/= (fref ierlst (lst) ((1 limlst))) 0) (setf ier 7))
             (if
              (and (= ier 7) (<= (+ errsum drl) (* correc 10.0d0)) (> lst 5))
              (go label80))
             (setf numrl2 (+ numrl2 1))
             (if (> lst 1) (go label20))
             (fset (fref psum (1) ((1 52))) (fref rslst (1) ((1 limlst))))
             (go label40)
            label20
             (fset (fref psum (numrl2) ((1 52)))
                   (+ (fref psum (ll) ((1 52)))
                      (fref rslst (lst) ((1 limlst)))))
             (if (= lst 2) (go label40))
             (if (= lst limlst) (setf ier 1))
             (multiple-value-bind
                 (var-0 var-1 var-2 var-3 var-4 var-5)
                 (dqelg numrl2 psum reseps abseps res3la nres)
               (declare (ignore var-1 var-4))
               (when var-0 (setf numrl2 var-0))
               (when var-2 (setf reseps var-2))
               (when var-3 (setf abseps var-3))
               (when var-5 (setf nres var-5)))
             (setf ktmin (+ ktmin 1))
             (if (and (>= ktmin 15) (<= abserr (* 0.001d0 (+ errsum drl))))
                 (setf ier 4))
             (if (and (> abseps abserr) (/= lst 3)) (go label30))
             (setf abserr abseps)
             (setf result reseps)
             (setf ktmin 0)
             (if
              (or (<= (+ abserr (* 10.0d0 correc)) epsabs)
                  (and (<= abserr epsabs) (>= (* 10.0d0 correc) epsabs)))
              (go label60))
            label30
             (if (and (/= ier 0) (/= ier 7)) (go label60))
            label40
             (setf ll numrl2)
             (setf c1 c2)
             (setf c2 (+ c2 cycle))
            label50))
     label60
      (setf abserr (+ abserr (* 10.0d0 correc)))
      (if (= ier 0) (go label999))
      (if (and (/= result 0.0d0) (/= (fref psum (numrl2) ((1 52))) 0.0d0))
          (go label70))
      (if (> abserr errsum) (go label80))
      (if (= (fref psum (numrl2) ((1 52))) 0.0d0) (go label999))
     label70
      (if
       (> (/ abserr (dabs result))
          (/ (+ errsum drl) (dabs (fref psum (numrl2) ((1 52))))))
       (go label80))
      (if (and (>= ier 1) (/= ier 7)) (setf abserr (+ abserr drl)))
      (go label999)
     label80
      (setf result (fref psum (numrl2) ((1 52))))
      (setf abserr (+ errsum drl))
     label999
      (go end_label)
     end_label
      (return
       (values f
               a
               omega
               integr
               epsabs
               limlst
               limit
               maxp1
               result
               abserr
               neval
               ier
               rslst
               erlst
               ierlst
               lst
               alist
               blist
               rlist
               elist
               iord
               nnlog
               chebmo)))))

