;;; Compiled by f2cl version 2.0 beta on 2001/02/23 at 10:09:07
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type 'simple-array)
;;;           (:array-slicing t))

(in-package "QUADPACK")
(use-package :f2cl)

(defun dqelg (n epstab result abserr res3la nres)
  (declare (type (array double-float (*)) res3la)
           (type double-float abserr result)
           (type (array double-float (*)) epstab)
           (type integer4 nres n))
  (prog ((i 0) (ib 0) (ib2 0) (ie 0) (indx 0) (k1 0) (k2 0) (k3 0) (limexp 0)
         (newelm 0) (num 0) (tol3 0.0d0) (tol2 0.0d0) (tol1 0.0d0) (ss 0.0d0)
         (res 0.0d0) (oflow 0.0d0) (e3 0.0d0) (e2 0.0d0) (e1abs 0.0d0)
         (e1 0.0d0) (e0 0.0d0) (err3 0.0d0) (err2 0.0d0) (err1 0.0d0)
         (error 0.0d0) (epsinf 0.0d0) (epmach 0.0d0) (delta3 0.0d0)
         (delta2 0.0d0) (delta1 0.0d0))
    (declare
     (type double-float delta1 delta2 delta3 epmach epsinf error err1 err2 err3
      e0 e1 e1abs e2 e3 oflow res ss tol1 tol2 tol3)
     (type integer4 num newelm limexp k3 k2 k1 indx ie ib2 ib i))
    (declare
     (ftype (function (integer4) (values double-float &rest t)) d1mach))
    (declare (ftype (function (double-float) (values double-float)) dabs))
    (declare
     (ftype (function (double-float double-float) (values double-float))
      dmax1))
    (setf epmach (d1mach 4))
    (setf oflow (d1mach 2))
    (setf nres (+ nres 1))
    (setf abserr oflow)
    (setf result (fref epstab (n) ((1 52))))
    (if (< n 3) (go label100))
    (setf limexp 50)
    (fset (fref epstab ((+ n 2)) ((1 52))) (fref epstab (n) ((1 52))))
    (setf newelm (truncate (- n 1) 2))
    (fset (fref epstab (n) ((1 52))) oflow)
    (setf num n)
    (setf k1 n)
    (fdo (i 1 (+ i 1))
         ((> i newelm) nil)
         (tagbody
           (setf k2 (- k1 1))
           (setf k3 (- k1 2))
           (setf res (fref epstab ((+ k1 2)) ((1 52))))
           (setf e0 (fref epstab (k3) ((1 52))))
           (setf e1 (fref epstab (k2) ((1 52))))
           (setf e2 res)
           (setf e1abs (dabs e1))
           (setf delta2 (- e2 e1))
           (setf err2 (dabs delta2))
           (setf tol2 (* (dmax1 (dabs e2) e1abs) epmach))
           (setf delta3 (- e1 e0))
           (setf err3 (dabs delta3))
           (setf tol3 (* (dmax1 e1abs (dabs e0)) epmach))
           (if (or (> err2 tol2) (> err3 tol3)) (go label10))
           (setf result res)
           (setf abserr (+ err2 err3))
           (go label100)
          label10
           (setf e3 (fref epstab (k1) ((1 52))))
           (fset (fref epstab (k1) ((1 52))) e1)
           (setf delta1 (- e1 e3))
           (setf err1 (dabs delta1))
           (setf tol1 (* (dmax1 e1abs (dabs e3)) epmach))
           (if (or (<= err1 tol1) (<= err2 tol2) (<= err3 tol3)) (go label20))
           (setf ss (+ (/ 1.0d0 delta1) (/ 1.0d0 delta2) (/ -1.0d0 delta3)))
           (setf epsinf (dabs (* ss e1)))
           (if (> epsinf 1.0d-4) (go label30))
          label20
           (setf n (- (+ i i) 1))
           (go label50)
          label30
           (setf res (+ e1 (/ 1.0d0 ss)))
           (fset (fref epstab (k1) ((1 52))) res)
           (setf k1 (- k1 2))
           (setf error (+ err2 (dabs (- res e2)) err3))
           (if (> error abserr) (go label40))
           (setf abserr error)
           (setf result res)
          label40))
   label50
    (if (= n limexp) (setf n (- (* 2 (truncate limexp 2)) 1)))
    (setf ib 1)
    (if (= (* (truncate num 2) 2) num) (setf ib 2))
    (setf ie (+ newelm 1))
    (fdo (i 1 (+ i 1))
         ((> i ie) nil)
         (tagbody
           (setf ib2 (+ ib 2))
           (fset (fref epstab (ib) ((1 52))) (fref epstab (ib2) ((1 52))))
           (setf ib ib2)
          label60))
    (if (= num n) (go label80))
    (setf indx (+ (- num n) 1))
    (fdo (i 1 (+ i 1))
         ((> i n) nil)
         (tagbody
           (fset (fref epstab (i) ((1 52))) (fref epstab (indx) ((1 52))))
           (setf indx (+ indx 1))
          label70))
   label80
    (if (>= nres 4) (go label90))
    (fset (fref res3la (nres) ((1 3))) result)
    (setf abserr oflow)
    (go label100)
   label90
    (setf abserr
            (+ (dabs (- result (fref res3la (3) ((1 3)))))
               (dabs (- result (fref res3la (2) ((1 3)))))
               (dabs (- result (fref res3la (1) ((1 3)))))))
    (fset (fref res3la (1) ((1 3))) (fref res3la (2) ((1 3))))
    (fset (fref res3la (2) ((1 3))) (fref res3la (3) ((1 3))))
    (fset (fref res3la (3) ((1 3))) result)
   label100
    (setf abserr (dmax1 abserr (* 5.0d0 epmach (dabs result))))
    (go end_label)
   end_label
    (return (values n epstab result abserr res3la nres))))

