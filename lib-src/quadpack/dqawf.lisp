;;; Compiled by f2cl version 2.0 beta on 2001/02/23 at 10:10:18
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type 'simple-array)
;;;           (:array-slicing t))

(in-package "QUADPACK")
(use-package :f2cl)

(defun dqawf
       (f a omega integr epsabs result abserr neval ier limlst lst leniw maxp1
        lenw iwork work)
  (declare (type double-float a omega epsabs result abserr)
           (type integer4 integr neval ier limlst lst leniw maxp1 lenw)
           (type (array integer4 (*)) iwork)
           (type (array double-float (*)) work))
  (prog ((last 0) (limit 0) (ll2 0) (lvl 0) (l1 0) (l2 0) (l3 0) (l4 0) (l5 0)
         (l6 0))
    (declare (type integer4 l6 l5 l4 l3 l2 l1 lvl ll2 limit last))
    (declare
     (ftype
      (function
       (double-float double-float double-float integer4 double-float integer4
        integer4 integer4 double-float double-float integer4 integer4
        array-double-float array-double-float array-integer4 integer4
        array-double-float array-double-float array-double-float
        array-double-float array-integer4 array-integer4 array-double-float)
       (values &rest t))
      dqawfe))
    (declare
     (ftype (function (string integer4 integer4 integer4) (values &rest t))
      xerror))
    (setf ier 6)
    (setf neval 0)
    (setf last 0)
    (setf result 0.0d0)
    (setf abserr 0.0d0)
    (if
     (or (< limlst 3)
         (< leniw (+ limlst 2))
         (< maxp1 1)
         (< lenw (+ (* leniw 2) (* maxp1 25))))
     (go label10))
    (setf limit (truncate (- leniw limlst) 2))
    (setf l1 (+ limlst 1))
    (setf l2 (+ limlst l1))
    (setf l3 (+ limit l2))
    (setf l4 (+ limit l3))
    (setf l5 (+ limit l4))
    (setf l6 (+ limit l5))
    (setf ll2 (+ limit l1))
    (multiple-value-bind
        (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10
         var-11 var-12 var-13 var-14 var-15 var-16 var-17 var-18 var-19 var-20
         var-21 var-22)
        (dqawfe f a omega integr epsabs limlst limit maxp1 result abserr neval
         ier (array-slice work double-float (1) ((1 lenw)))
         (array-slice work double-float (l1) ((1 lenw)))
         (array-slice iwork integer4 (1) ((1 leniw))) lst
         (array-slice work double-float (l2) ((1 lenw)))
         (array-slice work double-float (l3) ((1 lenw)))
         (array-slice work double-float (l4) ((1 lenw)))
         (array-slice work double-float (l5) ((1 lenw)))
         (array-slice iwork integer4 (l1) ((1 leniw)))
         (array-slice iwork integer4 (ll2) ((1 leniw)))
         (array-slice work double-float (l6) ((1 lenw))))
      (declare
       (ignore var-0 var-12 var-13 var-14 var-16 var-17 var-18 var-19 var-20
        var-21 var-22))
      (when var-1 (setf a var-1))
      (when var-2 (setf omega var-2))
      (when var-3 (setf integr var-3))
      (when var-4 (setf epsabs var-4))
      (when var-5 (setf limlst var-5))
      (when var-6 (setf limit var-6))
      (when var-7 (setf maxp1 var-7))
      (when var-8 (setf result var-8))
      (when var-9 (setf abserr var-9))
      (when var-10 (setf neval var-10))
      (when var-11 (setf ier var-11))
      (when var-15 (setf lst var-15)))
    (setf lvl 0)
   label10
    (if (= ier 6) (setf lvl 1))
    (if (/= ier 0)
        (multiple-value-bind
            (var-0 var-1 var-2 var-3)
            (xerror "abnormal return from dqawf" 26 ier lvl)
          (declare (ignore var-0 var-1))
          (when var-2 (setf ier var-2))
          (when var-3 (setf lvl var-3))))
    (go end_label)
   end_label
    (return
     (values f
             a
             omega
             integr
             epsabs
             result
             abserr
             neval
             ier
             limlst
             lst
             leniw
             maxp1
             lenw
             iwork
             work))))

