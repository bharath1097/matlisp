;;; Compiled by f2cl version 2.0 beta on 2001/02/23 at 10:10:19
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type 'simple-array)
;;;           (:array-slicing t))


(use-package :f2cl)

(defun dqawo
       (f a b omega integr epsabs epsrel result abserr neval ier leniw maxp1
        lenw last iwork work)
  (declare (type double-float a b omega epsabs epsrel result abserr)
           (type integer4 integr neval ier leniw maxp1 lenw last)
           (type (array integer4 (*)) iwork)
           (type (array double-float (*)) work))
  (prog ((limit 0) (lvl 0) (l1 0) (l2 0) (l3 0) (l4 0) (momcom 0))
    (declare (type integer4 momcom l4 l3 l2 l1 lvl limit))
    (declare
     (ftype
      (function
       (double-float double-float double-float double-float integer4
        double-float double-float integer4 integer4 integer4 double-float
        double-float integer4 integer4 integer4 array-double-float
        array-double-float array-double-float array-double-float array-integer4
        array-integer4 integer4 array-double-float)
       (values &rest t))
      dqawoe))
    (declare
     (ftype (function (string integer4 integer4 integer4) (values &rest t))
      xerror))
    (setf ier 6)
    (setf neval 0)
    (setf last 0)
    (setf result 0.0d0)
    (setf abserr 0.0d0)
    (if (or (< leniw 2) (< maxp1 1) (< lenw (+ (* leniw 2) (* maxp1 25))))
        (go label10))
    (setf limit (truncate leniw 2))
    (setf l1 (+ limit 1))
    (setf l2 (+ limit l1))
    (setf l3 (+ limit l2))
    (setf l4 (+ limit l3))
    (multiple-value-bind
        (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10
         var-11 var-12 var-13 var-14 var-15 var-16 var-17 var-18 var-19 var-20
         var-21 var-22)
        (dqawoe f a b omega integr epsabs epsrel limit 1 maxp1 result abserr
         neval ier last (array-slice work double-float (1) ((1 lenw)))
         (array-slice work double-float (l1) ((1 lenw)))
         (array-slice work double-float (l2) ((1 lenw)))
         (array-slice work double-float (l3) ((1 lenw)))
         (array-slice iwork integer4 (1) ((1 leniw)))
         (array-slice iwork integer4 (l1) ((1 leniw))) momcom
         (array-slice work double-float (l4) ((1 lenw))))
      (declare
       (ignore var-0 var-8 var-15 var-16 var-17 var-18 var-19 var-20 var-22))
      (when var-1 (setf a var-1))
      (when var-2 (setf b var-2))
      (when var-3 (setf omega var-3))
      (when var-4 (setf integr var-4))
      (when var-5 (setf epsabs var-5))
      (when var-6 (setf epsrel var-6))
      (when var-7 (setf limit var-7))
      (when var-9 (setf maxp1 var-9))
      (when var-10 (setf result var-10))
      (when var-11 (setf abserr var-11))
      (when var-12 (setf neval var-12))
      (when var-13 (setf ier var-13))
      (when var-14 (setf last var-14))
      (when var-21 (setf momcom var-21)))
    (setf lvl 0)
   label10
    (if (= ier 6) (setf lvl 0))
    (if (/= ier 0)
        (multiple-value-bind
            (var-0 var-1 var-2 var-3)
            (xerror "abnormal return from dqawo" 26 ier lvl)
          (declare (ignore var-0 var-1))
          (when var-2 (setf ier var-2))
          (when var-3 (setf lvl var-3))))
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
             result
             abserr
             neval
             ier
             leniw
             maxp1
             lenw
             last
             iwork
             work))))

