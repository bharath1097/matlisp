;;; Compiled by f2cl version 2.0 beta on 2001/02/23 at 10:08:41
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type 'simple-array)
;;;           (:array-slicing t))


(use-package :f2cl)

(defun xerror (messg nmessg nerr level)
  (declare (type integer4 level nerr nmessg)
           (type (simple-array base-char (*)) messg))
  (prog ()
    (declare)
    (declare
     (ftype
      (function
       (string integer4
               integer4
               integer4
               integer4
               integer4
               integer4
               integer4
               single-float
               single-float)
       (values &rest t))
      xerrwv))
    (multiple-value-bind
        (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
        (xerrwv messg nmessg nerr level 0 0 0 0 0.0 0.0)
      (declare (ignore var-4 var-5 var-6 var-7 var-8 var-9))
      (when var-0 (setf messg var-0))
      (when var-1 (setf nmessg var-1))
      (when var-2 (setf nerr var-2))
      (when var-3 (setf level var-3)))
    (go end_label)
   end_label
    (return (values messg nmessg nerr level))))

;;; Compiled by f2cl version 2.0 beta on 2001/02/23 at 10:08:41
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type 'simple-array)
;;;           (:array-slicing t))


(use-package :f2cl)

(defun xerrwv (messg nmessg nerr level ni i1 i2 nr r1 r2)
  (declare (type single-float r2 r1)
           (type integer4 nr i2 i1 ni level nerr nmessg)
           (type (simple-array base-char (*)) messg))
  (prog ((form
          (make-array '(37) :element-type 'base-char :initial-element #\Space))
         (lfirst
          (make-array '(20) :element-type 'base-char :initial-element #\Space))
         (lun (make-array 5 :element-type 'integer4)) (ifatal 0) (i 0)
         (iunit 0) (kunit 0) (isizef 0) (isizei 0) (nunit 0) (mkntrl 0)
         (llevel 0) (lerr 0) (lmessg 0) (kount 0) (junk 0) (kdummy 0)
         (maxmes 0) (lkntrl 0))
    (declare
     (type integer4 lkntrl maxmes kdummy junk kount lmessg lerr llevel mkntrl
      nunit isizei isizef kunit iunit i ifatal)
     (type (array integer4 (5)) lun)
     (type (simple-array base-char (20)) lfirst)
     (type (simple-array base-char (37)) form))
    (declare
     (ftype (function (integer4 integer4 logical) (values integer4 &rest t))
      j4save))
    (declare (ftype (function (string integer4) (values &rest t)) xerprt))
    (declare (ftype (function nil (values &rest t)) fdump))
    (declare
     (ftype
      (function (string integer4 integer4 integer4 integer4) (values &rest t))
      xersav))
    (declare (ftype (function (string integer4) (values &rest t)) xerabt))
    (declare
     (ftype
      (function (string integer4 integer4 integer4 integer4) (values &rest t))
      xerctl))
    (declare (ftype (function (integer4 integer4) (values integer4)) min0))
    (declare (ftype (function (integer4 integer4) (values integer4)) max0))
    (declare (ftype (function (integer4) (values integer4)) iabs))
    (declare
     (ftype (function (array-integer4 integer4) (values &rest t)) xgetua))
    (declare (ftype (function (integer4) (values integer4 &rest t)) i1mach))
    (declare (ftype (function (integer4) (values single-float)) float))
    (declare (ftype (function (single-float) (values integer4)) log10))
    (declare (ftype (function (integer4 integer4) (values integer4)) min))
    (setf lkntrl (j4save 2 0 %false%))
    (setf maxmes (j4save 4 0 %false%))
    (if (and (> nmessg 0) (/= nerr 0) (>= level -1) (<= level 2)) (go label10))
    (if (> lkntrl 0) (xerprt "FATAL ERROR IN..." 17))
    (xerprt "XERROR -- INVALID INPUT" 23)
    (if (> lkntrl 0) (fdump))
    (if (> lkntrl 0) (xerprt "JOB ABORT DUE TO FATAL ERROR." 29))
    (if (> lkntrl 0)
        (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4)
            (xersav " " 0 0 0 kdummy)
          (declare (ignore var-0 var-1 var-2 var-3))
          (when var-4 (setf kdummy var-4))))
    (xerabt "XERROR -- INVALID INPUT" 23)
    (go end_label)
   label10
    (setf junk
            (multiple-value-bind
                (ret-val var-0 var-1 var-2)
                (j4save 1 nerr %true%)
              (declare (ignore var-0 var-2))
              (when var-1 (setf nerr var-1))
              ret-val))
    (multiple-value-bind
        (var-0 var-1 var-2 var-3 var-4)
        (xersav messg nmessg nerr level kount)
      (declare (ignore))
      (when var-0 (setf messg var-0))
      (when var-1 (setf nmessg var-1))
      (when var-2 (setf nerr var-2))
      (when var-3 (setf level var-3))
      (when var-4 (setf kount var-4)))
    (f2cl-set-string lfirst messg (string 20))
    (setf lmessg nmessg)
    (setf lerr nerr)
    (setf llevel level)
    (multiple-value-bind
        (var-0 var-1 var-2 var-3 var-4)
        (xerctl lfirst lmessg lerr llevel lkntrl)
      (declare (ignore))
      (when var-0 (setf lfirst var-0))
      (when var-1 (setf lmessg var-1))
      (when var-2 (setf lerr var-2))
      (when var-3 (setf llevel var-3))
      (when var-4 (setf lkntrl var-4)))
    (setf lmessg nmessg)
    (setf lerr nerr)
    (setf llevel level)
    (setf lkntrl (max0 -2 (min0 2 lkntrl)))
    (setf mkntrl (iabs lkntrl))
    (if (and (< llevel 2) (= lkntrl 0)) (go label100))
    (if
     (or (and (= llevel -1) (> kount (min0 1 maxmes)))
         (and (= llevel 0) (> kount maxmes))
         (and (= llevel 1) (> kount maxmes) (= mkntrl 1))
         (and (= llevel 2) (> kount (max0 1 maxmes))))
     (go label100))
    (if (<= lkntrl 0) (go label20))
    (xerprt " " 1)
    (if (= llevel -1)
        (xerprt "WARNING MESSAGE...THIS MESSAGE WILL ONLY BE PRINTED ONCE."
         57))
    (if (= llevel 0) (xerprt "WARNING IN..." 13))
    (if (= llevel 1) (xerprt "RECOVERABLE ERROR IN..." 23))
    (if (= llevel 2) (xerprt "FATAL ERROR IN..." 17))
   label20
    (multiple-value-bind
        (var-0 var-1)
        (xerprt messg lmessg)
      (declare (ignore))
      (when var-0 (setf messg var-0))
      (when var-1 (setf lmessg var-1)))
    (multiple-value-bind
        (var-0 var-1)
        (xgetua lun nunit)
      (declare (ignore var-0))
      (when var-1 (setf nunit var-1)))
    (setf isizei (truncate (+ (log10 (ffloat (i1mach 9))) 1.0)))
    (setf isizef
            (truncate (+ (log10 (expt (ffloat (i1mach 10)) (i1mach 11))) 1.0)))
    (fdo (kunit 1 (+ kunit 1))
         ((> kunit nunit) nil)
         (tagbody
           (setf iunit (fref lun (kunit) ((1 5))))
           (if (= iunit 0) (setf iunit (i1mach 4)))
           (fdo (i 1 (+ i 1))
                ((> i (min ni 2)) nil)
                (tagbody
                  (fformat form
                           ("(11X,21HIN ABOVE MESSAGE, I" 1 (("~1D")) "=,I" 1
                            (("~2D")) ")   " "~%")
                           i
                           isizei)
                  (if (= i 1) (fformat iunit (("~A~%")) i1))
                  (if (= i 2) (fformat iunit (("~A~%")) i2))
                 label22))
           (fdo (i 1 (+ i 1))
                ((> i (min nr 2)) nil)
                (tagbody
                  (fformat form
                           ("(11X,21HIN ABOVE MESSAGE, R" 1 (("~1D")) "=,E" 1
                            (("~2D")) "." 1 (("~2D")) ")" "~%")
                           i
                           (+ isizef 10)
                           isizef)
                  (if (= i 1) (fformat iunit (("~A~%")) r1))
                  (if (= i 2) (fformat iunit (("~A~%")) r2))
                 label24))
           (if (<= lkntrl 0) (go label40))
           (fformat iunit (" ERROR NUMBER =" 1 (("~10D")) "~%") lerr)
          label40
          label50))
    (if (> lkntrl 0) (fdump))
   label100
    (setf ifatal 0)
    (if (or (= llevel 2) (and (= llevel 1) (= mkntrl 2))) (setf ifatal 1))
    (if (<= ifatal 0) (go end_label))
    (if (or (<= lkntrl 0) (> kount (max0 1 maxmes))) (go label120))
    (if (= llevel 1) (xerprt "JOB ABORT DUE TO UNRECOVERED ERROR." 35))
    (if (= llevel 2) (xerprt "JOB ABORT DUE TO FATAL ERROR." 29))
    (multiple-value-bind
        (var-0 var-1 var-2 var-3 var-4)
        (xersav " " -1 0 0 kdummy)
      (declare (ignore var-0 var-1 var-2 var-3))
      (when var-4 (setf kdummy var-4)))
   label120
    (if (and (= llevel 2) (> kount (max0 1 maxmes))) (setf lmessg 0))
    (multiple-value-bind
        (var-0 var-1)
        (xerabt messg lmessg)
      (declare (ignore))
      (when var-0 (setf messg var-0))
      (when var-1 (setf lmessg var-1)))
    (go end_label)
   end_label
    (return (values messg nmessg nerr level ni i1 i2 nr r1 r2))))

;;; Compiled by f2cl version 2.0 beta on 2001/02/23 at 10:08:41
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type 'simple-array)
;;;           (:array-slicing t))


(use-package :f2cl)

(let ((mestab (f2cl-init-string (10) (20)))
      (nertab (make-array 10 :element-type 'integer4))
      (levtab (make-array 10 :element-type 'integer4))
      (kount (make-array 10 :element-type 'integer4))
      (kountx 0))
  (declare (type integer4 kountx)
           (type (array integer4 (10)) kount levtab nertab)
           (type (array (simple-array base-char (20)) (10)) mestab))
  (fset (fref kount (1) ((1 10))) 0)
  (fset (fref kount (2) ((1 10))) 0)
  (fset (fref kount (3) ((1 10))) 0)
  (fset (fref kount (4) ((1 10))) 0)
  (fset (fref kount (5) ((1 10))) 0)
  (fset (fref kount (6) ((1 10))) 0)
  (fset (fref kount (7) ((1 10))) 0)
  (fset (fref kount (8) ((1 10))) 0)
  (fset (fref kount (9) ((1 10))) 0)
  (fset (fref kount (10) ((1 10))) 0)
  (setq kountx 0)
  (defun xersav (messg nmessg nerr level icount)
    (declare (type integer4 icount level nerr nmessg)
             (type (simple-array base-char (*)) messg))
    (prog ((mes
            (make-array '(20)
                        :element-type
                        'base-char
                        :initial-element
                        #\Space))
           (lun (make-array 5 :element-type 'integer4)) (ii 0) (i 0) (iunit 0)
           (kunit 0) (nunit 0))
      (declare (type integer4 nunit kunit iunit i ii)
               (type (array integer4 (5)) lun)
               (type (simple-array base-char (20)) mes))
      (declare
       (ftype (function (array-integer4 integer4) (values &rest t)) xgetua))
      (declare (ftype (function (integer4) (values integer4 &rest t)) i1mach))
      (if (> nmessg 0) (go label80))
      (if (= (fref kount (1) ((1 10))) 0) (go end_label))
      (multiple-value-bind
          (var-0 var-1)
          (xgetua lun nunit)
        (declare (ignore var-0))
        (when var-1 (setf nunit var-1)))
      (fdo (kunit 1 (+ kunit 1))
           ((> kunit nunit) nil)
           (tagbody
             (setf iunit (fref lun (kunit) ((1 5))))
             (if (= iunit 0) (setf iunit (i1mach 4)))
             (fformat iunit
                      ("0          ERROR MESSAGE SUMMARY" "~%"
                       " MESSAGE START             NERR     LEVEL     COUNT"
                       "~%")
                      nil)
             (fdo (i 1 (+ i 1))
                  ((> i 10) nil)
                  (tagbody
                    (if (= (fref kount (i) ((1 10))) 0) (go label30))
                    (fformat iunit
                             ("~1@T" ("~A") 3 (("~10D")) "~%")
                             (fref mestab (i) ((1 10)))
                             (fref nertab (i) ((1 10)))
                             (fref levtab (i) ((1 10)))
                             (fref kount (i) ((1 10))))
                   label20))
            label30
             (if (/= kountx 0)
                 (fformat iunit
                          ("0OTHER ERRORS NOT INDIVIDUALLY TABULATED=" 1
                           (("~10D")) "~%")
                          kountx))
             (fformat iunit ("~1@T" "~%") nil)
            label60))
      (if (< nmessg 0) (go end_label))
      (fdo (i 1 (+ i 1))
           ((> i 10) nil)
           (tagbody label70 (fset (fref kount (i) ((1 10))) 0)))
      (setf kountx 0)
      (go end_label)
     label80
      (f2cl-set-string mes messg (string 20))
      (fdo (i 1 (+ i 1))
           ((> i 10) nil)
           (tagbody
             (setf ii i)
             (if (= (fref kount (i) ((1 10))) 0) (go label110))
             (if (fstring-/= mes (fref mestab (i) ((1 10)))) (go label90))
             (if (/= nerr (fref nertab (i) ((1 10)))) (go label90))
             (if (/= level (fref levtab (i) ((1 10)))) (go label90))
             (go label100)
            label90))
      (setf kountx (+ kountx 1))
      (setf icount 1)
      (go end_label)
     label100
      (fset (fref kount (ii) ((1 10))) (+ (fref kount (ii) ((1 10))) 1))
      (setf icount (fref kount (ii) ((1 10))))
      (go end_label)
     label110
      (f2cl-set-string (fref mestab (ii) ((1 10))) mes (string 20))
      (fset (fref nertab (ii) ((1 10))) nerr)
      (fset (fref levtab (ii) ((1 10))) level)
      (fset (fref kount (ii) ((1 10))) 1)
      (setf icount 1)
      (go end_label)
     end_label
      (return (values messg nmessg nerr level icount)))))

;;; Compiled by f2cl version 2.0 beta on 2001/02/23 at 10:08:41
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type 'simple-array)
;;;           (:array-slicing t))


(use-package :f2cl)

(defun xgetua (iunita n)
  (declare (type integer4 n) (type (array integer4 (*)) iunita))
  (prog ((index 0) (i 0))
    (declare (type integer4 i index))
    (declare
     (ftype (function (integer4 integer4 logical) (values integer4 &rest t))
      j4save))
    (setf n (j4save 5 0 %false%))
    (fdo (i 1 (+ i 1))
         ((> i n) nil)
         (tagbody
           (setf index (+ i 4))
           (if (= i 1) (setf index 3))
           (fset (fref iunita (i) ((1 5))) (j4save index 0 %false%))
          label30))
    (go end_label)
   end_label
    (return (values iunita n))))

;;; Compiled by f2cl version 2.0 beta on 2001/02/23 at 10:08:41
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type 'simple-array)
;;;           (:array-slicing t))


(use-package :f2cl)

(defun fdump () (prog () (declare) (go end_label) end_label (return (values))))

;;; Compiled by f2cl version 2.0 beta on 2001/02/23 at 10:08:41
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type 'simple-array)
;;;           (:array-slicing t))


(use-package :f2cl)

(let ((iparam (make-array 9 :element-type 'integer4)))
  (declare (type (array integer4 (9)) iparam))
  (fset (fref iparam (1) ((1 9))) 0)
  (fset (fref iparam (2) ((1 9))) 2)
  (fset (fref iparam (3) ((1 9))) 0)
  (fset (fref iparam (4) ((1 9))) 10)
  (fset (fref iparam (5) ((1 9))) 1)
  (fset (fref iparam (6) ((1 9))) 0)
  (fset (fref iparam (7) ((1 9))) 0)
  (fset (fref iparam (8) ((1 9))) 0)
  (fset (fref iparam (9) ((1 9))) 0)
  (defun j4save (iwhich ivalue iset)
    (declare (type logical iset) (type integer4 ivalue iwhich))
    (prog ((j4save 0))
      (declare (type integer4 j4save))
      (setf j4save (fref iparam (iwhich) ((1 9))))
      (if iset (fset (fref iparam (iwhich) ((1 9))) ivalue))
      (go end_label)
     end_label
      (return (values j4save iwhich ivalue iset)))))

;;; Compiled by f2cl version 2.0 beta on 2001/02/23 at 10:08:41
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type 'simple-array)
;;;           (:array-slicing t))


(use-package :f2cl)

(defun xerabt (messg nmessg)
  (declare (type integer4 nmessg) (type (simple-array base-char (*)) messg))
  (prog () (declare) end_label (return (values messg nmessg))))

;;; Compiled by f2cl version 2.0 beta on 2001/02/23 at 10:08:41
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type 'simple-array)
;;;           (:array-slicing t))


(use-package :f2cl)

(defun xerctl (messg1 nmessg nerr level kontrl)
  (declare (type integer4 kontrl level nerr nmessg)
           (type (simple-array base-char (*)) messg1))
  (prog ()
    (declare)
    (go end_label)
   end_label
    (return (values messg1 nmessg nerr level kontrl))))

;;; Compiled by f2cl version 2.0 beta on 2001/02/23 at 10:08:41
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type 'simple-array)
;;;           (:array-slicing t))


(use-package :f2cl)

(defun xerprt (messg nmessg)
  (declare (type integer4 nmessg) (type (simple-array base-char (*)) messg))
  (prog ((lun (make-array 5 :element-type 'integer4)) (last 0) (ichar 0)
         (iunit 0) (kunit 0) (lenmes 0) (nunit 0))
    (declare (type integer4 nunit lenmes kunit iunit ichar last)
             (type (array integer4 (5)) lun))
    (declare
     (ftype (function (array-integer4 integer4) (values &rest t)) xgetua))
    (declare (ftype (function (string) (values integer4)) len))
    (declare (ftype (function (integer4) (values integer4 &rest t)) i1mach))
    (declare (ftype (function (integer4 integer4) (values integer4)) min0))
    (multiple-value-bind
        (var-0 var-1)
        (xgetua lun nunit)
      (declare (ignore var-0))
      (when var-1 (setf nunit var-1)))
    (setf lenmes (len messg))
    (fdo (kunit 1 (+ kunit 1))
         ((> kunit nunit) nil)
         (tagbody
           (setf iunit (fref lun (kunit) ((1 5))))
           (if (= iunit 0) (setf iunit (i1mach 4)))
           (fdo (ichar 1 (+ ichar 72))
                ((> ichar lenmes) nil)
                (tagbody
                  (setf last (min0 (+ ichar 71) lenmes))
                  (fformat iunit (("~A~%")) (fref-string messg (ichar last)))
                 label10))
          label20))
    (go end_label)
   end_label
    (return (values messg nmessg))))

