;;; Compiled by f2cl version 2.0 beta on 2002/01/06 at 19:01:15
;;; 
;;; Options: ((:PRUNE-LABELS NIL) (:AUTO-SAVE T) (:RELAXED-ARRAY-DECLS T)
;;;           (:COERCE-ASSIGNS :AS-NEEDED) (:ARRAY-TYPE 'SIMPLE-ARRAY)
;;;           (:ARRAY-SLICING T))


(use-package :f2cl)

(defstruct (refnum (:predicate is-refnum-p))
  (nprob 0 :type integer4)
  (nfev 0 :type integer4)
  (njev 0 :type integer4))


(let* ()
  (defparameter *refnum-common-block* (make-refnum)))



(let ((nread 5) (nwrite 6) (one 1.0d0) (ten 10.0d0))
  (declare (type double-float ten one) (type integer4 nwrite nread))
  (defun *main* ()
    (symbol-macrolet ((njev (refnum-njev *refnum-common-block*))
                      (nfev (refnum-nfev *refnum-common-block*))
                      (nprob (refnum-nprob *refnum-common-block*)))
      (prog ((ntries 0) (n 0) (m 0) (lwa 0) (k 0) (info 0) (ic 0) (i 0)
             (nx (make-array 60 :element-type 'integer4))
             (np (make-array 60 :element-type 'integer4))
             (nj (make-array 60 :element-type 'integer4))
             (nf (make-array 60 :element-type 'integer4))
             (na (make-array 60 :element-type 'integer4))
             (ma (make-array 60 :element-type 'integer4))
             (iwa (make-array 40 :element-type 'integer4)) (tol 0.0d0)
             (fnorm2 0.0d0) (fnorm1 0.0d0) (factor 0.0d0)
             (x (make-array 40 :element-type 'double-float))
             (wa (make-array 2865 :element-type 'double-float))
             (fvec (make-array 65 :element-type 'double-float))
             (fnm (make-array 60 :element-type 'double-float)))
        (declare (type (array double-float (60)) fnm)
                 (type (array double-float (65)) fvec)
                 (type (array double-float (2865)) wa)
                 (type (array double-float (40)) x)
                 (type double-float factor fnorm1 fnorm2 tol)
                 (type (array integer4 (40)) iwa)
                 (type (array integer4 (60)) ma na nf nj np nx)
                 (type integer4 i ic info k lwa m n ntries))
        (declare
         (ftype (function (integer4) (values double-float &rest t)) dpmpar))
        (declare (ftype (function (double-float) (values double-float)) dsqrt))
        (declare
         (ftype
          (function (integer4 array-double-float integer4 double-float)
           (values &rest t))
          initpt))
        (declare
         (ftype
          (function
           (integer4 integer4 array-double-float array-double-float integer4)
           (values &rest t))
          ssqfcn))
        (declare
         (ftype
          (function (integer4 array-double-float)
           (values double-float &rest t))
          enorm))
        (declare
         (ftype
          (function
           (single-float integer4 integer4 array-double-float
            array-double-float double-float integer4 array-integer4
            array-double-float integer4)
           (values &rest t))
          lmdif1))
        (setf tol (dsqrt (dpmpar 1)))
        (setf lwa 2865)
        (setf ic 0)
       label10
        (fortran_comment
         "***WARNING:  READ statement may not be translated correctly!")
        (setf nprob (read))
        (setf n (read))
        (setf m (read))
        (setf ntries (read))
        (fortran_comment
         "***WARNING: Preceding READ statements may not be correct!")
        (if (<= nprob 0) (go label30))
        (setf factor one)
        (fdo (k 1 (+ k 1))
             ((> k ntries) nil)
             (tagbody
               (setf ic (+ ic 1))
               (multiple-value-bind
                   (var-0 var-1 var-2 var-3)
                   (initpt n x nprob factor)
                 (declare (ignore var-1))
                 (when var-0 (setf n var-0))
                 (when var-2 (setf nprob var-2))
                 (when var-3 (setf factor var-3)))
               (multiple-value-bind
                   (var-0 var-1 var-2 var-3 var-4)
                   (ssqfcn m n x fvec nprob)
                 (declare (ignore var-2 var-3))
                 (when var-0 (setf m var-0))
                 (when var-1 (setf n var-1))
                 (when var-4 (setf nprob var-4)))
               (setf fnorm1
                       (coerce
                        (multiple-value-bind
                            (ret-val var-0 var-1)
                            (enorm m fvec)
                          (declare (ignore var-1))
                          (when var-0 (setf m var-0))
                          ret-val)
                        'double-float))
               (fformat nwrite
                        ("~%" "~%" "~%" "~%" "~5@T" " PROBLEM" 1 (("~5D"))
                         "~5@T" " DIMENSIONS" 2 (("~5D")) "~5@T" "~%" "~%"
                         "~%")
                        nprob
                        n
                        m)
               (setf nfev 0)
               (setf njev 0)
               (multiple-value-bind
                   (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                    var-9)
                   (lmdif1 #'fcn m n x fvec tol info iwa wa lwa)
                 (declare (ignore var-0 var-3 var-4 var-7 var-8))
                 (when var-1 (setf m var-1))
                 (when var-2 (setf n var-2))
                 (when var-5 (setf tol var-5))
                 (when var-6 (setf info var-6))
                 (when var-9 (setf lwa var-9)))
               (multiple-value-bind
                   (var-0 var-1 var-2 var-3 var-4)
                   (ssqfcn m n x fvec nprob)
                 (declare (ignore var-2 var-3))
                 (when var-0 (setf m var-0))
                 (when var-1 (setf n var-1))
                 (when var-4 (setf nprob var-4)))
               (setf fnorm2
                       (coerce
                        (multiple-value-bind
                            (ret-val var-0 var-1)
                            (enorm m fvec)
                          (declare (ignore var-1))
                          (when var-0 (setf m var-0))
                          ret-val)
                        'double-float))
               (fset (fref np (ic) ((1 60))) nprob)
               (fset (fref na (ic) ((1 60))) n)
               (fset (fref ma (ic) ((1 60))) m)
               (fset (fref nf (ic) ((1 60))) nfev)
               (setf njev (truncate njev n))
               (fset (fref nj (ic) ((1 60))) njev)
               (fset (fref nx (ic) ((1 60))) info)
               (fset (fref fnm (ic) ((1 60))) fnorm2)
               (fformat nwrite
                        ("~5@T" " INITIAL L2 NORM OF THE RESIDUALS" 1
                         (("~15,7,2,0,'*,,'DE")) "~%" "~%" "~5@T"
                         " FINAL L2 NORM OF THE RESIDUALS  " 1
                         (("~15,7,2,0,'*,,'DE")) "~%" "~%" "~5@T"
                         " NUMBER OF FUNCTION EVALUATIONS  " 1 (("~10D")) "~%"
                         "~%" "~5@T" " NUMBER OF JACOBIAN EVALUATIONS  " 1
                         (("~10D")) "~%" "~%" "~5@T" " EXIT PARAMETER" "~18@T"
                         1 (("~10D")) "~%" "~%" "~5@T"
                         " FINAL APPROXIMATE SOLUTION" "~%" "~%" t
                         ("~5@T" 5 (("~15,7,2,0,'*,,'DE"))) "~%")
                        fnorm1
                        fnorm2
                        nfev
                        njev
                        info
                        (do ((i 1 (+ i 1))
                             (ret nil
                                  (append ret (list (fref x (i) ((1 40)))))))
                            ((> i n) ret)))
               (setf factor (* ten factor))
              label20))
        (go label10)
       label30
        (fformat nwrite
                 ("1SUMMARY OF " 1 (("~3D")) " CALLS TO LMDIF1" "~%" "~%")
                 ic)
        (fformat nwrite
                 (" NPROB   N    M   NFEV  NJEV  INFO  FINAL L2 NORM" "~%"
                  "~%")
                 nil)
        (fdo (i 1 (+ i 1))
             ((> i ic) nil)
             (tagbody
               (fformat nwrite
                        (3 (("~5D")) 3 (("~6D")) "~1@T" 1
                         (("~15,7,2,0,'*,,'DE")) "~%")
                        (fref np (i) ((1 60)))
                        (fref na (i) ((1 60)))
                        (fref ma (i) ((1 60)))
                        (fref nf (i) ((1 60)))
                        (fref nj (i) ((1 60)))
                        (fref nx (i) ((1 60)))
                        (fref fnm (i) ((1 60))))
              label40))
       end_label
        (return nil)))))

;;; Compiled by f2cl version 2.0 beta on 2002/01/06 at 19:01:15
;;; 
;;; Options: ((:PRUNE-LABELS NIL) (:AUTO-SAVE T) (:RELAXED-ARRAY-DECLS T)
;;;           (:COERCE-ASSIGNS :AS-NEEDED) (:ARRAY-TYPE 'SIMPLE-ARRAY)
;;;           (:ARRAY-SLICING T))


(use-package :f2cl)


(defun fcn (m n x fvec iflag)
  (declare (type (array double-float (*)) fvec x) (type integer4 iflag n m))
  (symbol-macrolet ((njev (refnum-njev *refnum-common-block*))
                    (nfev (refnum-nfev *refnum-common-block*))
                    (nprob (refnum-nprob *refnum-common-block*)))
    (prog ()
      (declare)
      (declare
       (ftype
        (function
         (integer4 integer4 array-double-float array-double-float integer4)
         (values &rest t))
        ssqfcn))
      (multiple-value-bind
          (var-0 var-1 var-2 var-3 var-4)
          (ssqfcn m n x fvec nprob)
        (declare (ignore var-2 var-3))
        (when var-0 (setf m var-0))
        (when var-1 (setf n var-1))
        (when var-4 (setf nprob var-4)))
      (if (= iflag 1) (setf nfev (+ nfev 1)))
      (if (= iflag 2) (setf njev (+ njev 1)))
      (go end_label)
     end_label
      (return (values m n x fvec iflag)))))

;;; Compiled by f2cl version 2.0 beta on 2002/01/06 at 19:01:16
;;; 
;;; Options: ((:PRUNE-LABELS NIL) (:AUTO-SAVE T) (:RELAXED-ARRAY-DECLS T)
;;;           (:COERCE-ASSIGNS :AS-NEEDED) (:ARRAY-TYPE 'SIMPLE-ARRAY)
;;;           (:ARRAY-SLICING T))


(use-package :f2cl)

(let ((zero 0.0d0)
      (zp25 0.25d0)
      (zp5 0.5d0)
      (one 1.0d0)
      (two 2.0d0)
      (five 5.0d0)
      (eight 8.0d0)
      (ten 10.0d0)
      (c13 13.0d0)
      (c14 14.0d0)
      (c29 29.0d0)
      (c45 45.0d0)
      (v (make-array 11 :element-type 'double-float))
      (y1 (make-array 15 :element-type 'double-float))
      (y2 (make-array 11 :element-type 'double-float))
      (y3 (make-array 16 :element-type 'double-float))
      (y4 (make-array 33 :element-type 'double-float))
      (y5 (make-array 65 :element-type 'double-float)))
  (declare (type (array double-float (65)) y5)
           (type (array double-float (33)) y4)
           (type (array double-float (16)) y3)
           (type (array double-float (15)) y1)
           (type (array double-float (11)) y2 v)
           (type double-float c45 c29 c14 c13 ten eight five two one zp5 zp25
            zero))
  (fset (fref v (1) ((1 11))) 4.0d0)
  (fset (fref v (2) ((1 11))) 2.0d0)
  (fset (fref v (3) ((1 11))) 1.0d0)
  (fset (fref v (4) ((1 11))) 0.5d0)
  (fset (fref v (5) ((1 11))) 0.25d0)
  (fset (fref v (6) ((1 11))) 0.167d0)
  (fset (fref v (7) ((1 11))) 0.125d0)
  (fset (fref v (8) ((1 11))) 0.1d0)
  (fset (fref v (9) ((1 11))) 0.0833d0)
  (fset (fref v (10) ((1 11))) 0.0714d0)
  (fset (fref v (11) ((1 11))) 0.0625d0)
  (fset (fref y1 (1) ((1 15))) 0.13999999999999999d0)
  (fset (fref y1 (2) ((1 15))) 0.18000000000000002d0)
  (fset (fref y1 (3) ((1 15))) 0.22000000000000003d0)
  (fset (fref y1 (4) ((1 15))) 0.25d0)
  (fset (fref y1 (5) ((1 15))) 0.29d0)
  (fset (fref y1 (6) ((1 15))) 0.32000000000000006d0)
  (fset (fref y1 (7) ((1 15))) 0.35000000000000003d0)
  (fset (fref y1 (8) ((1 15))) 0.39d0)
  (fset (fref y1 (9) ((1 15))) 0.37000000000000005d0)
  (fset (fref y1 (10) ((1 15))) 0.58d0)
  (fset (fref y1 (11) ((1 15))) 0.73d0)
  (fset (fref y1 (12) ((1 15))) 0.96d0)
  (fset (fref y1 (13) ((1 15))) 1.34d0)
  (fset (fref y1 (14) ((1 15))) 2.1d0)
  (fset (fref y1 (15) ((1 15))) 4.39d0)
  (fset (fref y2 (1) ((1 11))) 0.1957d0)
  (fset (fref y2 (2) ((1 11))) 0.1947d0)
  (fset (fref y2 (3) ((1 11))) 0.17350000000000002d0)
  (fset (fref y2 (4) ((1 11))) 0.16000000000000003d0)
  (fset (fref y2 (5) ((1 11))) 0.0844d0)
  (fset (fref y2 (6) ((1 11))) 0.06269999999999999d0)
  (fset (fref y2 (7) ((1 11))) 0.045599999999999995d0)
  (fset (fref y2 (8) ((1 11))) 0.0342d0)
  (fset (fref y2 (9) ((1 11))) 0.0323d0)
  (fset (fref y2 (10) ((1 11))) 0.0235d0)
  (fset (fref y2 (11) ((1 11))) 0.0246d0)
  (fset (fref y3 (1) ((1 16))) 34780.0d0)
  (fset (fref y3 (2) ((1 16))) 28610.000000000004d0)
  (fset (fref y3 (3) ((1 16))) 23650.000000000004d0)
  (fset (fref y3 (4) ((1 16))) 19630.0d0)
  (fset (fref y3 (5) ((1 16))) 16370.0d0)
  (fset (fref y3 (6) ((1 16))) 13720.000000000002d0)
  (fset (fref y3 (7) ((1 16))) 11540.0d0)
  (fset (fref y3 (8) ((1 16))) 9744.0d0)
  (fset (fref y3 (9) ((1 16))) 8261.0d0)
  (fset (fref y3 (10) ((1 16))) 7030.0d0)
  (fset (fref y3 (11) ((1 16))) 6005.0d0)
  (fset (fref y3 (12) ((1 16))) 5147.0d0)
  (fset (fref y3 (13) ((1 16))) 4427.0d0)
  (fset (fref y3 (14) ((1 16))) 3820.0d0)
  (fset (fref y3 (15) ((1 16))) 3307.0d0)
  (fset (fref y3 (16) ((1 16))) 2872.0d0)
  (fset (fref y4 (1) ((1 33))) 0.844d0)
  (fset (fref y4 (2) ((1 33))) 0.908d0)
  (fset (fref y4 (3) ((1 33))) 0.932d0)
  (fset (fref y4 (4) ((1 33))) 0.9359999999999999d0)
  (fset (fref y4 (5) ((1 33))) 0.925d0)
  (fset (fref y4 (6) ((1 33))) 0.908d0)
  (fset (fref y4 (7) ((1 33))) 0.8810000000000001d0)
  (fset (fref y4 (8) ((1 33))) 0.8500000000000001d0)
  (fset (fref y4 (9) ((1 33))) 0.8180000000000001d0)
  (fset (fref y4 (10) ((1 33))) 0.784d0)
  (fset (fref y4 (11) ((1 33))) 0.751d0)
  (fset (fref y4 (12) ((1 33))) 0.718d0)
  (fset (fref y4 (13) ((1 33))) 0.685d0)
  (fset (fref y4 (14) ((1 33))) 0.658d0)
  (fset (fref y4 (15) ((1 33))) 0.6280000000000001d0)
  (fset (fref y4 (16) ((1 33))) 0.6030000000000001d0)
  (fset (fref y4 (17) ((1 33))) 0.58d0)
  (fset (fref y4 (18) ((1 33))) 0.558d0)
  (fset (fref y4 (19) ((1 33))) 0.538d0)
  (fset (fref y4 (20) ((1 33))) 0.522d0)
  (fset (fref y4 (21) ((1 33))) 0.506d0)
  (fset (fref y4 (22) ((1 33))) 0.49000000000000005d0)
  (fset (fref y4 (23) ((1 33))) 0.47800000000000004d0)
  (fset (fref y4 (24) ((1 33))) 0.467d0)
  (fset (fref y4 (25) ((1 33))) 0.4570000000000001d0)
  (fset (fref y4 (26) ((1 33))) 0.44800000000000006d0)
  (fset (fref y4 (27) ((1 33))) 0.438d0)
  (fset (fref y4 (28) ((1 33))) 0.431d0)
  (fset (fref y4 (29) ((1 33))) 0.42400000000000004d0)
  (fset (fref y4 (30) ((1 33))) 0.42000000000000004d0)
  (fset (fref y4 (31) ((1 33))) 0.414d0)
  (fset (fref y4 (32) ((1 33))) 0.41100000000000003d0)
  (fset (fref y4 (33) ((1 33))) 0.40599999999999997d0)
  (fset (fref y5 (1) ((1 65))) 1.366d0)
  (fset (fref y5 (2) ((1 65))) 1.191d0)
  (fset (fref y5 (3) ((1 65))) 1.112d0)
  (fset (fref y5 (4) ((1 65))) 1.013d0)
  (fset (fref y5 (5) ((1 65))) 0.9910000000000001d0)
  (fset (fref y5 (6) ((1 65))) 0.885d0)
  (fset (fref y5 (7) ((1 65))) 0.8310000000000001d0)
  (fset (fref y5 (8) ((1 65))) 0.8470000000000001d0)
  (fset (fref y5 (9) ((1 65))) 0.786d0)
  (fset (fref y5 (10) ((1 65))) 0.7250000000000001d0)
  (fset (fref y5 (11) ((1 65))) 0.746d0)
  (fset (fref y5 (12) ((1 65))) 0.679d0)
  (fset (fref y5 (13) ((1 65))) 0.6080000000000001d0)
  (fset (fref y5 (14) ((1 65))) 0.655d0)
  (fset (fref y5 (15) ((1 65))) 0.6160000000000001d0)
  (fset (fref y5 (16) ((1 65))) 0.606d0)
  (fset (fref y5 (17) ((1 65))) 0.602d0)
  (fset (fref y5 (18) ((1 65))) 0.626d0)
  (fset (fref y5 (19) ((1 65))) 0.651d0)
  (fset (fref y5 (20) ((1 65))) 0.7240000000000001d0)
  (fset (fref y5 (21) ((1 65))) 0.649d0)
  (fset (fref y5 (22) ((1 65))) 0.649d0)
  (fset (fref y5 (23) ((1 65))) 0.6940000000000001d0)
  (fset (fref y5 (24) ((1 65))) 0.6440000000000001d0)
  (fset (fref y5 (25) ((1 65))) 0.6240000000000001d0)
  (fset (fref y5 (26) ((1 65))) 0.661d0)
  (fset (fref y5 (27) ((1 65))) 0.6120000000000001d0)
  (fset (fref y5 (28) ((1 65))) 0.558d0)
  (fset (fref y5 (29) ((1 65))) 0.533d0)
  (fset (fref y5 (30) ((1 65))) 0.49500000000000005d0)
  (fset (fref y5 (31) ((1 65))) 0.5d0)
  (fset (fref y5 (32) ((1 65))) 0.42300000000000004d0)
  (fset (fref y5 (33) ((1 65))) 0.395d0)
  (fset (fref y5 (34) ((1 65))) 0.375d0)
  (fset (fref y5 (35) ((1 65))) 0.37200000000000005d0)
  (fset (fref y5 (36) ((1 65))) 0.391d0)
  (fset (fref y5 (37) ((1 65))) 0.396d0)
  (fset (fref y5 (38) ((1 65))) 0.405d0)
  (fset (fref y5 (39) ((1 65))) 0.42800000000000005d0)
  (fset (fref y5 (40) ((1 65))) 0.42900000000000005d0)
  (fset (fref y5 (41) ((1 65))) 0.523d0)
  (fset (fref y5 (42) ((1 65))) 0.562d0)
  (fset (fref y5 (43) ((1 65))) 0.6070000000000001d0)
  (fset (fref y5 (44) ((1 65))) 0.653d0)
  (fset (fref y5 (45) ((1 65))) 0.672d0)
  (fset (fref y5 (46) ((1 65))) 0.7080000000000001d0)
  (fset (fref y5 (47) ((1 65))) 0.633d0)
  (fset (fref y5 (48) ((1 65))) 0.668d0)
  (fset (fref y5 (49) ((1 65))) 0.645d0)
  (fset (fref y5 (50) ((1 65))) 0.6320000000000001d0)
  (fset (fref y5 (51) ((1 65))) 0.5910000000000001d0)
  (fset (fref y5 (52) ((1 65))) 0.559d0)
  (fset (fref y5 (53) ((1 65))) 0.597d0)
  (fset (fref y5 (54) ((1 65))) 0.625d0)
  (fset (fref y5 (55) ((1 65))) 0.739d0)
  (fset (fref y5 (56) ((1 65))) 0.71d0)
  (fset (fref y5 (57) ((1 65))) 0.7290000000000001d0)
  (fset (fref y5 (58) ((1 65))) 0.7200000000000001d0)
  (fset (fref y5 (59) ((1 65))) 0.6360000000000001d0)
  (fset (fref y5 (60) ((1 65))) 0.581d0)
  (fset (fref y5 (61) ((1 65))) 0.42800000000000005d0)
  (fset (fref y5 (62) ((1 65))) 0.292d0)
  (fset (fref y5 (63) ((1 65))) 0.16200000000000003d0)
  (fset (fref y5 (64) ((1 65))) 0.098d0)
  (fset (fref y5 (65) ((1 65))) 0.054000000000000006d0)
  (defun ssqfcn (m n x fvec nprob)
    (declare (type (array double-float (*)) fvec x) (type integer4 nprob n m))
    (labels ((dfloat (ivar)
               (coerce ivar 'double-float)))
      (declare
       (ftype (function (integer4) (values double-float &rest t)) dfloat))
      (prog ((div 0.0d0) (dx 0.0d0) (prod 0.0d0) (sum 0.0d0) (s1 0.0d0)
             (s2 0.0d0) (temp 0.0d0) (ti 0.0d0) (tmp1 0.0d0) (tmp2 0.0d0)
             (tmp3 0.0d0) (tmp4 0.0d0) (tpi 0.0d0) (i 0) (iev 0) (ivar 0) (j 0)
             (nm1 0))
        (declare (type integer4 nm1 j ivar iev i)
                 (type double-float tpi tmp4 tmp3 tmp2 tmp1 ti temp s2 s1 sum
                  prod dx div))
        (declare (ftype (function (double-float) (values double-float)) datan))
        (declare
         (ftype
          (function (double-float array-double-float) (values single-float))
          dsign))
        (declare (ftype (function (double-float) (values double-float)) dsqrt))
        (declare (ftype (function (double-float) (values double-float)) dexp))
        (declare (ftype (function (double-float) (values double-float)) dsin))
        (declare (ftype (function (double-float) (values double-float)) dcos))
        (computed-goto
         (label10 label40 label70 label110 label120 label130 label140 label150
          label170 label190 label210 label250 label270 label290 label310
          label360 label390 label410)
         nprob)
       label10
        (setf sum zero)
        (fdo (j 1 (+ j 1))
             ((> j n) nil)
             (tagbody (setf sum (+ sum (fref x (j) ((1 n))))) label20))
        (setf temp (+ (/ (* two sum) (dfloat m)) one))
        (fdo (i 1 (+ i 1))
             ((> i m) nil)
             (tagbody
               (fset (fref fvec (i) ((1 m))) (- temp))
               (if (<= i n)
                   (fset (fref fvec (i) ((1 m)))
                         (+ (fref fvec (i) ((1 m))) (fref x (i) ((1 n))))))
              label30))
        (go label430)
       label40
        (setf sum zero)
        (fdo (j 1 (+ j 1))
             ((> j n) nil)
             (tagbody
               (setf sum (+ sum (* (dfloat j) (fref x (j) ((1 n))))))
              label50))
        (fdo (i 1 (+ i 1))
             ((> i m) nil)
             (tagbody
               (fset (fref fvec (i) ((1 m))) (- (* (dfloat i) sum) one))
              label60))
        (go label430)
       label70
        (setf sum zero)
        (setf nm1 (- n 1))
        (if (< nm1 2) (go label90))
        (fdo (j 2 (+ j 1))
             ((> j nm1) nil)
             (tagbody
               (setf sum (+ sum (* (dfloat j) (fref x (j) ((1 n))))))
              label80))
       label90
        (fdo (i 1 (+ i 1))
             ((> i m) nil)
             (tagbody
               (fset (fref fvec (i) ((1 m))) (- (* (dfloat (- i 1)) sum) one))
              label100))
        (fset (fref fvec (m) ((1 m))) (- one))
        (go label430)
       label110
        (fset (fref fvec (1) ((1 m)))
              (* ten (- (fref x (2) ((1 n))) (expt (fref x (1) ((1 n))) 2))))
        (fset (fref fvec (2) ((1 m))) (- one (fref x (1) ((1 n)))))
        (go label430)
       label120
        (setf tpi (* eight (datan one)))
        (setf tmp1 (coerce (dsign zp25 (fref x (2) ((1 n)))) 'double-float))
        (if (> (fref x (1) ((1 n))) zero)
            (setf tmp1
                    (/ (datan (/ (fref x (2) ((1 n))) (fref x (1) ((1 n)))))
                       tpi)))
        (if (< (fref x (1) ((1 n))) zero)
            (setf tmp1
                    (+
                     (/ (datan (/ (fref x (2) ((1 n))) (fref x (1) ((1 n)))))
                        tpi)
                     zp5)))
        (setf tmp2
                (dsqrt
                 (+ (expt (fref x (1) ((1 n))) 2)
                    (expt (fref x (2) ((1 n))) 2))))
        (fset (fref fvec (1) ((1 m)))
              (* ten (- (fref x (3) ((1 n))) (* ten tmp1))))
        (fset (fref fvec (2) ((1 m))) (* ten (- tmp2 one)))
        (fset (fref fvec (3) ((1 m))) (fref x (3) ((1 n))))
        (go label430)
       label130
        (fset (fref fvec (1) ((1 m)))
              (+ (fref x (1) ((1 n))) (* ten (fref x (2) ((1 n))))))
        (fset (fref fvec (2) ((1 m)))
              (* (dsqrt five) (- (fref x (3) ((1 n))) (fref x (4) ((1 n))))))
        (fset (fref fvec (3) ((1 m)))
              (expt (- (fref x (2) ((1 n))) (* two (fref x (3) ((1 n))))) 2))
        (fset (fref fvec (4) ((1 m)))
              (* (dsqrt ten)
                 (expt (- (fref x (1) ((1 n))) (fref x (4) ((1 n)))) 2)))
        (go label430)
       label140
        (fset (fref fvec (1) ((1 m)))
              (+ (- (fref x (1) ((1 n))) c13)
                 (*
                  (- (* (- five (fref x (2) ((1 n)))) (fref x (2) ((1 n))))
                     two)
                  (fref x (2) ((1 n))))))
        (fset (fref fvec (2) ((1 m)))
              (+ (- (fref x (1) ((1 n))) c29)
                 (*
                  (- (* (+ one (fref x (2) ((1 n)))) (fref x (2) ((1 n)))) c14)
                  (fref x (2) ((1 n))))))
        (go label430)
       label150
        (fdo (i 1 (+ i 1))
             ((> i 15) nil)
             (tagbody
               (setf tmp1 (dfloat i))
               (setf tmp2 (dfloat (- 16 i)))
               (setf tmp3 tmp1)
               (if (> i 8) (setf tmp3 tmp2))
               (fset (fref fvec (i) ((1 m)))
                     (- (fref y1 (i) ((1 15)))
                        (+ (fref x (1) ((1 n)))
                           (/ tmp1
                              (+ (* (fref x (2) ((1 n))) tmp2)
                                 (* (fref x (3) ((1 n))) tmp3))))))
              label160))
        (go label430)
       label170
        (fdo (i 1 (+ i 1))
             ((> i 11) nil)
             (tagbody
               (setf tmp1
                       (* (fref v (i) ((1 11)))
                          (+ (fref v (i) ((1 11))) (fref x (2) ((1 n))))))
               (setf tmp2
                       (+
                        (* (fref v (i) ((1 11)))
                           (+ (fref v (i) ((1 11))) (fref x (3) ((1 n)))))
                        (fref x (4) ((1 n)))))
               (fset (fref fvec (i) ((1 m)))
                     (+ (fref y2 (i) ((1 11)))
                        (/ (* (- (fref x (1) ((1 n)))) tmp1) tmp2)))
              label180))
        (go label430)
       label190
        (fdo (i 1 (+ i 1))
             ((> i 16) nil)
             (tagbody
               (setf temp (+ (* five (dfloat i)) c45 (fref x (3) ((1 n)))))
               (setf tmp1 (/ (fref x (2) ((1 n))) temp))
               (setf tmp2 (dexp tmp1))
               (fset (fref fvec (i) ((1 m)))
                     (- (* (fref x (1) ((1 n))) tmp2) (fref y3 (i) ((1 16)))))
              label200))
        (go label430)
       label210
        (fdo (i 1 (+ i 1))
             ((> i 29) nil)
             (tagbody
               (setf div (/ (dfloat i) c29))
               (setf s1 zero)
               (setf dx one)
               (fdo (j 2 (+ j 1))
                    ((> j n) nil)
                    (tagbody
                      (setf s1
                              (+ s1
                                 (* (dfloat (- j 1)) dx (fref x (j) ((1 n))))))
                      (setf dx (* div dx))
                     label220))
               (setf s2 zero)
               (setf dx one)
               (fdo (j 1 (+ j 1))
                    ((> j n) nil)
                    (tagbody
                      (setf s2 (+ s2 (* dx (fref x (j) ((1 n))))))
                      (setf dx (* div dx))
                     label230))
               (fset (fref fvec (i) ((1 m))) (- s1 (expt s2 2) one))
              label240))
        (fset (fref fvec (30) ((1 m))) (fref x (1) ((1 n))))
        (fset (fref fvec (31) ((1 m)))
              (- (fref x (2) ((1 n))) (expt (fref x (1) ((1 n))) 2) one))
        (go label430)
       label250
        (fdo (i 1 (+ i 1))
             ((> i m) nil)
             (tagbody
               (setf temp (dfloat i))
               (setf tmp1 (/ temp ten))
               (fset (fref fvec (i) ((1 m)))
                     (+
                      (- (dexp (* (- tmp1) (fref x (1) ((1 n)))))
                         (dexp (* (- tmp1) (fref x (2) ((1 n))))))
                      (* (- (dexp (- temp)) (dexp (- tmp1)))
                         (fref x (3) ((1 n))))))
              label260))
        (go label430)
       label270
        (fdo (i 1 (+ i 1))
             ((> i m) nil)
             (tagbody
               (setf temp (dfloat i))
               (fset (fref fvec (i) ((1 m)))
                     (- (+ two (* two temp))
                        (dexp (* temp (fref x (1) ((1 n)))))
                        (dexp (* temp (fref x (2) ((1 n)))))))
              label280))
        (go label430)
       label290
        (fdo (i 1 (+ i 1))
             ((> i m) nil)
             (tagbody
               (setf temp (/ (dfloat i) five))
               (setf tmp1
                       (-
                        (+ (fref x (1) ((1 n))) (* temp (fref x (2) ((1 n)))))
                        (dexp temp)))
               (setf tmp2
                       (-
                        (+ (fref x (3) ((1 n)))
                           (* (dsin temp) (fref x (4) ((1 n)))))
                        (dcos temp)))
               (fset (fref fvec (i) ((1 m))) (+ (expt tmp1 2) (expt tmp2 2)))
              label300))
        (go label430)
       label310
        (fdo (i 1 (+ i 1))
             ((> i m) nil)
             (tagbody (fset (fref fvec (i) ((1 m))) zero) label320))
        (fdo (j 1 (+ j 1))
             ((> j n) nil)
             (tagbody
               (setf tmp1 one)
               (setf tmp2 (- (* two (fref x (j) ((1 n)))) one))
               (setf temp (* two tmp2))
               (fdo (i 1 (+ i 1))
                    ((> i m) nil)
                    (tagbody
                      (fset (fref fvec (i) ((1 m)))
                            (+ (fref fvec (i) ((1 m))) tmp2))
                      (setf ti (- (* temp tmp2) tmp1))
                      (setf tmp1 tmp2)
                      (setf tmp2 ti)
                     label330))
              label340))
        (setf dx (/ one (dfloat n)))
        (setf iev -1)
        (fdo (i 1 (+ i 1))
             ((> i m) nil)
             (tagbody
               (fset (fref fvec (i) ((1 m))) (* dx (fref fvec (i) ((1 m)))))
               (if (> iev 0)
                   (fset (fref fvec (i) ((1 m)))
                         (+ (fref fvec (i) ((1 m)))
                            (/ one (- (expt (dfloat i) 2) one)))))
               (setf iev (- iev))
              label350))
        (go label430)
       label360
        (setf sum (- (dfloat (+ n 1))))
        (setf prod one)
        (fdo (j 1 (+ j 1))
             ((> j n) nil)
             (tagbody
               (setf sum (+ sum (fref x (j) ((1 n)))))
               (setf prod (* (fref x (j) ((1 n))) prod))
              label370))
        (fdo (i 1 (+ i 1))
             ((> i n) nil)
             (tagbody
               (fset (fref fvec (i) ((1 m))) (+ (fref x (i) ((1 n))) sum))
              label380))
        (fset (fref fvec (n) ((1 m))) (- prod one))
        (go label430)
       label390
        (fdo (i 1 (+ i 1))
             ((> i 33) nil)
             (tagbody
               (setf temp (* ten (dfloat (- i 1))))
               (setf tmp1 (dexp (* (- (fref x (4) ((1 n)))) temp)))
               (setf tmp2 (dexp (* (- (fref x (5) ((1 n)))) temp)))
               (fset (fref fvec (i) ((1 m)))
                     (- (fref y4 (i) ((1 33)))
                        (+ (fref x (1) ((1 n)))
                           (* (fref x (2) ((1 n))) tmp1)
                           (* (fref x (3) ((1 n))) tmp2))))
              label400))
        (go label430)
       label410
        (fdo (i 1 (+ i 1))
             ((> i 65) nil)
             (tagbody
               (setf temp (/ (dfloat (- i 1)) ten))
               (setf tmp1 (dexp (* (- (fref x (5) ((1 n)))) temp)))
               (setf tmp2
                       (dexp
                        (* (- (fref x (6) ((1 n))))
                           (expt (- temp (fref x (9) ((1 n)))) 2))))
               (setf tmp3
                       (dexp
                        (* (- (fref x (7) ((1 n))))
                           (expt (- temp (fref x (10) ((1 n)))) 2))))
               (setf tmp4
                       (dexp
                        (* (- (fref x (8) ((1 n))))
                           (expt (- temp (fref x (11) ((1 n)))) 2))))
               (fset (fref fvec (i) ((1 m)))
                     (- (fref y5 (i) ((1 65)))
                        (+ (* (fref x (1) ((1 n))) tmp1)
                           (* (fref x (2) ((1 n))) tmp2)
                           (* (fref x (3) ((1 n))) tmp3)
                           (* (fref x (4) ((1 n))) tmp4))))
              label420))
       label430
        (go end_label)
       end_label
        (return (values m n x fvec nprob))))))

;;; Compiled by f2cl version 2.0 beta on 2002/01/06 at 19:01:16
;;; 
;;; Options: ((:PRUNE-LABELS NIL) (:AUTO-SAVE T) (:RELAXED-ARRAY-DECLS T)
;;;           (:COERCE-ASSIGNS :AS-NEEDED) (:ARRAY-TYPE 'SIMPLE-ARRAY)
;;;           (:ARRAY-SLICING T))


(use-package :f2cl)

(let ((zero 0.0d0)
      (half 0.5d0)
      (one 1.0d0)
      (two 2.0d0)
      (three 3.0d0)
      (five 5.0d0)
      (seven 7.0d0)
      (ten 10.0d0)
      (twenty 20.0d0)
      (twntf 25.0d0)
      (c1 1.2d0)
      (c2 0.25d0)
      (c3 0.39d0)
      (c4 0.41500000000000004d0)
      (c5 0.02d0)
      (c6 4000.0d0)
      (c7 250.0d0)
      (c8 0.30000000000000004d0)
      (c9 0.4d0)
      (c10 1.5d0)
      (c11 0.01d0)
      (c12 1.3d0)
      (c13 0.65d0)
      (c14 0.7000000000000001d0)
      (c15 0.6000000000000001d0)
      (c16 4.5d0)
      (c17 5.5d0))
  (declare
   (type double-float c17 c16 c15 c14 c13 c12 c11 c10 c9 c8 c7 c6 c5 c4 c3 c2
    c1 twntf twenty ten seven five three two one half zero))
  (defun initpt (n x nprob factor)
    (declare (type double-float factor)
             (type (array double-float (*)) x)
             (type integer4 nprob n))
    (labels ((dfloat (ivar)
               (coerce ivar 'double-float)))
      (declare
       (ftype (function (integer4) (values double-float &rest t)) dfloat))
      (prog ((h 0.0d0) (ivar 0) (j 0))
        (declare (type integer4 j ivar) (type double-float h))
        (computed-goto
         (label10 label10 label10 label30 label40 label50 label60 label70
          label80 label90 label100 label120 label130 label140 label150 label170
          label190 label200)
         nprob)
       label10
        (fdo (j 1 (+ j 1))
             ((> j n) nil)
             (tagbody (fset (fref x (j) ((1 n))) one) label20))
        (go label210)
       label30
        (fset (fref x (1) ((1 n))) (- c1))
        (fset (fref x (2) ((1 n))) one)
        (go label210)
       label40
        (fset (fref x (1) ((1 n))) (- one))
        (fset (fref x (2) ((1 n))) zero)
        (fset (fref x (3) ((1 n))) zero)
        (go label210)
       label50
        (fset (fref x (1) ((1 n))) three)
        (fset (fref x (2) ((1 n))) (- one))
        (fset (fref x (3) ((1 n))) zero)
        (fset (fref x (4) ((1 n))) one)
        (go label210)
       label60
        (fset (fref x (1) ((1 n))) half)
        (fset (fref x (2) ((1 n))) (- two))
        (go label210)
       label70
        (fset (fref x (1) ((1 n))) one)
        (fset (fref x (2) ((1 n))) one)
        (fset (fref x (3) ((1 n))) one)
        (go label210)
       label80
        (fset (fref x (1) ((1 n))) c2)
        (fset (fref x (2) ((1 n))) c3)
        (fset (fref x (3) ((1 n))) c4)
        (fset (fref x (4) ((1 n))) c3)
        (go label210)
       label90
        (fset (fref x (1) ((1 n))) c5)
        (fset (fref x (2) ((1 n))) c6)
        (fset (fref x (3) ((1 n))) c7)
        (go label210)
       label100
        (fdo (j 1 (+ j 1))
             ((> j n) nil)
             (tagbody (fset (fref x (j) ((1 n))) zero) label110))
        (go label210)
       label120
        (fset (fref x (1) ((1 n))) zero)
        (fset (fref x (2) ((1 n))) ten)
        (fset (fref x (3) ((1 n))) twenty)
        (go label210)
       label130
        (fset (fref x (1) ((1 n))) c8)
        (fset (fref x (2) ((1 n))) c9)
        (go label210)
       label140
        (fset (fref x (1) ((1 n))) twntf)
        (fset (fref x (2) ((1 n))) five)
        (fset (fref x (3) ((1 n))) (- five))
        (fset (fref x (4) ((1 n))) (- one))
        (go label210)
       label150
        (setf h (/ one (dfloat (+ n 1))))
        (fdo (j 1 (+ j 1))
             ((> j n) nil)
             (tagbody (fset (fref x (j) ((1 n))) (* (dfloat j) h)) label160))
        (go label210)
       label170
        (fdo (j 1 (+ j 1))
             ((> j n) nil)
             (tagbody (fset (fref x (j) ((1 n))) half) label180))
        (go label210)
       label190
        (fset (fref x (1) ((1 n))) half)
        (fset (fref x (2) ((1 n))) c10)
        (fset (fref x (3) ((1 n))) (- one))
        (fset (fref x (4) ((1 n))) c11)
        (fset (fref x (5) ((1 n))) c5)
        (go label210)
       label200
        (fset (fref x (1) ((1 n))) c12)
        (fset (fref x (2) ((1 n))) c13)
        (fset (fref x (3) ((1 n))) c13)
        (fset (fref x (4) ((1 n))) c14)
        (fset (fref x (5) ((1 n))) c15)
        (fset (fref x (6) ((1 n))) three)
        (fset (fref x (7) ((1 n))) five)
        (fset (fref x (8) ((1 n))) seven)
        (fset (fref x (9) ((1 n))) two)
        (fset (fref x (10) ((1 n))) c16)
        (fset (fref x (11) ((1 n))) c17)
       label210
        (if (= factor one) (go label260))
        (if (= nprob 11) (go label230))
        (fdo (j 1 (+ j 1))
             ((> j n) nil)
             (tagbody
               (fset (fref x (j) ((1 n))) (* factor (fref x (j) ((1 n)))))
              label220))
        (go label250)
       label230
        (fdo (j 1 (+ j 1))
             ((> j n) nil)
             (tagbody (fset (fref x (j) ((1 n))) factor) label240))
       label250
       label260
        (go end_label)
       end_label
        (return (values n x nprob factor))))))

