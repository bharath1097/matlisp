;;; Compiled by f2cl version 2.0 beta on 2001/02/23 at 10:09:05
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type 'simple-array)
;;;           (:array-slicing t))


(use-package :f2cl)

(let ((wg (make-array 8 :element-type 'double-float))
      (xgk (make-array 8 :element-type 'double-float))
      (wgk (make-array 8 :element-type 'double-float)))
  (declare (type (array double-float (8)) wgk xgk wg))
  (fset (fref wg (1) ((1 8))) 0.0d0)
  (fset (fref wg (2) ((1 8))) 0.1294849661688697d0)
  (fset (fref wg (3) ((1 8))) 0.0d0)
  (fset (fref wg (4) ((1 8))) 0.27970539148927664d0)
  (fset (fref wg (5) ((1 8))) 0.0d0)
  (fset (fref wg (6) ((1 8))) 0.3818300505051189d0)
  (fset (fref wg (7) ((1 8))) 0.0d0)
  (fset (fref wg (8) ((1 8))) 0.4179591836734694d0)
  (fset (fref xgk (1) ((1 8))) 0.9914553711208126d0)
  (fset (fref xgk (2) ((1 8))) 0.9491079123427585d0)
  (fset (fref xgk (3) ((1 8))) 0.8648644233597691d0)
  (fset (fref xgk (4) ((1 8))) 0.7415311855993945d0)
  (fset (fref xgk (5) ((1 8))) 0.5860872354676911d0)
  (fset (fref xgk (6) ((1 8))) 0.4058451513773972d0)
  (fset (fref xgk (7) ((1 8))) 0.20778495500789848d0)
  (fset (fref xgk (8) ((1 8))) 0.0d0)
  (fset (fref wgk (1) ((1 8))) 0.022935322010529224d0)
  (fset (fref wgk (2) ((1 8))) 0.06309209262997856d0)
  (fset (fref wgk (3) ((1 8))) 0.10479001032225019d0)
  (fset (fref wgk (4) ((1 8))) 0.14065325971552592d0)
  (fset (fref wgk (5) ((1 8))) 0.1690047266392679d0)
  (fset (fref wgk (6) ((1 8))) 0.19035057806478542d0)
  (fset (fref wgk (7) ((1 8))) 0.20443294007529889d0)
  (fset (fref wgk (8) ((1 8))) 0.20948214108472782d0)
  (defun dqk15i (f boun inf a b result abserr resabs resasc)
    (declare (type integer4 inf)
             (type double-float boun a b result abserr resabs resasc)
             (type (function (double-float) (values double-float &rest t)) f))
    (prog ((j 0) (uflow 0.0d0) (tabsc2 0.0d0) (tabsc1 0.0d0) (reskh 0.0d0)
           (resk 0.0d0) (resg 0.0d0) (hlgth 0.0d0)
           (fv2 (make-array 7 :element-type 'double-float))
           (fv1 (make-array 7 :element-type 'double-float)) (fval2 0.0d0)
           (fval1 0.0d0) (fsum 0.0d0) (fc 0.0d0) (epmach 0.0d0) (dinf 0.0d0)
           (centr 0.0d0) (absc2 0.0d0) (absc1 0.0d0) (absc 0.0d0))
      (declare (type (array double-float (7)) fv1 fv2)
               (type double-float absc absc1 absc2 centr dinf epmach fc fsum
                fval1 fval2 hlgth resg resk reskh tabsc1 tabsc2 uflow)
               (type integer4 j))
      (declare
       (ftype (function (integer4) (values double-float &rest t)) d1mach))
      (declare (ftype (function (integer4 integer4) (values integer4)) min0))
      (declare (ftype (function (double-float) (values double-float)) dabs))
      (declare
       (ftype (function (double-float double-float) (values double-float))
        dmin1))
      (declare
       (ftype (function (double-float double-float) (values double-float))
        dmax1))
      (setf epmach (d1mach 4))
      (setf uflow (d1mach 1))
      (setf dinf (coerce (min0 1 inf) 'double-float))
      (setf centr (* 0.5d0 (+ a b)))
      (setf hlgth (* 0.5d0 (- b a)))
      (setf tabsc1 (+ boun (/ (* dinf (- 1.0d0 centr)) centr)))
      (setf fval1
              (coerce
               (multiple-value-bind
                   (ret-val var-0)
                   (funcall f tabsc1)
                 (declare (ignore))
                 (when var-0 (setf tabsc1 var-0))
                 ret-val)
               'double-float))
      (if (= inf 2) (setf fval1 (+ fval1 (funcall f (- tabsc1)))))
      (setf fc (/ (/ fval1 centr) centr))
      (setf resg (* (fref wg (8) ((1 8))) fc))
      (setf resk (* (fref wgk (8) ((1 8))) fc))
      (setf resabs (dabs resk))
      (fdo (j 1 (+ j 1))
           ((> j 7) nil)
           (tagbody
             (setf absc (* hlgth (fref xgk (j) ((1 8)))))
             (setf absc1 (- centr absc))
             (setf absc2 (+ centr absc))
             (setf tabsc1 (+ boun (/ (* dinf (- 1.0d0 absc1)) absc1)))
             (setf tabsc2 (+ boun (/ (* dinf (- 1.0d0 absc2)) absc2)))
             (setf fval1
                     (coerce
                      (multiple-value-bind
                          (ret-val var-0)
                          (funcall f tabsc1)
                        (declare (ignore))
                        (when var-0 (setf tabsc1 var-0))
                        ret-val)
                      'double-float))
             (setf fval2
                     (coerce
                      (multiple-value-bind
                          (ret-val var-0)
                          (funcall f tabsc2)
                        (declare (ignore))
                        (when var-0 (setf tabsc2 var-0))
                        ret-val)
                      'double-float))
             (if (= inf 2) (setf fval1 (+ fval1 (funcall f (- tabsc1)))))
             (if (= inf 2) (setf fval2 (+ fval2 (funcall f (- tabsc2)))))
             (setf fval1 (/ (/ fval1 absc1) absc1))
             (setf fval2 (/ (/ fval2 absc2) absc2))
             (fset (fref fv1 (j) ((1 7))) fval1)
             (fset (fref fv2 (j) ((1 7))) fval2)
             (setf fsum (+ fval1 fval2))
             (setf resg (+ resg (* (fref wg (j) ((1 8))) fsum)))
             (setf resk (+ resk (* (fref wgk (j) ((1 8))) fsum)))
             (setf resabs
                     (+ resabs
                        (* (fref wgk (j) ((1 8)))
                           (+ (dabs fval1) (dabs fval2)))))
            label10))
      (setf reskh (* resk 0.5d0))
      (setf resasc (* (fref wgk (8) ((1 8))) (dabs (- fc reskh))))
      (fdo (j 1 (+ j 1))
           ((> j 7) nil)
           (tagbody
             (setf resasc
                     (+ resasc
                        (* (fref wgk (j) ((1 8)))
                           (+ (dabs (- (fref fv1 (j) ((1 7))) reskh))
                              (dabs (- (fref fv2 (j) ((1 7))) reskh))))))
            label20))
      (setf result (* resk hlgth))
      (setf resasc (* resasc hlgth))
      (setf resabs (* resabs hlgth))
      (setf abserr (dabs (* (- resk resg) hlgth)))
      (if (and (/= resasc 0.0d0) (/= abserr 0.0d0))
          (setf abserr
                  (* resasc
                     (dmin1 1.0d0
                            (expt (/ (* 200.0d0 abserr) resasc) 1.5d0)))))
      (if (> resabs (/ uflow (* 50.0d0 epmach)))
          (setf abserr (dmax1 (* epmach 50.0d0 resabs) abserr)))
      (go end_label)
     end_label
      (return (values f boun inf a b result abserr resabs resasc)))))

