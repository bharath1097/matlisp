;;; Compiled by f2cl version 2.0 beta on 2001/02/23 at 10:08:35
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type 'simple-array)
;;;           (:array-slicing t))


(use-package :f2cl)

(let ((xgk (make-array 8 :element-type 'double-float))
      (wgk (make-array 8 :element-type 'double-float))
      (wg (make-array 4 :element-type 'double-float)))
  (declare (type (array double-float (4)) wg)
           (type (array double-float (8)) wgk xgk))
  (fset (fref xgk (1) ((1 8))) 0.9914553711208126d0)
  (fset (fref xgk (2) ((1 8))) 0.9491079123427585d0)
  (fset (fref xgk (3) ((1 8))) 0.8648644233597691d0)
  (fset (fref xgk (4) ((1 8))) 0.7415311855993943d0)
  (fset (fref xgk (5) ((1 8))) 0.5860872354676911d0)
  (fset (fref xgk (6) ((1 8))) 0.4058451513773972d0)
  (fset (fref xgk (7) ((1 8))) 0.2077849550078985d0)
  (fset (fref xgk (8) ((1 8))) 0.0d0)
  (fset (fref wgk (1) ((1 8))) 0.02293532201052922d0)
  (fset (fref wgk (2) ((1 8))) 0.06309209262997854d0)
  (fset (fref wgk (3) ((1 8))) 0.1047900103222502d0)
  (fset (fref wgk (4) ((1 8))) 0.1406532597155259d0)
  (fset (fref wgk (5) ((1 8))) 0.1690047266392679d0)
  (fset (fref wgk (6) ((1 8))) 0.1903505780647854d0)
  (fset (fref wgk (7) ((1 8))) 0.2044329400752989d0)
  (fset (fref wgk (8) ((1 8))) 0.2094821410847278d0)
  (fset (fref wg (1) ((1 4))) 0.1294849661688697d0)
  (fset (fref wg (2) ((1 4))) 0.2797053914892767d0)
  (fset (fref wg (3) ((1 4))) 0.3818300505051889d0)
  (fset (fref wg (4) ((1 4))) 0.4179591836734694d0)
  (defun dqk15w (f w p1 p2 p3 p4 kp a b result abserr resabs resasc)
    (declare (type integer4 kp)
             (type double-float p1 p2 p3 p4 a b result abserr resabs resasc)
             (type
              (function
               (double-float double-float double-float double-float
                double-float integer4)
               (values double-float &rest t))
              w)
             (type (function (double-float) (values double-float &rest t)) f))
    (prog ((j 0) (jtw 0) (jtwm1 0) (uflow 0.0d0) (reskh 0.0d0) (resk 0.0d0)
           (resg 0.0d0) (hlgth 0.0d0)
           (fv2 (make-array 7 :element-type 'double-float))
           (fv1 (make-array 7 :element-type 'double-float)) (fval2 0.0d0)
           (fval1 0.0d0) (fsum 0.0d0) (fc 0.0d0) (epmach 0.0d0) (dhlgth 0.0d0)
           (centr 0.0d0) (absc2 0.0d0) (absc1 0.0d0) (absc 0.0d0))
      (declare (type (array double-float (7)) fv1 fv2)
               (type double-float absc absc1 absc2 centr dhlgth epmach fc fsum
                fval1 fval2 hlgth resg resk reskh uflow)
               (type integer4 jtwm1 jtw j))
      (declare
       (ftype (function (integer4) (values double-float &rest t)) d1mach))
      (declare (ftype (function (double-float) (values double-float)) dabs))
      (declare
       (ftype (function (double-float double-float) (values double-float))
        dmin1))
      (declare
       (ftype (function (double-float double-float) (values double-float))
        dmax1))
      (setf epmach (d1mach 4))
      (setf uflow (d1mach 1))
      (setf centr (* 0.5d0 (+ a b)))
      (setf hlgth (* 0.5d0 (- b a)))
      (setf dhlgth (dabs hlgth))
      (setf fc
              (coerce
               (*
                (multiple-value-bind
                    (ret-val var-0)
                    (funcall f centr)
                  (declare (ignore))
                  (when var-0 (setf centr var-0))
                  ret-val)
                (multiple-value-bind
                    (ret-val var-0 var-1 var-2 var-3 var-4 var-5)
                    (funcall w centr p1 p2 p3 p4 kp)
                  (declare (ignore))
                  (when var-0 (setf centr var-0))
                  (when var-1 (setf p1 var-1))
                  (when var-2 (setf p2 var-2))
                  (when var-3 (setf p3 var-3))
                  (when var-4 (setf p4 var-4))
                  (when var-5 (setf kp var-5))
                  ret-val))
               'double-float))
      (setf resg (* (fref wg (4) ((1 4))) fc))
      (setf resk (* (fref wgk (8) ((1 8))) fc))
      (setf resabs (dabs resk))
      (fdo (j 1 (+ j 1))
           ((> j 3) nil)
           (tagbody
             (setf jtw (* j 2))
             (setf absc (* hlgth (fref xgk (jtw) ((1 8)))))
             (setf absc1 (- centr absc))
             (setf absc2 (+ centr absc))
             (setf fval1
                     (coerce
                      (*
                       (multiple-value-bind
                           (ret-val var-0)
                           (funcall f absc1)
                         (declare (ignore))
                         (when var-0 (setf absc1 var-0))
                         ret-val)
                       (multiple-value-bind
                           (ret-val var-0 var-1 var-2 var-3 var-4 var-5)
                           (funcall w absc1 p1 p2 p3 p4 kp)
                         (declare (ignore))
                         (when var-0 (setf absc1 var-0))
                         (when var-1 (setf p1 var-1))
                         (when var-2 (setf p2 var-2))
                         (when var-3 (setf p3 var-3))
                         (when var-4 (setf p4 var-4))
                         (when var-5 (setf kp var-5))
                         ret-val))
                      'double-float))
             (setf fval2
                     (coerce
                      (*
                       (multiple-value-bind
                           (ret-val var-0)
                           (funcall f absc2)
                         (declare (ignore))
                         (when var-0 (setf absc2 var-0))
                         ret-val)
                       (multiple-value-bind
                           (ret-val var-0 var-1 var-2 var-3 var-4 var-5)
                           (funcall w absc2 p1 p2 p3 p4 kp)
                         (declare (ignore))
                         (when var-0 (setf absc2 var-0))
                         (when var-1 (setf p1 var-1))
                         (when var-2 (setf p2 var-2))
                         (when var-3 (setf p3 var-3))
                         (when var-4 (setf p4 var-4))
                         (when var-5 (setf kp var-5))
                         ret-val))
                      'double-float))
             (fset (fref fv1 (jtw) ((1 7))) fval1)
             (fset (fref fv2 (jtw) ((1 7))) fval2)
             (setf fsum (+ fval1 fval2))
             (setf resg (+ resg (* (fref wg (j) ((1 4))) fsum)))
             (setf resk (+ resk (* (fref wgk (jtw) ((1 8))) fsum)))
             (setf resabs
                     (+ resabs
                        (* (fref wgk (jtw) ((1 8)))
                           (+ (dabs fval1) (dabs fval2)))))
            label10))
      (fdo (j 1 (+ j 1))
           ((> j 4) nil)
           (tagbody
             (setf jtwm1 (- (* j 2) 1))
             (setf absc (* hlgth (fref xgk (jtwm1) ((1 8)))))
             (setf absc1 (- centr absc))
             (setf absc2 (+ centr absc))
             (setf fval1
                     (coerce
                      (*
                       (multiple-value-bind
                           (ret-val var-0)
                           (funcall f absc1)
                         (declare (ignore))
                         (when var-0 (setf absc1 var-0))
                         ret-val)
                       (multiple-value-bind
                           (ret-val var-0 var-1 var-2 var-3 var-4 var-5)
                           (funcall w absc1 p1 p2 p3 p4 kp)
                         (declare (ignore))
                         (when var-0 (setf absc1 var-0))
                         (when var-1 (setf p1 var-1))
                         (when var-2 (setf p2 var-2))
                         (when var-3 (setf p3 var-3))
                         (when var-4 (setf p4 var-4))
                         (when var-5 (setf kp var-5))
                         ret-val))
                      'double-float))
             (setf fval2
                     (coerce
                      (*
                       (multiple-value-bind
                           (ret-val var-0)
                           (funcall f absc2)
                         (declare (ignore))
                         (when var-0 (setf absc2 var-0))
                         ret-val)
                       (multiple-value-bind
                           (ret-val var-0 var-1 var-2 var-3 var-4 var-5)
                           (funcall w absc2 p1 p2 p3 p4 kp)
                         (declare (ignore))
                         (when var-0 (setf absc2 var-0))
                         (when var-1 (setf p1 var-1))
                         (when var-2 (setf p2 var-2))
                         (when var-3 (setf p3 var-3))
                         (when var-4 (setf p4 var-4))
                         (when var-5 (setf kp var-5))
                         ret-val))
                      'double-float))
             (fset (fref fv1 (jtwm1) ((1 7))) fval1)
             (fset (fref fv2 (jtwm1) ((1 7))) fval2)
             (setf fsum (+ fval1 fval2))
             (setf resk (+ resk (* (fref wgk (jtwm1) ((1 8))) fsum)))
             (setf resabs
                     (+ resabs
                        (* (fref wgk (jtwm1) ((1 8)))
                           (+ (dabs fval1) (dabs fval2)))))
            label15))
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
      (setf resabs (* resabs dhlgth))
      (setf resasc (* resasc dhlgth))
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
      (return (values f w p1 p2 p3 p4 kp a b result abserr resabs resasc)))))

