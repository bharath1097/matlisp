;;; Compiled by f2cl version 2.0 beta on 2002/01/07 at 17:54:10
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls nil)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t))

(in-package "MINPACK")



(use-package :f2cl)

(let ((one 1.0d0) (zero 0.0d0))
  (declare (type double-float zero one))
  (defun dogleg (n r lr diag qtb delta x wa1 wa2)
    (declare (type double-float delta)
             (type (array double-float (*)) wa2 wa1 x qtb diag r)
             (type integer4 lr n))
    (prog ((alpha 0.0d0) (bnorm 0.0d0) (epsmch 0.0d0) (gnorm 0.0d0)
           (qnorm 0.0d0) (sgnorm 0.0d0) (sum 0.0d0) (temp 0.0d0) (i 0) (j 0)
           (jj 0) (jp1 0) (k 0) (l 0))
      (declare (type integer4 l k jp1 jj j i)
               (type double-float temp sum sgnorm qnorm gnorm epsmch bnorm
                alpha))
      (declare
       (ftype (function (integer4) (values double-float &rest t)) dpmpar))
      (declare
       (ftype (function (array-double-float) (values double-float)) dabs))
      (declare
       (ftype (function (double-float double-float) (values double-float))
        dmax1))
      (declare
       (ftype
        (function (integer4 array-double-float) (values double-float &rest t))
        enorm))
      (declare (ftype (function (double-float) (values double-float)) dsqrt))
      (declare
       (ftype (function (double-float double-float) (values double-float))
        dmin1))
      '"     **********"
      '""
      '"     subroutine dogleg"
      '""
      '"     given an m by n matrix a, an n by n nonsingular diagonal"
      '"     matrix d, an m-vector b, and a positive number delta, the"
      '"     problem is to determine the convex combination x of the"
      '"     gauss-newton and scaled gradient directions that minimizes"
      '"     (a*x - b) in the least squares sense, subject to the"
      '"     restriction that the euclidean norm of d*x be at most delta."
      '""
      '"     this subroutine completes the solution of the problem"
      '"     if it is provided with the necessary information from the"
      '"     qr factorization of a. that is, if a = q*r, where q has"
      '"     orthogonal columns and r is an upper triangular matrix,"
      '"     then dogleg expects the full upper triangle of r and"
      '"     the first n components of (q transpose)*b."
      '""
      '"     the subroutine statement is"
      '""
      '"       subroutine dogleg(n,r,lr,diag,qtb,delta,x,wa1,wa2)"
      '""
      '"     where"
      '""
      '"       n is a positive integer input variable set to the order of r."
      '""
      '"       r is an input array of length lr which must contain the upper"
      '"         triangular matrix r stored by rows."
      '""
      '"       lr is a positive integer input variable not less than"
      '"         (n*(n+1))/2."
      '""
      '"       diag is an input array of length n which must contain the"
      '"         diagonal elements of the matrix d."
      '""
      '"       qtb is an input array of length n which must contain the first"
      '"         n elements of the vector (q transpose)*b."
      '""
      '"       delta is a positive input variable which specifies an upper"
      '"         bound on the euclidean norm of d*x."
      '""
      '"       x is an output array of length n which contains the desired"
      '"         convex combination of the gauss-newton direction and the"
      '"         scaled gradient direction."
      '""
      '"       wa1 and wa2 are work arrays of length n."
      '""
      '"     subprograms called"
      '""
      '"       minpack-supplied ... dpmpar,enorm"
      '""
      '"       fortran-supplied ... dabs,dmax1,dmin1,dsqrt"
      '""
      '"     argonne national laboratory. minpack project. march 1980."
      '"     burton s. garbow, kenneth e. hillstrom, jorge j. more"
      '""
      '"     **********"
      '""
      '"     epsmch is the machine precision."
      '""
      (setf epsmch (dpmpar 1))
      '""
      '"     first, calculate the gauss-newton direction."
      '""
      (setf jj (+ (truncate (* n (+ n 1)) 2) 1))
      (fdo (k 1 (+ k 1))
           ((> k n) nil)
           (tagbody
             (setf j (+ (- n k) 1))
             (setf jp1 (+ j 1))
             (setf jj (- jj k))
             (setf l (+ jj 1))
             (setf sum zero)
             (if (< n jp1) (go label20))
             (fdo (i jp1 (+ i 1))
                  ((> i n) nil)
                  (tagbody
                    (setf sum
                            (+ sum
                               (* (fref r (l) ((1 lr))) (fref x (i) ((1 n))))))
                    (setf l (+ l 1))
                   label10))
            label20
             (setf temp (fref r (jj) ((1 lr))))
             (if (/= temp zero) (go label40))
             (setf l j)
             (fdo (i 1 (+ i 1))
                  ((> i j) nil)
                  (tagbody
                    (setf temp (dmax1 temp (dabs (fref r (l) ((1 lr))))))
                    (setf l (- (+ l n) i))
                   label30))
             (setf temp (* epsmch temp))
             (if (= temp zero) (setf temp epsmch))
            label40
             (fset (fref x (j) ((1 n)))
                   (/ (- (fref qtb (j) ((1 n))) sum) temp))
            label50))
      '""
      '"     test whether the gauss-newton direction is acceptable."
      '""
      (fdo (j 1 (+ j 1))
           ((> j n) nil)
           (tagbody
             (fset (fref wa1 (j) ((1 n))) zero)
             (fset (fref wa2 (j) ((1 n)))
                   (* (fref diag (j) ((1 n))) (fref x (j) ((1 n)))))
            label60))
      (setf qnorm
              (coerce
               (multiple-value-bind
                   (ret-val var-0 var-1)
                   (enorm n wa2)
                 (declare (ignore var-1))
                 (when var-0 (setf n var-0))
                 ret-val)
               'double-float))
      (if (<= qnorm delta) (go label140))
      '""
      '"     the gauss-newton direction is not acceptable."
      '"     next, calculate the scaled gradient direction."
      '""
      (setf l 1)
      (fdo (j 1 (+ j 1))
           ((> j n) nil)
           (tagbody
             (setf temp (fref qtb (j) ((1 n))))
             (fdo (i j (+ i 1))
                  ((> i n) nil)
                  (tagbody
                    (fset (fref wa1 (i) ((1 n)))
                          (+ (fref wa1 (i) ((1 n)))
                             (* (fref r (l) ((1 lr))) temp)))
                    (setf l (+ l 1))
                   label70))
             (fset (fref wa1 (j) ((1 n)))
                   (/ (fref wa1 (j) ((1 n))) (fref diag (j) ((1 n)))))
            label80))
      '""
      '"     calculate the norm of the scaled gradient and test for"
      '"     the special case in which the scaled gradient is zero."
      '""
      (setf gnorm
              (coerce
               (multiple-value-bind
                   (ret-val var-0 var-1)
                   (enorm n wa1)
                 (declare (ignore var-1))
                 (when var-0 (setf n var-0))
                 ret-val)
               'double-float))
      (setf sgnorm zero)
      (setf alpha (/ delta qnorm))
      (if (= gnorm zero) (go label120))
      '""
      '"     calculate the point along the scaled gradient"
      '"     at which the quadratic is minimized."
      '""
      (fdo (j 1 (+ j 1))
           ((> j n) nil)
           (tagbody
             (fset (fref wa1 (j) ((1 n)))
                   (/ (/ (fref wa1 (j) ((1 n))) gnorm)
                      (fref diag (j) ((1 n)))))
            label90))
      (setf l 1)
      (fdo (j 1 (+ j 1))
           ((> j n) nil)
           (tagbody
             (setf sum zero)
             (fdo (i j (+ i 1))
                  ((> i n) nil)
                  (tagbody
                    (setf sum
                            (+ sum
                               (* (fref r (l) ((1 lr)))
                                  (fref wa1 (i) ((1 n))))))
                    (setf l (+ l 1))
                   label100))
             (fset (fref wa2 (j) ((1 n))) sum)
            label110))
      (setf temp
              (coerce
               (multiple-value-bind
                   (ret-val var-0 var-1)
                   (enorm n wa2)
                 (declare (ignore var-1))
                 (when var-0 (setf n var-0))
                 ret-val)
               'double-float))
      (setf sgnorm (/ (/ gnorm temp) temp))
      '""
      '"     test whether the scaled gradient direction is acceptable."
      '""
      (setf alpha zero)
      (if (>= sgnorm delta) (go label120))
      '""
      '"     the scaled gradient direction is not acceptable."
      '"     finally, calculate the point along the dogleg"
      '"     at which the quadratic is minimized."
      '""
      (setf bnorm
              (coerce
               (multiple-value-bind
                   (ret-val var-0 var-1)
                   (enorm n qtb)
                 (declare (ignore var-1))
                 (when var-0 (setf n var-0))
                 ret-val)
               'double-float))
      (setf temp (* (/ bnorm gnorm) (/ bnorm qnorm) (/ sgnorm delta)))
      (setf temp
              (+ (- temp (* (/ delta qnorm) (expt (/ sgnorm delta) 2)))
                 (dsqrt
                  (+ (expt (- temp (/ delta qnorm)) 2)
                     (* (- one (expt (/ delta qnorm) 2))
                        (- one (expt (/ sgnorm delta) 2)))))))
      (setf alpha
              (/ (* (/ delta qnorm) (- one (expt (/ sgnorm delta) 2))) temp))
     label120
      '""
      '"     form appropriate convex combination of the gauss-newton"
      '"     direction and the scaled gradient direction."
      '""
      (setf temp (* (- one alpha) (dmin1 sgnorm delta)))
      (fdo (j 1 (+ j 1))
           ((> j n) nil)
           (tagbody
             (fset (fref x (j) ((1 n)))
                   (+ (* temp (fref wa1 (j) ((1 n))))
                      (* alpha (fref x (j) ((1 n))))))
            label130))
     label140
      (go end_label)
      '""
      '"     last card of subroutine dogleg."
      '""
     end_label
      (return (values n r lr diag qtb delta x wa1 wa2)))))

