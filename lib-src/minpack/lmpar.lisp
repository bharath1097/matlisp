;;; Compiled by f2cl version 2.0 beta on 2002/01/07 at 17:53:53
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls nil)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t))

(in-package "MINPACK")



(use-package :f2cl)

(let ((p1 0.1d0) (p001 0.001d0) (zero 0.0d0))
  (declare (type double-float zero p001 p1))
  (defun lmpar (n r ldr ipvt diag qtb delta par x sdiag wa1 wa2)
    (declare (type double-float par delta)
             (type (array integer4 (*)) ipvt)
             (type (array double-float (*)) wa2 wa1 sdiag x qtb diag r)
             (type integer4 ldr n))
    (prog ((dxnorm 0.0d0) (dwarf 0.0d0) (fp 0.0d0) (gnorm 0.0d0) (parc 0.0d0)
           (parl 0.0d0) (paru 0.0d0) (sum 0.0d0) (temp 0.0d0) (i 0) (iter 0)
           (j 0) (jm1 0) (jp1 0) (k 0) (l 0) (nsing 0))
      (declare (type integer4 nsing l k jp1 jm1 j iter i)
               (type double-float temp sum paru parl parc gnorm fp dwarf
                dxnorm))
      (declare
       (ftype (function (integer4) (values double-float &rest t)) dpmpar))
      (declare
       (ftype
        (function (integer4 array-double-float) (values double-float &rest t))
        enorm))
      (declare
       (ftype (function (double-float double-float) (values double-float))
        dmin1))
      (declare
       (ftype (function (double-float double-float) (values double-float))
        dmax1))
      (declare (ftype (function (double-float) (values double-float)) dsqrt))
      (declare
       (ftype
        (function
         (integer4 array-double-float integer4 array-integer4
          array-double-float array-double-float array-double-float
          array-double-float array-double-float)
         (values &rest t))
        qrsolv))
      (declare (ftype (function (double-float) (values double-float)) dabs))
      '"     **********"
      '""
      '"     subroutine lmpar"
      '""
      '"     given an m by n matrix a, an n by n nonsingular diagonal"
      '"     matrix d, an m-vector b, and a positive number delta,"
      '"     the problem is to determine a value for the parameter"
      '"     par such that if x solves the system"
      '""
      '"           a*x = b ,     sqrt(par)*d*x = 0 ,"
      '""
      '"     in the least squares sense, and dxnorm is the euclidean"
      '"     norm of d*x, then either par is zero and"
      '""
      '"           (dxnorm-delta) .le. 0.1*delta ,"
      '""
      '"     or par is positive and"
      '""
      '"           abs(dxnorm-delta) .le. 0.1*delta ."
      '""
      '"     this subroutine completes the solution of the problem"
      '"     if it is provided with the necessary information from the"
      '"     qr factorization, with column pivoting, of a. that is, if"
      '"     a*p = q*r, where p is a permutation matrix, q has orthogonal"
      '"     columns, and r is an upper triangular matrix with diagonal"
      '"     elements of nonincreasing magnitude, then lmpar expects"
      '"     the full upper triangle of r, the permutation matrix p,"
      '"     and the first n components of (q transpose)*b. on output"
      '"     lmpar also provides an upper triangular matrix s such that"
      '""
      '"            t   t                   t"
      '"           p *(a *a + par*d*d)*p = s *s ."
      '""
      '"     s is employed within lmpar and may be of separate interest."
      '""
      '"     only a few iterations are generally needed for convergence"
      '"     of the algorithm. if, however, the limit of 10 iterations"
      '"     is reached, then the output par will contain the best"
      '"     value obtained so far."
      '""
      '"     the subroutine statement is"
      '""
      '"       subroutine lmpar(n,r,ldr,ipvt,diag,qtb,delta,par,x,sdiag,"
      '"                        wa1,wa2)"
      '""
      '"     where"
      '""
      '"       n is a positive integer input variable set to the order of r."
      '""
      '"       r is an n by n array. on input the full upper triangle"
      '"         must contain the full upper triangle of the matrix r."
      '"         on output the full upper triangle is unaltered, and the"
      '"         strict lower triangle contains the strict upper triangle"
      '"         (transposed) of the upper triangular matrix s."
      '""
      '"       ldr is a positive integer input variable not less than n"
      '"         which specifies the leading dimension of the array r."
      '""
      '"       ipvt is an integer input array of length n which defines the"
      '"         permutation matrix p such that a*p = q*r. column j of p"
      '"         is column ipvt(j) of the identity matrix."
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
      '"       par is a nonnegative variable. on input par contains an"
      '"         initial estimate of the levenberg-marquardt parameter."
      '"         on output par contains the final estimate."
      '""
      '"       x is an output array of length n which contains the least"
      '"         squares solution of the system a*x = b, sqrt(par)*d*x = 0,"
      '"         for the output par."
      '""
      '"       sdiag is an output array of length n which contains the"
      '"         diagonal elements of the upper triangular matrix s."
      '""
      '"       wa1 and wa2 are work arrays of length n."
      '""
      '"     subprograms called"
      '""
      '"       minpack-supplied ... dpmpar,enorm,qrsolv"
      '""
      '"       fortran-supplied ... dabs,dmax1,dmin1,dsqrt"
      '""
      '"     argonne national laboratory. minpack project. march 1980."
      '"     burton s. garbow, kenneth e. hillstrom, jorge j. more"
      '""
      '"     **********"
      '""
      '"     dwarf is the smallest positive magnitude."
      '""
      (setf dwarf (dpmpar 2))
      '""
      '"     compute and store in x the gauss-newton direction. if the"
      '"     jacobian is rank-deficient, obtain a least squares solution."
      '""
      (setf nsing n)
      (fdo (j 1 (+ j 1))
           ((> j n) nil)
           (tagbody
             (fset (fref wa1 (j) ((1 n))) (fref qtb (j) ((1 n))))
             (if (and (= (fref r (j j) ((1 ldr) (1 n))) zero) (= nsing n))
                 (setf nsing (- j 1)))
             (if (< nsing n) (fset (fref wa1 (j) ((1 n))) zero))
            label10))
      (if (< nsing 1) (go label50))
      (fdo (k 1 (+ k 1))
           ((> k nsing) nil)
           (tagbody
             (setf j (+ (- nsing k) 1))
             (fset (fref wa1 (j) ((1 n)))
                   (/ (fref wa1 (j) ((1 n))) (fref r (j j) ((1 ldr) (1 n)))))
             (setf temp (fref wa1 (j) ((1 n))))
             (setf jm1 (- j 1))
             (if (< jm1 1) (go label30))
             (fdo (i 1 (+ i 1))
                  ((> i jm1) nil)
                  (tagbody
                    (fset (fref wa1 (i) ((1 n)))
                          (- (fref wa1 (i) ((1 n)))
                             (* (fref r (i j) ((1 ldr) (1 n))) temp)))
                   label20))
            label30
            label40))
     label50
      (fdo (j 1 (+ j 1))
           ((> j n) nil)
           (tagbody
             (setf l (fref ipvt (j) ((1 n))))
             (fset (fref x (l) ((1 n))) (fref wa1 (j) ((1 n))))
            label60))
      '""
      '"     initialize the iteration counter."
      '"     evaluate the function at the origin, and test"
      '"     for acceptance of the gauss-newton direction."
      '""
      (setf iter 0)
      (fdo (j 1 (+ j 1))
           ((> j n) nil)
           (tagbody
             (fset (fref wa2 (j) ((1 n)))
                   (* (fref diag (j) ((1 n))) (fref x (j) ((1 n)))))
            label70))
      (setf dxnorm
              (coerce
               (multiple-value-bind
                   (ret-val var-0 var-1)
                   (enorm n wa2)
                 (declare (ignore var-1))
                 (when var-0 (setf n var-0))
                 ret-val)
               'double-float))
      (setf fp (- dxnorm delta))
      (if (<= fp (* p1 delta)) (go label220))
      '""
      '"     if the jacobian is not rank deficient, the newton"
      '"     step provides a lower bound, parl, for the zero of"
      '"     the function. otherwise set this bound to zero."
      '""
      (setf parl zero)
      (if (< nsing n) (go label120))
      (fdo (j 1 (+ j 1))
           ((> j n) nil)
           (tagbody
             (setf l (fref ipvt (j) ((1 n))))
             (fset (fref wa1 (j) ((1 n)))
                   (* (fref diag (l) ((1 n)))
                      (/ (fref wa2 (l) ((1 n))) dxnorm)))
            label80))
      (fdo (j 1 (+ j 1))
           ((> j n) nil)
           (tagbody
             (setf sum zero)
             (setf jm1 (- j 1))
             (if (< jm1 1) (go label100))
             (fdo (i 1 (+ i 1))
                  ((> i jm1) nil)
                  (tagbody
                    (setf sum
                            (+ sum
                               (* (fref r (i j) ((1 ldr) (1 n)))
                                  (fref wa1 (i) ((1 n))))))
                   label90))
            label100
             (fset (fref wa1 (j) ((1 n)))
                   (/ (- (fref wa1 (j) ((1 n))) sum)
                      (fref r (j j) ((1 ldr) (1 n)))))
            label110))
      (setf temp
              (coerce
               (multiple-value-bind
                   (ret-val var-0 var-1)
                   (enorm n wa1)
                 (declare (ignore var-1))
                 (when var-0 (setf n var-0))
                 ret-val)
               'double-float))
      (setf parl (/ (/ (/ fp delta) temp) temp))
     label120
      '""
      '"     calculate an upper bound, paru, for the zero of the function."
      '""
      (fdo (j 1 (+ j 1))
           ((> j n) nil)
           (tagbody
             (setf sum zero)
             (fdo (i 1 (+ i 1))
                  ((> i j) nil)
                  (tagbody
                    (setf sum
                            (+ sum
                               (* (fref r (i j) ((1 ldr) (1 n)))
                                  (fref qtb (i) ((1 n))))))
                   label130))
             (setf l (fref ipvt (j) ((1 n))))
             (fset (fref wa1 (j) ((1 n))) (/ sum (fref diag (l) ((1 n)))))
            label140))
      (setf gnorm
              (coerce
               (multiple-value-bind
                   (ret-val var-0 var-1)
                   (enorm n wa1)
                 (declare (ignore var-1))
                 (when var-0 (setf n var-0))
                 ret-val)
               'double-float))
      (setf paru (/ gnorm delta))
      (if (= paru zero) (setf paru (/ dwarf (dmin1 delta p1))))
      '""
      '"     if the input par lies outside of the interval (parl,paru),"
      '"     set par to the closer endpoint."
      '""
      (setf par (dmax1 par parl))
      (setf par (dmin1 par paru))
      (if (= par zero) (setf par (/ gnorm dxnorm)))
      '""
      '"     beginning of an iteration."
      '""
     label150
      (setf iter (+ iter 1))
      '""
      '"        evaluate the function at the current value of par."
      '""
      (if (= par zero) (setf par (dmax1 dwarf (* p001 paru))))
      (setf temp (dsqrt par))
      (fdo (j 1 (+ j 1))
           ((> j n) nil)
           (tagbody
             (fset (fref wa1 (j) ((1 n))) (* temp (fref diag (j) ((1 n)))))
            label160))
      (multiple-value-bind
          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
          (qrsolv n r ldr ipvt wa1 qtb x sdiag wa2)
        (declare (ignore var-1 var-3 var-4 var-5 var-6 var-7 var-8))
        (when var-0 (setf n var-0))
        (when var-2 (setf ldr var-2)))
      (fdo (j 1 (+ j 1))
           ((> j n) nil)
           (tagbody
             (fset (fref wa2 (j) ((1 n)))
                   (* (fref diag (j) ((1 n))) (fref x (j) ((1 n)))))
            label170))
      (setf dxnorm
              (coerce
               (multiple-value-bind
                   (ret-val var-0 var-1)
                   (enorm n wa2)
                 (declare (ignore var-1))
                 (when var-0 (setf n var-0))
                 ret-val)
               'double-float))
      (setf temp fp)
      (setf fp (- dxnorm delta))
      '""
      '"        if the function is small enough, accept the current value"
      '"        of par. also test for the exceptional cases where parl"
      '"        is zero or the number of iterations has reached 10."
      '""
      (if
       (or (<= (dabs fp) (* p1 delta))
           (and (= parl zero) (<= fp temp) (< temp zero))
           (= iter 10))
       (go label220))
      '""
      '"        compute the newton correction."
      '""
      (fdo (j 1 (+ j 1))
           ((> j n) nil)
           (tagbody
             (setf l (fref ipvt (j) ((1 n))))
             (fset (fref wa1 (j) ((1 n)))
                   (* (fref diag (l) ((1 n)))
                      (/ (fref wa2 (l) ((1 n))) dxnorm)))
            label180))
      (fdo (j 1 (+ j 1))
           ((> j n) nil)
           (tagbody
             (fset (fref wa1 (j) ((1 n)))
                   (/ (fref wa1 (j) ((1 n))) (fref sdiag (j) ((1 n)))))
             (setf temp (fref wa1 (j) ((1 n))))
             (setf jp1 (+ j 1))
             (if (< n jp1) (go label200))
             (fdo (i jp1 (+ i 1))
                  ((> i n) nil)
                  (tagbody
                    (fset (fref wa1 (i) ((1 n)))
                          (- (fref wa1 (i) ((1 n)))
                             (* (fref r (i j) ((1 ldr) (1 n))) temp)))
                   label190))
            label200
            label210))
      (setf temp
              (coerce
               (multiple-value-bind
                   (ret-val var-0 var-1)
                   (enorm n wa1)
                 (declare (ignore var-1))
                 (when var-0 (setf n var-0))
                 ret-val)
               'double-float))
      (setf parc (/ (/ (/ fp delta) temp) temp))
      '""
      '"        depending on the sign of the function, update parl or paru."
      '""
      (if (> fp zero) (setf parl (dmax1 parl par)))
      (if (< fp zero) (setf paru (dmin1 paru par)))
      '""
      '"        compute an improved estimate for par."
      '""
      (setf par (dmax1 parl (+ par parc)))
      '""
      '"        end of an iteration."
      '""
      (go label150)
     label220
      '""
      '"     termination."
      '""
      (if (= iter 0) (setf par zero))
      (go end_label)
      '""
      '"     last card of subroutine lmpar."
      '""
     end_label
      (return (values n r ldr ipvt diag qtb delta par x sdiag wa1 wa2)))))

