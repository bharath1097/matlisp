;;; Compiled by f2cl version 2.0 beta on 2002/01/07 at 17:53:57
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls nil)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t))

(in-package "MINPACK")



(use-package :f2cl)

(let ((one 1.0d0)
      (p1 0.1d0)
      (p5 0.5d0)
      (p25 0.25d0)
      (p75 0.75d0)
      (p0001 1.0d-4)
      (zero 0.0d0))
  (declare (type double-float zero p0001 p75 p25 p5 p1 one))
  (defun lmdif
         (fcn m n x fvec ftol xtol gtol maxfev epsfcn diag mode factor nprint
          info nfev fjac ldfjac ipvt qtf wa1 wa2 wa3 wa4)
    (declare (type double-float ftol xtol gtol epsfcn factor)
             (type integer4 m n maxfev mode nprint info nfev ldfjac)
             (type (array integer4 (*)) ipvt)
             (type (array double-float (*)) x fvec diag fjac qtf wa1 wa2 wa3
              wa4)
             (type
              (function
               (integer4 integer4 array-double-float array-double-float
                integer4)
               (values &rest t))
              fcn))
    (prog ((actred 0.0d0) (delta 0.0d0) (dirder 0.0d0) (epsmch 0.0d0)
           (fnorm 0.0d0) (fnorm1 0.0d0) (gnorm 0.0d0) (par 0.0d0) (pnorm 0.0d0)
           (prered 0.0d0) (ratio 0.0d0) (sum 0.0d0) (temp 0.0d0) (temp1 0.0d0)
           (temp2 0.0d0) (xnorm 0.0d0) (i 0) (iflag 0) (iter 0) (j 0) (l 0))
      (declare (type integer4 l j iter iflag i)
               (type double-float xnorm temp2 temp1 temp sum ratio prered pnorm
                par gnorm fnorm1 fnorm epsmch dirder delta actred))
      (declare
       (ftype (function (integer4) (values double-float &rest t)) dpmpar))
      (declare
       (ftype
        (function (integer4 array-double-float) (values double-float &rest t))
        enorm))
      (declare
       (ftype
        (function
         (single-float integer4 integer4 array-double-float array-double-float
          array-double-float integer4 integer4 double-float array-double-float)
         (values &rest t))
        fdjac2))
      (declare (ftype (function (integer4 integer4) (values integer4)) mod))
      (declare
       (ftype
        (function
         (integer4 integer4 array-double-float integer4 logical array-integer4
          integer4 array-double-float array-double-float array-double-float)
         (values &rest t))
        qrfac))
      (declare (ftype (function (double-float) (values double-float)) dabs))
      (declare
       (ftype
        (function
         ((or array-double-float double-float)
          (or array-double-float double-float))
         (values double-float))
        dmax1))
      (declare
       (ftype
        (function
         (integer4 array-double-float integer4 array-integer4
          array-double-float array-double-float double-float double-float
          array-double-float array-double-float array-double-float
          array-double-float)
         (values &rest t))
        lmpar))
      (declare
       (ftype (function (double-float double-float) (values double-float))
        dmin1))
      (declare (ftype (function (double-float) (values double-float)) dsqrt))
      '"     **********"
      '""
      '"     subroutine lmdif"
      '""
      '"     the purpose of lmdif is to minimize the sum of the squares of"
      '"     m nonlinear functions in n variables by a modification of"
      '"     the levenberg-marquardt algorithm. the user must provide a"
      '"     subroutine which calculates the functions. the jacobian is"
      '"     then calculated by a forward-difference approximation."
      '""
      '"     the subroutine statement is"
      '""
      '"       subroutine lmdif(fcn,m,n,x,fvec,ftol,xtol,gtol,maxfev,epsfcn,"
      '"                        diag,mode,factor,nprint,info,nfev,fjac,"
      '"                        ldfjac,ipvt,qtf,wa1,wa2,wa3,wa4)"
      '""
      '"     where"
      '""
      '"       fcn is the name of the user-supplied subroutine which"
      '"         calculates the functions. fcn must be declared"
      '"         in an external statement in the user calling"
      '"         program, and should be written as follows."
      '""
      '"         subroutine fcn(m,n,x,fvec,iflag)"
      '"         integer m,n,iflag"
      '"         double precision x(n),fvec(m)"
      '"         ----------"
      '"         calculate the functions at x and"
      '"         return this vector in fvec."
      '"         ----------"
      '"         return"
      '"         end"
      '""
      '"         the value of iflag should not be changed by fcn unless"
      '"         the user wants to terminate execution of lmdif."
      '"         in this case set iflag to a negative integer."
      '""
      '"       m is a positive integer input variable set to the number"
      '"         of functions."
      '""
      '"       n is a positive integer input variable set to the number"
      '"         of variables. n must not exceed m."
      '""
      '"       x is an array of length n. on input x must contain"
      '"         an initial estimate of the solution vector. on output x"
      '"         contains the final estimate of the solution vector."
      '""
      '"       fvec is an output array of length m which contains"
      '"         the functions evaluated at the output x."
      '""
      '"       ftol is a nonnegative input variable. termination"
      '"         occurs when both the actual and predicted relative"
      '"         reductions in the sum of squares are at most ftol."
      '"         therefore, ftol measures the relative error desired"
      '"         in the sum of squares."
      '""
      '"       xtol is a nonnegative input variable. termination"
      '"         occurs when the relative error between two consecutive"
      '"         iterates is at most xtol. therefore, xtol measures the"
      '"         relative error desired in the approximate solution."
      '""
      '"       gtol is a nonnegative input variable. termination"
      '"         occurs when the cosine of the angle between fvec and"
      '"         any column of the jacobian is at most gtol in absolute"
      '"         value. therefore, gtol measures the orthogonality"
      '"         desired between the function vector and the columns"
      '"         of the jacobian."
      '""
      '"       maxfev is a positive integer input variable. termination"
      '"         occurs when the number of calls to fcn is at least"
      '"         maxfev by the end of an iteration."
      '""
      '"       epsfcn is an input variable used in determining a suitable"
      '"         step length for the forward-difference approximation. this"
      '"         approximation assumes that the relative errors in the"
      '"         functions are of the order of epsfcn. if epsfcn is less"
      '"         than the machine precision, it is assumed that the relative"
      '"         errors in the functions are of the order of the machine"
      '"         precision."
      '""
      '"       diag is an array of length n. if mode = 1 (see"
      '"         below), diag is internally set. if mode = 2, diag"
      '"         must contain positive entries that serve as"
      '"         multiplicative scale factors for the variables."
      '""
      '"       mode is an integer input variable. if mode = 1, the"
      '"         variables will be scaled internally. if mode = 2,"
      '"         the scaling is specified by the input diag. other"
      '"         values of mode are equivalent to mode = 1."
      '""
      '"       factor is a positive input variable used in determining the"
      '"         initial step bound. this bound is set to the product of"
      '"         factor and the euclidean norm of diag*x if nonzero, or else"
      '"         to factor itself. in most cases factor should lie in the"
      '"         interval (.1,100.). 100. is a generally recommended value."
      '""
      '"       nprint is an integer input variable that enables controlled"
      '"         printing of iterates if it is positive. in this case,"
      '"         fcn is called with iflag = 0 at the beginning of the first"
      '"         iteration and every nprint iterations thereafter and"
      '"         immediately prior to return, with x and fvec available"
      '"         for printing. if nprint is not positive, no special calls"
      '"         of fcn with iflag = 0 are made."
      '""
      '"       info is an integer output variable. if the user has"
      '"         terminated execution, info is set to the (negative)"
      '"         value of iflag. see description of fcn. otherwise,"
      '"         info is set as follows."
      '""
      '"         info = 0  improper input parameters."
      '""
      '"         info = 1  both actual and predicted relative reductions"
      '"                   in the sum of squares are at most ftol."
      '""
      '"         info = 2  relative error between two consecutive iterates"
      '"                   is at most xtol."
      '""
      '"         info = 3  conditions for info = 1 and info = 2 both hold."
      '""
      '"         info = 4  the cosine of the angle between fvec and any"
      '"                   column of the jacobian is at most gtol in"
      '"                   absolute value."
      '""
      '"         info = 5  number of calls to fcn has reached or"
      '"                   exceeded maxfev."
      '""
      '"         info = 6  ftol is too small. no further reduction in"
      '"                   the sum of squares is possible."
      '""
      '"         info = 7  xtol is too small. no further improvement in"
      '"                   the approximate solution x is possible."
      '""
      '"         info = 8  gtol is too small. fvec is orthogonal to the"
      '"                   columns of the jacobian to machine precision."
      '""
      '"       nfev is an integer output variable set to the number of"
      '"         calls to fcn."
      '""
      '"       fjac is an output m by n array. the upper n by n submatrix"
      '"         of fjac contains an upper triangular matrix r with"
      '"         diagonal elements of nonincreasing magnitude such that"
      '""
      '"                t     t           t"
      '"               p *(jac *jac)*p = r *r,"
      '""
      '"         where p is a permutation matrix and jac is the final"
      '"         calculated jacobian. column j of p is column ipvt(j)"
      '"         (see below) of the identity matrix. the lower trapezoidal"
      '"         part of fjac contains information generated during"
      '"         the computation of r."
      '""
      '"       ldfjac is a positive integer input variable not less than m"
      '"         which specifies the leading dimension of the array fjac."
      '""
      '"       ipvt is an integer output array of length n. ipvt"
      '"         defines a permutation matrix p such that jac*p = q*r,"
      '"         where jac is the final calculated jacobian, q is"
      '"         orthogonal (not stored), and r is upper triangular"
      '"         with diagonal elements of nonincreasing magnitude."
      '"         column j of p is column ipvt(j) of the identity matrix."
      '""
      '"       qtf is an output array of length n which contains"
      '"         the first n elements of the vector (q transpose)*fvec."
      '""
      '"       wa1, wa2, and wa3 are work arrays of length n."
      '""
      '"       wa4 is a work array of length m."
      '""
      '"     subprograms called"
      '""
      '"       user-supplied ...... fcn"
      '""
      '"       minpack-supplied ... dpmpar,enorm,fdjac2,lmpar,qrfac"
      '""
      '"       fortran-supplied ... dabs,dmax1,dmin1,dsqrt,mod"
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
      (setf info 0)
      (setf iflag 0)
      (setf nfev 0)
      '""
      '"     check the input parameters for errors."
      '""
      (if
       (or (<= n 0)
           (< m n)
           (< ldfjac m)
           (< ftol zero)
           (< xtol zero)
           (< gtol zero)
           (<= maxfev 0)
           (<= factor zero))
       (go label300))
      (if (/= mode 2) (go label20))
      (fdo (j 1 (+ j 1))
           ((> j n) nil)
           (tagbody
             (if (<= (fref diag (j) ((1 n))) zero) (go label300))
            label10))
     label20
      '""
      '"     evaluate the function at the starting point"
      '"     and calculate its norm."
      '""
      (setf iflag 1)
      (multiple-value-bind
          (var-0 var-1 var-2 var-3 var-4)
          (funcall fcn m n x fvec iflag)
        (declare (ignore var-2 var-3))
        (when var-0 (setf m var-0))
        (when var-1 (setf n var-1))
        (when var-4 (setf iflag var-4)))
      (setf nfev 1)
      (if (< iflag 0) (go label300))
      (setf fnorm
              (coerce
               (multiple-value-bind
                   (ret-val var-0 var-1)
                   (enorm m fvec)
                 (declare (ignore var-1))
                 (when var-0 (setf m var-0))
                 ret-val)
               'double-float))
      '""
      '"     initialize levenberg-marquardt parameter and iteration counter."
      '""
      (setf par zero)
      (setf iter 1)
      '""
      '"     beginning of the outer loop."
      '""
     label30
      '""
      '"        calculate the jacobian matrix."
      '""
      (setf iflag 2)
      (multiple-value-bind
          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
          (fdjac2 fcn m n x fvec fjac ldfjac iflag epsfcn wa4)
        (declare (ignore var-0 var-3 var-4 var-5 var-9))
        (when var-1 (setf m var-1))
        (when var-2 (setf n var-2))
        (when var-6 (setf ldfjac var-6))
        (when var-7 (setf iflag var-7))
        (when var-8 (setf epsfcn var-8)))
      (setf nfev (+ nfev n))
      (if (< iflag 0) (go label300))
      '""
      '"        if requested, call fcn to enable printing of iterates."
      '""
      (if (<= nprint 0) (go label40))
      (setf iflag 0)
      (if (= (mod (- iter 1) nprint) 0)
          (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4)
              (funcall fcn m n x fvec iflag)
            (declare (ignore var-2 var-3))
            (when var-0 (setf m var-0))
            (when var-1 (setf n var-1))
            (when var-4 (setf iflag var-4))))
      (if (< iflag 0) (go label300))
     label40
      '""
      '"        compute the qr factorization of the jacobian."
      '""
      (multiple-value-bind
          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
          (qrfac m n fjac ldfjac %true% ipvt n wa1 wa2 wa3)
        (declare (ignore var-2 var-4 var-5 var-7 var-8 var-9))
        (when var-0 (setf m var-0))
        (when var-1 (setf n var-1))
        (when var-3 (setf ldfjac var-3))
        (when var-6 (setf n var-6)))
      '""
      '"        on the first iteration and if mode is 1, scale according"
      '"        to the norms of the columns of the initial jacobian."
      '""
      (if (/= iter 1) (go label80))
      (if (= mode 2) (go label60))
      (fdo (j 1 (+ j 1))
           ((> j n) nil)
           (tagbody
             (fset (fref diag (j) ((1 n))) (fref wa2 (j) ((1 n))))
             (if (= (fref wa2 (j) ((1 n))) zero)
                 (fset (fref diag (j) ((1 n))) one))
            label50))
     label60
      '""
      '"        on the first iteration, calculate the norm of the scaled x"
      '"        and initialize the step bound delta."
      '""
      (fdo (j 1 (+ j 1))
           ((> j n) nil)
           (tagbody
             (fset (fref wa3 (j) ((1 n)))
                   (* (fref diag (j) ((1 n))) (fref x (j) ((1 n)))))
            label70))
      (setf xnorm
              (coerce
               (multiple-value-bind
                   (ret-val var-0 var-1)
                   (enorm n wa3)
                 (declare (ignore var-1))
                 (when var-0 (setf n var-0))
                 ret-val)
               'double-float))
      (setf delta (* factor xnorm))
      (if (= delta zero) (setf delta factor))
     label80
      '""
      '"        form (q transpose)*fvec and store the first n components in"
      '"        qtf."
      '""
      (fdo (i 1 (+ i 1))
           ((> i m) nil)
           (tagbody
             (fset (fref wa4 (i) ((1 m))) (fref fvec (i) ((1 m))))
            label90))
      (fdo (j 1 (+ j 1))
           ((> j n) nil)
           (tagbody
             (if (= (fref fjac (j j) ((1 ldfjac) (1 n))) zero) (go label120))
             (setf sum zero)
             (fdo (i j (+ i 1))
                  ((> i m) nil)
                  (tagbody
                    (setf sum
                            (+ sum
                               (* (fref fjac (i j) ((1 ldfjac) (1 n)))
                                  (fref wa4 (i) ((1 m))))))
                   label100))
             (setf temp (/ (- sum) (fref fjac (j j) ((1 ldfjac) (1 n)))))
             (fdo (i j (+ i 1))
                  ((> i m) nil)
                  (tagbody
                    (fset (fref wa4 (i) ((1 m)))
                          (+ (fref wa4 (i) ((1 m)))
                             (* (fref fjac (i j) ((1 ldfjac) (1 n))) temp)))
                   label110))
            label120
             (fset (fref fjac (j j) ((1 ldfjac) (1 n))) (fref wa1 (j) ((1 n))))
             (fset (fref qtf (j) ((1 n))) (fref wa4 (j) ((1 m))))
            label130))
      '""
      '"        compute the norm of the scaled gradient."
      '""
      (setf gnorm zero)
      (if (= fnorm zero) (go label170))
      (fdo (j 1 (+ j 1))
           ((> j n) nil)
           (tagbody
             (setf l (fref ipvt (j) ((1 n))))
             (if (= (fref wa2 (l) ((1 n))) zero) (go label150))
             (setf sum zero)
             (fdo (i 1 (+ i 1))
                  ((> i j) nil)
                  (tagbody
                    (setf sum
                            (+ sum
                               (* (fref fjac (i j) ((1 ldfjac) (1 n)))
                                  (/ (fref qtf (i) ((1 n))) fnorm))))
                   label140))
             (setf gnorm (dmax1 gnorm (dabs (/ sum (fref wa2 (l) ((1 n)))))))
            label150
            label160))
     label170
      '""
      '"        test for convergence of the gradient norm."
      '""
      (if (<= gnorm gtol) (setf info 4))
      (if (/= info 0) (go label300))
      '""
      '"        rescale if necessary."
      '""
      (if (= mode 2) (go label190))
      (fdo (j 1 (+ j 1))
           ((> j n) nil)
           (tagbody
             (fset (fref diag (j) ((1 n)))
                   (dmax1 (fref diag (j) ((1 n))) (fref wa2 (j) ((1 n)))))
            label180))
     label190
      '""
      '"        beginning of the inner loop."
      '""
     label200
      '""
      '"           determine the levenberg-marquardt parameter."
      '""
      (multiple-value-bind
          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10
           var-11)
          (lmpar n fjac ldfjac ipvt diag qtf delta par wa1 wa2 wa3 wa4)
        (declare (ignore var-1 var-3 var-4 var-5 var-8 var-9 var-10 var-11))
        (when var-0 (setf n var-0))
        (when var-2 (setf ldfjac var-2))
        (when var-6 (setf delta var-6))
        (when var-7 (setf par var-7)))
      '""
      '"           store the direction p and x + p. calculate the norm of p."
      '""
      (fdo (j 1 (+ j 1))
           ((> j n) nil)
           (tagbody
             (fset (fref wa1 (j) ((1 n))) (- (fref wa1 (j) ((1 n)))))
             (fset (fref wa2 (j) ((1 n)))
                   (+ (fref x (j) ((1 n))) (fref wa1 (j) ((1 n)))))
             (fset (fref wa3 (j) ((1 n)))
                   (* (fref diag (j) ((1 n))) (fref wa1 (j) ((1 n)))))
            label210))
      (setf pnorm
              (coerce
               (multiple-value-bind
                   (ret-val var-0 var-1)
                   (enorm n wa3)
                 (declare (ignore var-1))
                 (when var-0 (setf n var-0))
                 ret-val)
               'double-float))
      '""
      '"           on the first iteration, adjust the initial step bound."
      '""
      (if (= iter 1) (setf delta (dmin1 delta pnorm)))
      '""
      '"           evaluate the function at x + p and calculate its norm."
      '""
      (setf iflag 1)
      (multiple-value-bind
          (var-0 var-1 var-2 var-3 var-4)
          (funcall fcn m n wa2 wa4 iflag)
        (declare (ignore var-2 var-3))
        (when var-0 (setf m var-0))
        (when var-1 (setf n var-1))
        (when var-4 (setf iflag var-4)))
      (setf nfev (+ nfev 1))
      (if (< iflag 0) (go label300))
      (setf fnorm1
              (coerce
               (multiple-value-bind
                   (ret-val var-0 var-1)
                   (enorm m wa4)
                 (declare (ignore var-1))
                 (when var-0 (setf m var-0))
                 ret-val)
               'double-float))
      '""
      '"           compute the scaled actual reduction."
      '""
      (setf actred (- one))
      (if (< (* p1 fnorm1) fnorm)
          (setf actred (- one (expt (/ fnorm1 fnorm) 2))))
      '""
      '"           compute the scaled predicted reduction and"
      '"           the scaled directional derivative."
      '""
      (fdo (j 1 (+ j 1))
           ((> j n) nil)
           (tagbody
             (fset (fref wa3 (j) ((1 n))) zero)
             (setf l (fref ipvt (j) ((1 n))))
             (setf temp (fref wa1 (l) ((1 n))))
             (fdo (i 1 (+ i 1))
                  ((> i j) nil)
                  (tagbody
                    (fset (fref wa3 (i) ((1 n)))
                          (+ (fref wa3 (i) ((1 n)))
                             (* (fref fjac (i j) ((1 ldfjac) (1 n))) temp)))
                   label220))
            label230))
      (setf temp1
              (/
               (multiple-value-bind
                   (ret-val var-0 var-1)
                   (enorm n wa3)
                 (declare (ignore var-1))
                 (when var-0 (setf n var-0))
                 ret-val)
               fnorm))
      (setf temp2 (/ (* (dsqrt par) pnorm) fnorm))
      (setf prered (+ (expt temp1 2) (/ (expt temp2 2) p5)))
      (setf dirder (- (+ (expt temp1 2) (expt temp2 2))))
      '""
      '"           compute the ratio of the actual to the predicted"
      '"           reduction."
      '""
      (setf ratio zero)
      (if (/= prered zero) (setf ratio (/ actred prered)))
      '""
      '"           update the step bound."
      '""
      (if (> ratio p25) (go label240))
      (if (>= actred zero) (setf temp p5))
      (if (< actred zero)
          (setf temp (/ (* p5 dirder) (+ dirder (* p5 actred)))))
      (if (or (>= (* p1 fnorm1) fnorm) (< temp p1)) (setf temp p1))
      (setf delta (* temp (dmin1 delta (/ pnorm p1))))
      (setf par (/ par temp))
      (go label260)
     label240
      (if (and (/= par zero) (< ratio p75)) (go label250))
      (setf delta (/ pnorm p5))
      (setf par (* p5 par))
     label250
     label260
      '""
      '"           test for successful iteration."
      '""
      (if (< ratio p0001) (go label290))
      '""
      '"           successful iteration. update x, fvec, and their norms."
      '""
      (fdo (j 1 (+ j 1))
           ((> j n) nil)
           (tagbody
             (fset (fref x (j) ((1 n))) (fref wa2 (j) ((1 n))))
             (fset (fref wa2 (j) ((1 n)))
                   (* (fref diag (j) ((1 n))) (fref x (j) ((1 n)))))
            label270))
      (fdo (i 1 (+ i 1))
           ((> i m) nil)
           (tagbody
             (fset (fref fvec (i) ((1 m))) (fref wa4 (i) ((1 m))))
            label280))
      (setf xnorm
              (coerce
               (multiple-value-bind
                   (ret-val var-0 var-1)
                   (enorm n wa2)
                 (declare (ignore var-1))
                 (when var-0 (setf n var-0))
                 ret-val)
               'double-float))
      (setf fnorm fnorm1)
      (setf iter (+ iter 1))
     label290
      '""
      '"           tests for convergence."
      '""
      (if (and (<= (dabs actred) ftol) (<= prered ftol) (<= (* p5 ratio) one))
          (setf info 1))
      (if (<= delta (* xtol xnorm)) (setf info 2))
      (if
       (and (<= (dabs actred) ftol)
            (<= prered ftol)
            (<= (* p5 ratio) one)
            (= info 2))
       (setf info 3))
      (if (/= info 0) (go label300))
      '""
      '"           tests for termination and stringent tolerances."
      '""
      (if (>= nfev maxfev) (setf info 5))
      (if
       (and (<= (dabs actred) epsmch) (<= prered epsmch) (<= (* p5 ratio) one))
       (setf info 6))
      (if (<= delta (* epsmch xnorm)) (setf info 7))
      (if (<= gnorm epsmch) (setf info 8))
      (if (/= info 0) (go label300))
      '""
      '"           end of the inner loop. repeat if iteration unsuccessful."
      '""
      (if (< ratio p0001) (go label200))
      '""
      '"        end of the outer loop."
      '""
      (go label30)
     label300
      '""
      '"     termination, either normal or user imposed."
      '""
      (if (< iflag 0) (setf info iflag))
      (setf iflag 0)
      (if (> nprint 0)
          (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4)
              (funcall fcn m n x fvec iflag)
            (declare (ignore var-2 var-3))
            (when var-0 (setf m var-0))
            (when var-1 (setf n var-1))
            (when var-4 (setf iflag var-4))))
      (go end_label)
      '""
      '"     last card of subroutine lmdif."
      '""
     end_label
      (return
       (values fcn
               m
               n
               x
               fvec
               ftol
               xtol
               gtol
               maxfev
               epsfcn
               diag
               mode
               factor
               nprint
               info
               nfev
               fjac
               ldfjac
               ipvt
               qtf
               wa1
               wa2
               wa3
               wa4)))))

