;;; Compiled by f2cl version 2.0 beta on 2002/01/07 at 17:54:14
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls nil)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t))

(in-package "MINPACK")



(use-package :f2cl)

(let ((one 1.0d0)
      (p1 0.1d0)
      (p5 0.5d0)
      (p001 0.001d0)
      (p0001 1.0d-4)
      (zero 0.0d0))
  (declare (type double-float zero p0001 p001 p5 p1 one))
  (defun hybrj
         (fcn n x fvec fjac ldfjac xtol maxfev diag mode factor nprint info
          nfev njev r lr qtf wa1 wa2 wa3 wa4)
    (declare (type double-float factor xtol)
             (type (array double-float (*)) wa4 wa3 wa2 wa1 qtf r diag fjac
              fvec x)
             (type integer4 lr njev nfev info nprint mode maxfev ldfjac n)
             (type single-float fcn)
             (type
              (function
               (integer4 array-double-float array-double-float
                array-double-float integer4 integer4)
               (values &rest t))
              fcn))
    (prog ((actred 0.0d0) (delta 0.0d0) (epsmch 0.0d0) (fnorm 0.0d0)
           (fnorm1 0.0d0) (pnorm 0.0d0) (prered 0.0d0) (ratio 0.0d0)
           (sum 0.0d0) (temp 0.0d0) (xnorm 0.0d0) (jeval nil) (sing nil)
           (iwa (make-array 1 :element-type 'integer4)) (i 0) (iflag 0)
           (iter 0) (j 0) (jm1 0) (l 0) (ncfail 0) (ncsuc 0) (nslow1 0)
           (nslow2 0))
      (declare (type integer4 nslow2 nslow1 ncsuc ncfail l jm1 j iter iflag i)
               (type (array integer4 (1)) iwa)
               (type logical sing jeval)
               (type double-float xnorm temp sum ratio prered pnorm fnorm1
                fnorm epsmch delta actred))
      (declare
       (ftype (function (integer4) (values double-float &rest t)) dpmpar))
      (declare
       (ftype
        (function (integer4 array-double-float) (values double-float &rest t))
        enorm))
      (declare
       (ftype
        (function
         (integer4 integer4 array-double-float integer4 logical array-integer4
          integer4 array-double-float array-double-float array-double-float)
         (values &rest t))
        qrfac))
      (declare
       (ftype
        (function
         (integer4 integer4 array-double-float integer4 array-double-float)
         (values &rest t))
        qform))
      (declare
       (ftype
        (function
         ((or double-float array-double-float)
          (or double-float array-double-float))
         (values double-float))
        dmax1))
      (declare (ftype (function (integer4 integer4) (values integer4)) mod))
      (declare
       (ftype
        (function
         (integer4 array-double-float integer4 array-double-float
          array-double-float double-float array-double-float array-double-float
          array-double-float)
         (values &rest t))
        dogleg))
      (declare
       (ftype (function (double-float double-float) (values double-float))
        dmin1))
      (declare (ftype (function (double-float) (values double-float)) dabs))
      (declare
       (ftype
        (function
         (integer4 integer4 array-double-float integer4 array-double-float
          array-double-float array-double-float logical)
         (values &rest t))
        r1updt))
      (declare
       (ftype
        (function
         (integer4 integer4 array-double-float integer4 array-double-float
          array-double-float)
         (values &rest t))
        r1mpyq))
      '"     **********"
      '""
      '"     subroutine hybrj"
      '""
      '"     the purpose of hybrj is to find a zero of a system of"
      '"     n nonlinear functions in n variables by a modification"
      '"     of the powell hybrid method. the user must provide a"
      '"     subroutine which calculates the functions and the jacobian."
      '""
      '"     the subroutine statement is"
      '""
      '"       subroutine hybrj(fcn,n,x,fvec,fjac,ldfjac,xtol,maxfev,diag,"
      '"                        mode,factor,nprint,info,nfev,njev,r,lr,qtf,"
      '"                        wa1,wa2,wa3,wa4)"
      '""
      '"     where"
      '""
      '"       fcn is the name of the user-supplied subroutine which"
      '"         calculates the functions and the jacobian. fcn must"
      '"         be declared in an external statement in the user"
      '"         calling program, and should be written as follows."
      '""
      '"         subroutine fcn(n,x,fvec,fjac,ldfjac,iflag)"
      '"         integer n,ldfjac,iflag"
      '"         double precision x(n),fvec(n),fjac(ldfjac,n)"
      '"         ----------"
      '"         if iflag = 1 calculate the functions at x and"
      '"         return this vector in fvec. do not alter fjac."
      '"         if iflag = 2 calculate the jacobian at x and"
      '"         return this matrix in fjac. do not alter fvec."
      '"         ---------"
      '"         return"
      '"         end"
      '""
      '"         the value of iflag should not be changed by fcn unless"
      '"         the user wants to terminate execution of hybrj."
      '"         in this case set iflag to a negative integer."
      '""
      '"       n is a positive integer input variable set to the number"
      '"         of functions and variables."
      '""
      '"       x is an array of length n. on input x must contain"
      '"         an initial estimate of the solution vector. on output x"
      '"         contains the final estimate of the solution vector."
      '""
      '"       fvec is an output array of length n which contains"
      '"         the functions evaluated at the output x."
      '""
      '"       fjac is an output n by n array which contains the"
      '"         orthogonal matrix q produced by the qr factorization"
      '"         of the final approximate jacobian."
      '""
      '"       ldfjac is a positive integer input variable not less than n"
      '"         which specifies the leading dimension of the array fjac."
      '""
      '"       xtol is a nonnegative input variable. termination"
      '"         occurs when the relative error between two consecutive"
      '"         iterates is at most xtol."
      '""
      '"       maxfev is a positive integer input variable. termination"
      '"         occurs when the number of calls to fcn with iflag = 1"
      '"         has reached maxfev."
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
      '"         for printing. fvec and fjac should not be altered."
      '"         if nprint is not positive, no special calls of fcn"
      '"         with iflag = 0 are made."
      '""
      '"       info is an integer output variable. if the user has"
      '"         terminated execution, info is set to the (negative)"
      '"         value of iflag. see description of fcn. otherwise,"
      '"         info is set as follows."
      '""
      '"         info = 0   improper input parameters."
      '""
      '"         info = 1   relative error between two consecutive iterates"
      '"                    is at most xtol."
      '""
      '"         info = 2   number of calls to fcn with iflag = 1 has"
      '"                    reached maxfev."
      '""
      '"         info = 3   xtol is too small. no further improvement in"
      '"                    the approximate solution x is possible."
      '""
      '"         info = 4   iteration is not making good progress, as"
      '"                    measured by the improvement from the last"
      '"                    five jacobian evaluations."
      '""
      '"         info = 5   iteration is not making good progress, as"
      '"                    measured by the improvement from the last"
      '"                    ten iterations."
      '""
      '"       nfev is an integer output variable set to the number of"
      '"         calls to fcn with iflag = 1."
      '""
      '"       njev is an integer output variable set to the number of"
      '"         calls to fcn with iflag = 2."
      '""
      '"       r is an output array of length lr which contains the"
      '"         upper triangular matrix produced by the qr factorization"
      '"         of the final approximate jacobian, stored rowwise."
      '""
      '"       lr is a positive integer input variable not less than"
      '"         (n*(n+1))/2."
      '""
      '"       qtf is an output array of length n which contains"
      '"         the vector (q transpose)*fvec."
      '""
      '"       wa1, wa2, wa3, and wa4 are work arrays of length n."
      '""
      '"     subprograms called"
      '""
      '"       user-supplied ...... fcn"
      '""
      '"       minpack-supplied ... dogleg,dpmpar,enorm,"
      '"                            qform,qrfac,r1mpyq,r1updt"
      '""
      '"       fortran-supplied ... dabs,dmax1,dmin1,mod"
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
      (setf njev 0)
      '""
      '"     check the input parameters for errors."
      '""
      (if
       (or (<= n 0)
           (< ldfjac n)
           (< xtol zero)
           (<= maxfev 0)
           (<= factor zero)
           (< lr (truncate (* n (+ n 1)) 2)))
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
          (var-0 var-1 var-2 var-3 var-4 var-5)
          (funcall fcn n x fvec fjac ldfjac iflag)
        (declare (ignore var-1 var-2 var-3))
        (when var-0 (setf n var-0))
        (when var-4 (setf ldfjac var-4))
        (when var-5 (setf iflag var-5)))
      (setf nfev 1)
      (if (< iflag 0) (go label300))
      (setf fnorm
              (coerce
               (multiple-value-bind
                   (ret-val var-0 var-1)
                   (enorm n fvec)
                 (declare (ignore var-1))
                 (when var-0 (setf n var-0))
                 ret-val)
               'double-float))
      '""
      '"     initialize iteration counter and monitors."
      '""
      (setf iter 1)
      (setf ncsuc 0)
      (setf ncfail 0)
      (setf nslow1 0)
      (setf nslow2 0)
      '""
      '"     beginning of the outer loop."
      '""
     label30
      (setf jeval %true%)
      '""
      '"        calculate the jacobian matrix."
      '""
      (setf iflag 2)
      (multiple-value-bind
          (var-0 var-1 var-2 var-3 var-4 var-5)
          (funcall fcn n x fvec fjac ldfjac iflag)
        (declare (ignore var-1 var-2 var-3))
        (when var-0 (setf n var-0))
        (when var-4 (setf ldfjac var-4))
        (when var-5 (setf iflag var-5)))
      (setf njev (+ njev 1))
      (if (< iflag 0) (go label300))
      '""
      '"        compute the qr factorization of the jacobian."
      '""
      (multiple-value-bind
          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
          (qrfac n n fjac ldfjac %false% iwa 1 wa1 wa2 wa3)
        (declare (ignore var-2 var-4 var-5 var-6 var-7 var-8 var-9))
        (when var-0 (setf n var-0))
        (when var-1 (setf n var-1))
        (when var-3 (setf ldfjac var-3)))
      '""
      '"        on the first iteration and if mode is 1, scale according"
      '"        to the norms of the columns of the initial jacobian."
      '""
      (if (/= iter 1) (go label70))
      (if (= mode 2) (go label50))
      (fdo (j 1 (+ j 1))
           ((> j n) nil)
           (tagbody
             (fset (fref diag (j) ((1 n))) (fref wa2 (j) ((1 n))))
             (if (= (fref wa2 (j) ((1 n))) zero)
                 (fset (fref diag (j) ((1 n))) one))
            label40))
     label50
      '""
      '"        on the first iteration, calculate the norm of the scaled x"
      '"        and initialize the step bound delta."
      '""
      (fdo (j 1 (+ j 1))
           ((> j n) nil)
           (tagbody
             (fset (fref wa3 (j) ((1 n)))
                   (* (fref diag (j) ((1 n))) (fref x (j) ((1 n)))))
            label60))
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
     label70
      '""
      '"        form (q transpose)*fvec and store in qtf."
      '""
      (fdo (i 1 (+ i 1))
           ((> i n) nil)
           (tagbody
             (fset (fref qtf (i) ((1 n))) (fref fvec (i) ((1 n))))
            label80))
      (fdo (j 1 (+ j 1))
           ((> j n) nil)
           (tagbody
             (if (= (fref fjac (j j) ((1 ldfjac) (1 n))) zero) (go label110))
             (setf sum zero)
             (fdo (i j (+ i 1))
                  ((> i n) nil)
                  (tagbody
                    (setf sum
                            (+ sum
                               (* (fref fjac (i j) ((1 ldfjac) (1 n)))
                                  (fref qtf (i) ((1 n))))))
                   label90))
             (setf temp (/ (- sum) (fref fjac (j j) ((1 ldfjac) (1 n)))))
             (fdo (i j (+ i 1))
                  ((> i n) nil)
                  (tagbody
                    (fset (fref qtf (i) ((1 n)))
                          (+ (fref qtf (i) ((1 n)))
                             (* (fref fjac (i j) ((1 ldfjac) (1 n))) temp)))
                   label100))
            label110
            label120))
      '""
      '"        copy the triangular factor of the qr factorization into r."
      '""
      (setf sing %false%)
      (fdo (j 1 (+ j 1))
           ((> j n) nil)
           (tagbody
             (setf l j)
             (setf jm1 (- j 1))
             (if (< jm1 1) (go label140))
             (fdo (i 1 (+ i 1))
                  ((> i jm1) nil)
                  (tagbody
                    (fset (fref r (l) ((1 lr)))
                          (fref fjac (i j) ((1 ldfjac) (1 n))))
                    (setf l (- (+ l n) i))
                   label130))
            label140
             (fset (fref r (l) ((1 lr))) (fref wa1 (j) ((1 n))))
             (if (= (fref wa1 (j) ((1 n))) zero) (setf sing %true%))
            label150))
      '""
      '"        accumulate the orthogonal factor in fjac."
      '""
      (multiple-value-bind
          (var-0 var-1 var-2 var-3 var-4)
          (qform n n fjac ldfjac wa1)
        (declare (ignore var-2 var-4))
        (when var-0 (setf n var-0))
        (when var-1 (setf n var-1))
        (when var-3 (setf ldfjac var-3)))
      '""
      '"        rescale if necessary."
      '""
      (if (= mode 2) (go label170))
      (fdo (j 1 (+ j 1))
           ((> j n) nil)
           (tagbody
             (fset (fref diag (j) ((1 n)))
                   (dmax1 (fref diag (j) ((1 n))) (fref wa2 (j) ((1 n)))))
            label160))
     label170
      '""
      '"        beginning of the inner loop."
      '""
     label180
      '""
      '"           if requested, call fcn to enable printing of iterates."
      '""
      (if (<= nprint 0) (go label190))
      (setf iflag 0)
      (if (= (mod (- iter 1) nprint) 0)
          (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5)
              (funcall fcn n x fvec fjac ldfjac iflag)
            (declare (ignore var-1 var-2 var-3))
            (when var-0 (setf n var-0))
            (when var-4 (setf ldfjac var-4))
            (when var-5 (setf iflag var-5))))
      (if (< iflag 0) (go label300))
     label190
      '""
      '"           determine the direction p."
      '""
      (multiple-value-bind
          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
          (dogleg n r lr diag qtf delta wa1 wa2 wa3)
        (declare (ignore var-1 var-3 var-4 var-6 var-7 var-8))
        (when var-0 (setf n var-0))
        (when var-2 (setf lr var-2))
        (when var-5 (setf delta var-5)))
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
            label200))
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
          (var-0 var-1 var-2 var-3 var-4 var-5)
          (funcall fcn n wa2 wa4 fjac ldfjac iflag)
        (declare (ignore var-1 var-2 var-3))
        (when var-0 (setf n var-0))
        (when var-4 (setf ldfjac var-4))
        (when var-5 (setf iflag var-5)))
      (setf nfev (+ nfev 1))
      (if (< iflag 0) (go label300))
      (setf fnorm1
              (coerce
               (multiple-value-bind
                   (ret-val var-0 var-1)
                   (enorm n wa4)
                 (declare (ignore var-1))
                 (when var-0 (setf n var-0))
                 ret-val)
               'double-float))
      '""
      '"           compute the scaled actual reduction."
      '""
      (setf actred (- one))
      (if (< fnorm1 fnorm) (setf actred (- one (expt (/ fnorm1 fnorm) 2))))
      '""
      '"           compute the scaled predicted reduction."
      '""
      (setf l 1)
      (fdo (i 1 (+ i 1))
           ((> i n) nil)
           (tagbody
             (setf sum zero)
             (fdo (j i (+ j 1))
                  ((> j n) nil)
                  (tagbody
                    (setf sum
                            (+ sum
                               (* (fref r (l) ((1 lr)))
                                  (fref wa1 (j) ((1 n))))))
                    (setf l (+ l 1))
                   label210))
             (fset (fref wa3 (i) ((1 n))) (+ (fref qtf (i) ((1 n))) sum))
            label220))
      (setf temp
              (coerce
               (multiple-value-bind
                   (ret-val var-0 var-1)
                   (enorm n wa3)
                 (declare (ignore var-1))
                 (when var-0 (setf n var-0))
                 ret-val)
               'double-float))
      (setf prered zero)
      (if (< temp fnorm) (setf prered (- one (expt (/ temp fnorm) 2))))
      '""
      '"           compute the ratio of the actual to the predicted"
      '"           reduction."
      '""
      (setf ratio zero)
      (if (> prered zero) (setf ratio (/ actred prered)))
      '""
      '"           update the step bound."
      '""
      (if (>= ratio p1) (go label230))
      (setf ncsuc 0)
      (setf ncfail (+ ncfail 1))
      (setf delta (* p5 delta))
      (go label240)
     label230
      (setf ncfail 0)
      (setf ncsuc (+ ncsuc 1))
      (if (or (>= ratio p5) (> ncsuc 1))
          (setf delta (dmax1 delta (/ pnorm p5))))
      (if (<= (dabs (- ratio one)) p1) (setf delta (/ pnorm p5)))
     label240
      '""
      '"           test for successful iteration."
      '""
      (if (< ratio p0001) (go label260))
      '""
      '"           successful iteration. update x, fvec, and their norms."
      '""
      (fdo (j 1 (+ j 1))
           ((> j n) nil)
           (tagbody
             (fset (fref x (j) ((1 n))) (fref wa2 (j) ((1 n))))
             (fset (fref wa2 (j) ((1 n)))
                   (* (fref diag (j) ((1 n))) (fref x (j) ((1 n)))))
             (fset (fref fvec (j) ((1 n))) (fref wa4 (j) ((1 n))))
            label250))
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
     label260
      '""
      '"           determine the progress of the iteration."
      '""
      (setf nslow1 (+ nslow1 1))
      (if (>= actred p001) (setf nslow1 0))
      (if jeval (setf nslow2 (+ nslow2 1)))
      (if (>= actred p1) (setf nslow2 0))
      '""
      '"           test for convergence."
      '""
      (if (or (<= delta (* xtol xnorm)) (= fnorm zero)) (setf info 1))
      (if (/= info 0) (go label300))
      '""
      '"           tests for termination and stringent tolerances."
      '""
      (if (>= nfev maxfev) (setf info 2))
      (if (<= (* p1 (dmax1 (* p1 delta) pnorm)) (* epsmch xnorm))
          (setf info 3))
      (if (= nslow2 5) (setf info 4))
      (if (= nslow1 10) (setf info 5))
      (if (/= info 0) (go label300))
      '""
      '"           criterion for recalculating jacobian."
      '""
      (if (= ncfail 2) (go label290))
      '""
      '"           calculate the rank one modification to the jacobian"
      '"           and update qtf if necessary."
      '""
      (fdo (j 1 (+ j 1))
           ((> j n) nil)
           (tagbody
             (setf sum zero)
             (fdo (i 1 (+ i 1))
                  ((> i n) nil)
                  (tagbody
                    (setf sum
                            (+ sum
                               (* (fref fjac (i j) ((1 ldfjac) (1 n)))
                                  (fref wa4 (i) ((1 n))))))
                   label270))
             (fset (fref wa2 (j) ((1 n)))
                   (/ (- sum (fref wa3 (j) ((1 n)))) pnorm))
             (fset (fref wa1 (j) ((1 n)))
                   (* (fref diag (j) ((1 n)))
                      (/ (* (fref diag (j) ((1 n))) (fref wa1 (j) ((1 n))))
                         pnorm)))
             (if (>= ratio p0001) (fset (fref qtf (j) ((1 n))) sum))
            label280))
      '""
      '"           compute the qr factorization of the updated jacobian."
      '""
      (multiple-value-bind
          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
          (r1updt n n r lr wa1 wa2 wa3 sing)
        (declare (ignore var-2 var-4 var-5 var-6))
        (when var-0 (setf n var-0))
        (when var-1 (setf n var-1))
        (when var-3 (setf lr var-3))
        (when var-7 (setf sing var-7)))
      (multiple-value-bind
          (var-0 var-1 var-2 var-3 var-4 var-5)
          (r1mpyq n n fjac ldfjac wa2 wa3)
        (declare (ignore var-2 var-4 var-5))
        (when var-0 (setf n var-0))
        (when var-1 (setf n var-1))
        (when var-3 (setf ldfjac var-3)))
      (multiple-value-bind
          (var-0 var-1 var-2 var-3 var-4 var-5)
          (r1mpyq 1 n qtf 1 wa2 wa3)
        (declare (ignore var-0 var-2 var-3 var-4 var-5))
        (when var-1 (setf n var-1)))
      '""
      '"           end of the inner loop."
      '""
      (setf jeval %false%)
      (go label180)
     label290
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
              (var-0 var-1 var-2 var-3 var-4 var-5)
              (funcall fcn n x fvec fjac ldfjac iflag)
            (declare (ignore var-1 var-2 var-3))
            (when var-0 (setf n var-0))
            (when var-4 (setf ldfjac var-4))
            (when var-5 (setf iflag var-5))))
      (go end_label)
      '""
      '"     last card of subroutine hybrj."
      '""
     end_label
      (return
       (values fcn
               n
               x
               fvec
               fjac
               ldfjac
               xtol
               maxfev
               diag
               mode
               factor
               nprint
               info
               nfev
               njev
               r
               lr
               qtf
               wa1
               wa2
               wa3
               wa4)))))

