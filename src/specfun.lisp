(in-package "MATLISP")

(def-fortran-routine anorm :double-float
  "
 This function evaluates the normal distribution function:

                              / x
                     1       |       -t*t/2
          P(x) = ----------- |      e       dt
                 sqrt(2 pi)  |
                             /-oo

"
  (x :double-float :input))

(def-fortran-routine besei0 :double-float
  "
Computes the modified Bessel function of the first kind of order zero,
multiplied by exp(-|x|).
"
  (x :double-float :input))

(def-fortran-routine besei1 :double-float
  "
Computes the modified Bessel function of the first kind of order one,
multiplied by exp(-|x|).
"
  (x :double-float :input))

(def-fortran-routine besek0 :double-float
  "
Computes the modified Bessel function of the second kind of order zero,
multiplied by exp(-|x|)  x > 0.
"
  (x :double-float :input))

(def-fortran-routine besek1 :double-float
  "
Computes the modified Bessel function of the second kind of order one,
multiplied by exp(-|x|)  0 < x.
"
  (x :double-float :input))

(def-fortran-routine besi0 :double-float
  "
Computes the modified Bessel function of the second kind of order zero,
"
  (x :double-float :input))

(def-fortran-routine besi1 :double-float
  "
Computes the modified Bessel function of the second kind of order one,
"
  (x :double-float :input))

(def-fortran-routine besj0 :double-float
  "
Computes the Bessel function of the first kind of order zero,
"
  (x :double-float :input))

(def-fortran-routine besj1 :double-float
  "
Computes the Bessel function of the first kind of order one,
"
  (x :double-float :input))

(def-fortran-routine besk0 :double-float
  "
Computes the modified Bessel function of the second kind of order zero,
"
  (x :double-float :input))

(def-fortran-routine besk1 :double-float
  "
Computes the modified Bessel function of the second kind of order one,
"
  (x :double-float :input))

(def-fortran-routine besy0 :double-float
  "
Computes the Bessel function of the second kind of order zero,
"
  (x :double-float :input))

(def-fortran-routine besy1 :double-float
  "
Computes the Bessel function of the second kind of order one,
"
  (x :double-float :input))

(def-fortran-routine daw :double-float
  "
 This function program evaluates Dawson's integral,

                       2  / x   2
                     -x   |    t
             F(x) = e     |   e    dt
                          |
                          / 0

   for a real argument x.
"
  (x :double-float :input))

(def-fortran-routine derf :double-float
  "
Compute erf(x)
"
  (x :double-float :input))

(def-fortran-routine derfc :double-float
  "
Compute erfc(x) = 1 - erf(x)
"
  (x :double-float :input))

(def-fortran-routine derfcx :double-float
  "
Compute erfcx(x) = exp(x*x)*erfc(x)
"
  (x :double-float :input))

(def-fortran-routine dgamma :double-float
  "
Compute Gamma(x) for real x
"
  (x :double-float :input))

(def-fortran-routine dlgama :double-float
  "
Compute log(gamma(x)) for positive real x.
"
  (x :double-float :input))

(def-fortran-routine ei :double-float
  "
Compute exponential integral Ei(x), x real.
"
  (x :double-float :input))

(def-fortran-routine eone :double-float
  "
Compute exponential integral E1(x), x real.
"
  (x :double-float :input))

(def-fortran-routine expei :double-float
  "
Compute the function exp(-x)*Ei(x), x real.
"
  (x :double-float :input))

(def-fortran-routine psi :double-float
  "
 This function program evaluates the logarithmic derivative of the
   gamma function,

      psi(x) = d/dx (gamma(x)) / gamma(x) = d/dx (ln gamma(x))

   for real x
"
  (x :double-float :input))

(def-fortran-routine ribesl :void
  "
  This routine calculates Bessel functions I SUB(N+ALPHA) (X)
  for non-negative argument X, and non-negative order N+ALPHA,
  with or without exponential scaling.


 Explanation of variables in the calling sequence

 X     - Working precision non-negative real argument for which
         I's or exponentially scaled I's (I*EXP(-X))
         are to be calculated.  If I's are to be calculated,
         X must be less than EXPARG (see below).
 ALPHA - Working precision fractional part of order for which
         I's or exponentially scaled I's (I*EXP(-X)) are
         to be calculated.  0 .LE. ALPHA .LT. 1.0.
 NB    - Integer number of functions to be calculated, NB .GT. 0.
         The first function calculated is of order ALPHA, and the
         last is of order (NB - 1 + ALPHA).
 IZE   - Integer type.  IZE = 1 if unscaled I's are to calculated,
         and 2 if exponentially scaled I's are to be calculated.
 B     - Working precision output vector of length NB.  If the routine
         terminates normally (NCALC=NB), the vector B contains the
         functions I(ALPHA,X) through I(NB-1+ALPHA,X), or the
         corresponding exponentially scaled functions.
 NCALC - Integer output variable indicating possible errors.
         Before using the vector B, the user should check that
         NCALC=NB, i.e., all orders have been calculated to
         the desired accuracy.  See error returns below.
"
  (x :double-float :input)
  (alpha :double-float :input)
  (nb :integer :input)
  (ize :integer :input)
  (b (* :double-float) :output)
  (ncalc :integer :output))

(def-fortran-routine rjbesl :void
  "
 This routine calculates Bessel functions J sub(N+ALPHA) (X)
   for non-negative argument X, and non-negative order N+ALPHA.


  Explanation of variables in the calling sequence.

   X     - working precision non-negative real argument for which
           J's are to be calculated.
   ALPHA - working precision fractional part of order for which
           J's or exponentially scaled J'r (J*exp(X)) are
           to be calculated.  0 <= ALPHA < 1.0.
   NB  - integer number of functions to be calculated, NB > 0.
           The first function calculated is of order ALPHA, and the
           last is of order (NB - 1 + ALPHA).
   B  - working precision output vector of length NB.  If RJBESL
           terminates normally (NCALC=NB), the vector B contains the
           functions J/ALPHA/(X) through J/NB-1+ALPHA/(X), or the
           corresponding exponentially scaled functions.
   NCALC - integer output variable indicating possible errors.
           Before using the vector B, the user should check that
           NCALC=NB, i.e., all orders have been calculated to
           the desired accuracy.  See Error Returns below.
  Error returns

    In case of an error,  NCALC .NE. NB, and not all J's are
    calculated to the desired accuracy.

    NCALC .LT. 0:  An argument is out of range. For example,
       NBES .LE. 0, ALPHA .LT. 0 or .GT. 1, or X is too large.
       In this case, B(1) is set to zero, the remainder of the
       B-vector is not calculated, and NCALC is set to
       MIN(NB,0)-1 so that NCALC .NE. NB.

    NB .GT. NCALC .GT. 0: Not all requested function values could
       be calculated accurately.  This usually occurs because NB is
       much larger than ABS(X).  In this case, B(N) is calculated
       to the desired accuracy for N .LE. NCALC, but precision
       is lost for NCALC .LT. N .LE. NB.  If B(N) does not vanish
       for N .GT. NCALC (because it is too small to be represented),
       and B(N)/B(NCALC) = 10**(-K), then only the first NSIG-K
       significant figures of B(N) can be trusted.
"
  (x :double-float :input)
  (alpha :double-float :input)
  (nb :integer :input)
  (b (* :double-float) :output)
  (ncalc :integer :output))

(def-fortran-routine rkbesl :void
  "
  This FORTRAN 77 routine calculates modified Bessel functions
  of the second kind, K SUB(N+ALPHA) (X), for non-negative
  argument X, and non-negative order N+ALPHA, with or without
  exponential scaling.

  Explanation of variables in the calling sequence

  Description of output values ..

 X     - Working precision non-negative real argument for which
         K's or exponentially scaled K's (K*EXP(X))
         are to be calculated.  If K's are to be calculated,
         X must not be greater than XMAX (see below).
 ALPHA - Working precision fractional part of order for which
         K's or exponentially scaled K's (K*EXP(X)) are
         to be calculated.  0 .LE. ALPHA .LT. 1.0.
 NB    - Integer number of functions to be calculated, NB .GT. 0.
         The first function calculated is of order ALPHA, and the
         last is of order (NB - 1 + ALPHA).
 IZE   - Integer type.  IZE = 1 if unscaled K's are to be calculated,
         and 2 if exponentially scaled K's are to be calculated.
 BK    - Working precision output vector of length NB.  If the
         routine terminates normally (NCALC=NB), the vector BK
         contains the functions K(ALPHA,X), ... , K(NB-1+ALPHA,X),
         or the corresponding exponentially scaled functions.
         If (0 .LT. NCALC .LT. NB), BK(I) contains correct function
         values for I .LE. NCALC, and contains the ratios
         K(ALPHA+I-1,X)/K(ALPHA+I-2,X) for the rest of the array.
 NCALC - Integer output variable indicating possible errors.
         Before using the vector BK, the user should check that
         NCALC=NB, i.e., all orders have been calculated to
         the desired accuracy.  See error returns below.


*******************************************************************

 Error returns

  In case of an error, NCALC .NE. NB, and not all K's are
  calculated to the desired accuracy.

  NCALC .LT. -1:  An argument is out of range. For example,
       NB .LE. 0, IZE is not 1 or 2, or IZE=1 and ABS(X) .GE.
       XMAX.  In this case, the B-vector is not calculated,
       and NCALC is set to MIN0(NB,0)-2  so that NCALC .NE. NB.
  NCALC = -1:  Either  K(ALPHA,X) .GE. XINF  or
       K(ALPHA+NB-1,X)/K(ALPHA+NB-2,X) .GE. XINF.  In this case,
       the B-vector is not calculated.  Note that again
       NCALC .NE. NB.

  0 .LT. NCALC .LT. NB: Not all requested function values could
       be calculated accurately.  BK(I) contains correct function
       values for I .LE. NCALC, and contains the ratios
       K(ALPHA+I-1,X)/K(ALPHA+I-2,X) for the rest of the array.
"
  (x :double-float :input)
  (alpha :double-float :input)
  (nb :integer :input)
  (ize :integer :input)
  (b (* :double-float) :output)
  (ncalc :integer :output))

(def-fortran-routine rybesl :void
  "
  This routine calculates Bessel functions Y SUB(N+ALPHA) (X)
  for non-negative argument X, and non-negative order N+ALPHA.


 Explanation of variables in the calling sequence

 X     - Working precision positive real argument for which
         Y's are to be calculated.
 ALPHA - Working precision fractional part of order for which
         Y's are to be calculated.  0 .LE. ALPHA .LT. 1.0.
 NB    - Integer number of functions to be calculated, NB .GT. 0.
         The first function calculated is of order ALPHA, and the
         last is of order (NB - 1 + ALPHA).
 BY    - Working precision output vector of length NB.  If the
         routine terminates normally (NCALC=NB), the vector BY
         contains the functions Y(ALPHA,X), ... , Y(NB-1+ALPHA,X),
         If (0 .LT. NCALC .LT. NB), BY(I) contains correct function
         values for I .LE. NCALC, and contains the ratios
         Y(ALPHA+I-1,X)/Y(ALPHA+I-2,X) for the rest of the array.
 NCALC - Integer output variable indicating possible errors.
         Before using the vector BY, the user should check that
         NCALC=NB, i.e., all orders have been calculated to
         the desired accuracy.  See error returns below.


*******************************************************************
*******************************************************************

 Explanation of machine-dependent constants.  Let

   beta   = Radix for the floating-point system
   p      = Number of significant base-beta digits in the
            significand of a floating-point number
   minexp = Smallest representable power of beta
   maxexp = Smallest power of beta that overflows

 Then the following machine-dependent constants must be declared
   in DATA statements.  IEEE values are provided as a default.

   EPS    = beta ** (-p)
   DEL    = Machine number below which sin(x)/x = 1; approximately
            SQRT(EPS).
   XMIN   = Smallest acceptable argument for RBESY; approximately
            max(2*beta**minexp,2/XINF), rounded up
   XINF   = Largest positive machine number; approximately
            beta**maxexp
   THRESH = Lower bound for use of the asymptotic form; approximately
            AINT(-LOG10(EPS/2.0))+1.0
   XLARGE = Upper bound on X; approximately 1/DEL, because the sine
            and cosine functions have lost about half of their
            precision at that point.


     Approximate values for some important machines are:

                        beta    p     minexp      maxexp      EPS

  CRAY-1        (S.P.)    2    48     -8193        8191    3.55E-15
  Cyber 180/185
    under NOS   (S.P.)    2    48      -975        1070    3.55E-15
  IEEE (IBM/XT,
    SUN, etc.)  (S.P.)    2    24      -126         128    5.96E-8
  IEEE (IBM/XT,
    SUN, etc.)  (D.P.)    2    53     -1022        1024    1.11D-16
  IBM 3033      (D.P.)   16    14       -65          63    1.39D-17
  VAX           (S.P.)    2    24      -128         127    5.96E-8
  VAX D-Format  (D.P.)    2    56      -128         127    1.39D-17
  VAX G-Format  (D.P.)    2    53     -1024        1023    1.11D-16


                         DEL      XMIN      XINF     THRESH  XLARGE

 CRAY-1        (S.P.)  5.0E-8  3.67E-2466 5.45E+2465  15.0E0  2.0E7
 Cyber 180/855
   under NOS   (S.P.)  5.0E-8  6.28E-294  1.26E+322   15.0E0  2.0E7
 IEEE (IBM/XT,
   SUN, etc.)  (S.P.)  1.0E-4  2.36E-38   3.40E+38     8.0E0  1.0E4
 IEEE (IBM/XT,
   SUN, etc.)  (D.P.)  1.0D-8  4.46D-308  1.79D+308   16.0D0  1.0D8
 IBM 3033      (D.P.)  1.0D-8  2.77D-76   7.23D+75    17.0D0  1.0D8
 VAX           (S.P.)  1.0E-4  1.18E-38   1.70E+38     8.0E0  1.0E4
 VAX D-Format  (D.P.)  1.0D-9  1.18D-38   1.70D+38    17.0D0  1.0D9
 VAX G-Format  (D.P.)  1.0D-8  2.23D-308  8.98D+307   16.0D0  1.0D8

*******************************************************************
*******************************************************************

 Error returns

  In case of an error, NCALC .NE. NB, and not all Y's are
  calculated to the desired accuracy.

  NCALC .LE. -1:  An argument is out of range. For example,
       NB .LE. 0, or ABS(X) .GE. XLARGE.  In this case,
       BY(1) = 0.0, the remainder of the BY-vector is not
       calculated, and NCALC is set to MIN0(NB,0)-1  so that
       NCALC .NE. NB.
  1 .LT. NCALC .LT. NB: Not all requested function values could
       be calculated accurately.  BY(I) contains correct function
       values for I .LE. NCALC, and and the remaining NB-NCALC
       array elements contain 0.0.
"
  (x :double-float :input)
  (alpha :double-float :input)
  (nb :integer :input)
  (b (* :double-float) :output)
  (ncalc :integer :output))


(make-real-mapper -normal-cdf anorm)
(make-real-mapper -bessel-scaled-i0 besei0)
(make-real-mapper -bessel-scaled-i1 besei1)
(make-real-mapper -bessel-scaled-k0 besek0)
(make-real-mapper -bessel-scaled-k1 besek1)
(make-real-mapper -bessel-i0 besi0)
(make-real-mapper -bessel-i1 besi1)
(make-real-mapper -bessel-j0 besj0)
(make-real-mapper -bessel-j1 besj1)
(make-real-mapper -bessel-k0 besk0)
(make-real-mapper -bessel-k1 besk1)
(make-real-mapper -bessel-y0 besy0)
(make-real-mapper -bessel-y1 besy1)
(make-real-mapper -dawson-integral daw)
(make-real-mapper -erf derf)
(make-real-mapper -erfc derfc)
(make-real-mapper -erfcx derfcx)
(make-real-mapper -gamma dgamma)
(make-real-mapper -log-gamma dlgama)
;; We need to make sure the arguments to dlgama are positive!
(defmethod m-log-gamma :before ((matrix real-matrix))
  (assert (every #'(lambda (x)
		     (>= x 0))
		 (store matrix))))
(make-real-mapper -exponential-integral ei)
(make-real-mapper -exponential-integral-1 eone)
(make-real-mapper -psi psi)

(macrolet
    ((frob (name)
       `(progn
	 (defmethod ,name ((x double-float) (alpha double-float) (n fixnum)
			   &key (scale-p t))
	   (assert (>= x 0))
	   (assert (<= 0 alpha 1))
	   (assert (plusp n)))

	 (defmethod ,name ((x double-float) (alpha double-float) (n fixnum)
			   &key (scale-p t))
	   (let ((result (make-real-matrix n 1)))
	     (multiple-value-bind (t-result ncalc)
		 (ribesl x alpha n (if scale-p 2 1) (store result) 0)
	       (assert (= ncalc n) nil
		       "Could not compute all values to desired precision"))
	     result)))))
  (frob bessel-series-i)
  (frob bessel-series-j)
  (frob bessel-series-k)
  (frob bessel-series-y))