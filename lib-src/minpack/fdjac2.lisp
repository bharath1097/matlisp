;;; Compiled by f2cl version 2.0 beta on 2002/01/07 at 17:53:51
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls nil)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t))

(in-package "MINPACK")



(use-package :f2cl)

(let ((zero 0.0d0))
  (declare (type double-float zero))
  (defun fdjac2 (fcn m n x fvec fjac ldfjac iflag epsfcn wa)
    (declare (type double-float epsfcn)
             (type (array double-float (*)) wa fjac fvec x)
             (type integer4 iflag ldfjac n m)
             (type single-float fcn)
             (type
              (function
               (integer4 integer4 array-double-float array-double-float
                integer4)
               (values &rest t))
              fcn))
    (prog ((eps 0.0d0) (epsmch 0.0d0) (h 0.0d0) (temp 0.0d0) (i 0) (j 0))
      (declare (type integer4 j i) (type double-float temp h epsmch eps))
      (declare
       (ftype (function (integer4) (values double-float &rest t)) dpmpar))
      (declare
       (ftype (function (double-float double-float) (values double-float))
        dmax1))
      (declare (ftype (function (double-float) (values double-float)) dsqrt))
      (declare (ftype (function (double-float) (values double-float)) dabs))
      '"     **********"
      '""
      '"     subroutine fdjac2"
      '""
      '"     this subroutine computes a forward-difference approximation"
      '"     to the m by n jacobian matrix associated with a specified"
      '"     problem of m functions in n variables."
      '""
      '"     the subroutine statement is"
      '""
      '"       subroutine fdjac2(fcn,m,n,x,fvec,fjac,ldfjac,iflag,epsfcn,wa)"
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
      '"         the user wants to terminate execution of fdjac2."
      '"         in this case set iflag to a negative integer."
      '""
      '"       m is a positive integer input variable set to the number"
      '"         of functions."
      '""
      '"       n is a positive integer input variable set to the number"
      '"         of variables. n must not exceed m."
      '""
      '"       x is an input array of length n."
      '""
      '"       fvec is an input array of length m which must contain the"
      '"         functions evaluated at x."
      '""
      '"       fjac is an output m by n array which contains the"
      '"         approximation to the jacobian matrix evaluated at x."
      '""
      '"       ldfjac is a positive integer input variable not less than m"
      '"         which specifies the leading dimension of the array fjac."
      '""
      '"       iflag is an integer variable which can be used to terminate"
      '"         the execution of fdjac2. see description of fcn."
      '""
      '"       epsfcn is an input variable used in determining a suitable"
      '"         step length for the forward-difference approximation. this"
      '"         approximation assumes that the relative errors in the"
      '"         functions are of the order of epsfcn. if epsfcn is less"
      '"         than the machine precision, it is assumed that the relative"
      '"         errors in the functions are of the order of the machine"
      '"         precision."
      '""
      '"       wa is a work array of length m."
      '""
      '"     subprograms called"
      '""
      '"       user-supplied ...... fcn"
      '""
      '"       minpack-supplied ... dpmpar"
      '""
      '"       fortran-supplied ... dabs,dmax1,dsqrt"
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
      (setf eps (dsqrt (dmax1 epsfcn epsmch)))
      (fdo (j 1 (+ j 1))
           ((> j n) nil)
           (tagbody
             (setf temp (fref x (j) ((1 n))))
             (setf h (* eps (dabs temp)))
             (if (= h zero) (setf h eps))
             (fset (fref x (j) ((1 n))) (+ temp h))
             (multiple-value-bind
                 (var-0 var-1 var-2 var-3 var-4)
                 (funcall fcn m n x wa iflag)
               (declare (ignore var-2 var-3))
               (when var-0 (setf m var-0))
               (when var-1 (setf n var-1))
               (when var-4 (setf iflag var-4)))
             (if (< iflag 0) (go label30))
             (fset (fref x (j) ((1 n))) temp)
             (fdo (i 1 (+ i 1))
                  ((> i m) nil)
                  (tagbody
                    (fset (fref fjac (i j) ((1 ldfjac) (1 n)))
                          (/ (- (fref wa (i) ((1 m))) (fref fvec (i) ((1 m))))
                             h))
                   label10))
            label20))
     label30
      (go end_label)
      '""
      '"     last card of subroutine fdjac2."
      '""
     end_label
      (return (values fcn m n x fvec fjac ldfjac iflag epsfcn wa)))))

