;;; Compiled by f2cl version 2.0 beta on 2002/01/07 at 17:54:03
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls nil)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t))

(in-package "MINPACK")



(use-package :f2cl)

(let ((factor 100.0d0) (zero 0.0d0))
  (declare (type double-float zero factor))
  (defun lmdif1 (fcn m n x fvec tol info iwa wa lwa)
    (declare (type double-float tol)
             (type (array integer4 (*)) iwa)
             (type (array double-float (*)) x fvec wa)
             (type integer4 m n info lwa))
    (prog ((epsfcn 0.0d0) (ftol 0.0d0) (gtol 0.0d0) (xtol 0.0d0) (maxfev 0)
           (mode 0) (mp5n 0) (nfev 0) (nprint 0))
      (declare (type integer4 nprint nfev mp5n mode maxfev)
               (type double-float xtol gtol ftol epsfcn))
      (declare
       (ftype
        (function
         (single-float integer4 integer4 array-double-float array-double-float
          double-float double-float double-float integer4 double-float
          array-double-float integer4 double-float integer4 integer4 integer4
          array-double-float integer4 array-integer4 array-double-float
          array-double-float array-double-float array-double-float
          array-double-float)
         (values &rest t))
        lmdif))
      '"     **********"
      '""
      '"     subroutine lmdif1"
      '""
      '"     the purpose of lmdif1 is to minimize the sum of the squares of"
      '"     m nonlinear functions in n variables by a modification of the"
      '"     levenberg-marquardt algorithm. this is done by using the more"
      '"     general least-squares solver lmdif. the user must provide a"
      '"     subroutine which calculates the functions. the jacobian is"
      '"     then calculated by a forward-difference approximation."
      '""
      '"     the subroutine statement is"
      '""
      '"       subroutine lmdif1(fcn,m,n,x,fvec,tol,info,iwa,wa,lwa)"
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
      '"         the user wants to terminate execution of lmdif1."
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
      '"       tol is a nonnegative input variable. termination occurs"
      '"         when the algorithm estimates either that the relative"
      '"         error in the sum of squares is at most tol or that"
      '"         the relative error between x and the solution is at"
      '"         most tol."
      '""
      '"       info is an integer output variable. if the user has"
      '"         terminated execution, info is set to the (negative)"
      '"         value of iflag. see description of fcn. otherwise,"
      '"         info is set as follows."
      '""
      '"         info = 0  improper input parameters."
      '""
      '"         info = 1  algorithm estimates that the relative error"
      '"                   in the sum of squares is at most tol."
      '""
      '"         info = 2  algorithm estimates that the relative error"
      '"                   between x and the solution is at most tol."
      '""
      '"         info = 3  conditions for info = 1 and info = 2 both hold."
      '""
      '"         info = 4  fvec is orthogonal to the columns of the"
      '"                   jacobian to machine precision."
      '""
      '"         info = 5  number of calls to fcn has reached or"
      '"                   exceeded 200*(n+1)."
      '""
      '"         info = 6  tol is too small. no further reduction in"
      '"                   the sum of squares is possible."
      '""
      '"         info = 7  tol is too small. no further improvement in"
      '"                   the approximate solution x is possible."
      '""
      '"       iwa is an integer work array of length n."
      '""
      '"       wa is a work array of length lwa."
      '""
      '"       lwa is a positive integer input variable not less than"
      '"         m*n+5*n+m."
      '""
      '"     subprograms called"
      '""
      '"       user-supplied ...... fcn"
      '""
      '"       minpack-supplied ... lmdif"
      '""
      '"     argonne national laboratory. minpack project. march 1980."
      '"     burton s. garbow, kenneth e. hillstrom, jorge j. more"
      '""
      '"     **********"
      (setf info 0)
      '""
      '"     check the input parameters for errors."
      '""
      (if (or (<= n 0) (< m n) (< tol zero) (< lwa (+ (* m n) (* 5 n) m)))
          (go label10))
      '""
      '"     call lmdif."
      '""
      (setf maxfev (* 200 (+ n 1)))
      (setf ftol tol)
      (setf xtol tol)
      (setf gtol zero)
      (setf epsfcn zero)
      (setf mode 1)
      (setf nprint 0)
      (setf mp5n (+ m (* 5 n)))
      (multiple-value-bind
          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10
           var-11 var-12 var-13 var-14 var-15 var-16 var-17 var-18 var-19
           var-20 var-21 var-22 var-23)
          (lmdif fcn m n x fvec ftol xtol gtol maxfev epsfcn
           (array-slice wa double-float (1) ((1 lwa))) mode factor nprint info
           nfev (array-slice wa double-float ((+ mp5n 1)) ((1 lwa))) m iwa
           (array-slice wa double-float ((+ n 1)) ((1 lwa)))
           (array-slice wa double-float ((+ (* 2 n) 1)) ((1 lwa)))
           (array-slice wa double-float ((+ (* 3 n) 1)) ((1 lwa)))
           (array-slice wa double-float ((+ (* 4 n) 1)) ((1 lwa)))
           (array-slice wa double-float ((+ (* 5 n) 1)) ((1 lwa))))
        (declare
         (ignore var-0 var-3 var-4 var-10 var-16 var-18 var-19 var-20 var-21
          var-22 var-23))
        (when var-1 (setf m var-1))
        (when var-2 (setf n var-2))
        (when var-5 (setf ftol var-5))
        (when var-6 (setf xtol var-6))
        (when var-7 (setf gtol var-7))
        (when var-8 (setf maxfev var-8))
        (when var-9 (setf epsfcn var-9))
        (when var-11 (setf mode var-11))
        (when var-12 (setf factor var-12))
        (when var-13 (setf nprint var-13))
        (when var-14 (setf info var-14))
        (when var-15 (setf nfev var-15))
        (when var-17 (setf m var-17)))
      (if (= info 8) (setf info 4))
     label10
      (go end_label)
      '""
      '"     last card of subroutine lmdif1."
      '""
     end_label
      (return (values fcn m n x fvec tol info iwa wa lwa)))))

