;;; Compiled by f2cl version 2.0 beta on 2002/01/07 at 17:53:55
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls nil)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t))

(in-package "MINPACK")



(use-package :f2cl)

(let ((one 1.0d0) (p05 0.05d0) (zero 0.0d0))
  (declare (type double-float zero p05 one))
  (defun qrfac (m n a lda pivot ipvt lipvt rdiag acnorm wa)
    (declare (type (array integer4 (*)) ipvt)
             (type logical pivot)
             (type (array double-float (*)) wa acnorm rdiag a)
             (type integer4 lipvt lda n m))
    (prog ((ajnorm 0.0d0) (epsmch 0.0d0) (sum 0.0d0) (temp 0.0d0) (i 0) (j 0)
           (jp1 0) (k 0) (kmax 0) (minmn 0) (double-float 0.0f0)
           (array-slice 0.0f0))
      (declare (type single-float array-slice double-float)
               (type integer4 minmn kmax k jp1 j i)
               (type double-float temp sum epsmch ajnorm))
      (declare
       (ftype (function (integer4) (values double-float &rest t)) dpmpar))
      (declare
       (ftype
        (function (integer4 array-double-float) (values double-float &rest t))
        enorm))
      (declare (ftype (function (integer4 integer4) (values integer4)) min0))
      (declare
       (ftype (function (double-float double-float) (values double-float))
        dmax1))
      (declare (ftype (function (double-float) (values double-float)) dsqrt))
      '"     **********"
      '""
      '"     subroutine qrfac"
      '""
      '"     this subroutine uses householder transformations with column"
      '"     pivoting (optional) to compute a qr factorization of the"
      '"     m by n matrix a. that is, qrfac determines an orthogonal"
      '"     matrix q, a permutation matrix p, and an upper trapezoidal"
      '"     matrix r with diagonal elements of nonincreasing magnitude,"
      '"     such that a*p = q*r. the householder transformation for"
      '"     column k, k = 1,2,...,min(m,n), is of the form"
      '""
      '"                           t"
      '"           i - (1/u(k))*u*u"
      '""
      '"     where u has zeros in the first k-1 positions. the form of"
      '"     this transformation and the method of pivoting first"
      '"     appeared in the corresponding linpack subroutine."
      '""
      '"     the subroutine statement is"
      '""
      '"       subroutine qrfac(m,n,a,lda,pivot,ipvt,lipvt,rdiag,acnorm,wa)"
      '""
      '"     where"
      '""
      '"       m is a positive integer input variable set to the number"
      '"         of rows of a."
      '""
      '"       n is a positive integer input variable set to the number"
      '"         of columns of a."
      '""
      '"       a is an m by n array. on input a contains the matrix for"
      '"         which the qr factorization is to be computed. on output"
      '"         the strict upper trapezoidal part of a contains the strict"
      '"         upper trapezoidal part of r, and the lower trapezoidal"
      '"         part of a contains a factored form of q (the non-trivial"
      '"         elements of the u vectors described above)."
      '""
      '"       lda is a positive integer input variable not less than m"
      '"         which specifies the leading dimension of the array a."
      '""
      '"       pivot is a logical input variable. if pivot is set true,"
      '"         then column pivoting is enforced. if pivot is set false,"
      '"         then no column pivoting is done."
      '""
      '"       ipvt is an integer output array of length lipvt. ipvt"
      '"         defines the permutation matrix p such that a*p = q*r."
      '"         column j of p is column ipvt(j) of the identity matrix."
      '"         if pivot is false, ipvt is not referenced."
      '""
      '"       lipvt is a positive integer input variable. if pivot is false,"
      '"         then lipvt may be as small as 1. if pivot is true, then"
      '"         lipvt must be at least n."
      '""
      '"       rdiag is an output array of length n which contains the"
      '"         diagonal elements of r."
      '""
      '"       acnorm is an output array of length n which contains the"
      '"         norms of the corresponding columns of the input matrix a."
      '"         if this information is not needed, then acnorm can coincide"
      '"         with rdiag."
      '""
      '"       wa is a work array of length n. if pivot is false, then wa"
      '"         can coincide with rdiag."
      '""
      '"     subprograms called"
      '""
      '"       minpack-supplied ... dpmpar,enorm"
      '""
      '"       fortran-supplied ... dmax1,dsqrt,min0"
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
      '"     compute the initial column norms and initialize several arrays."
      '""
      (fdo (j 1 (+ j 1))
           ((> j n) nil)
           (tagbody
             (fset (fref acnorm (j) ((1 n)))
                   (coerce
                    (multiple-value-bind
                        (ret-val var-0 var-1)
                        (enorm m
                         (array-slice a double-float (1 j) ((1 lda) (1 n))))
                      (declare (ignore var-1))
                      (when var-0 (setf m var-0))
                      ret-val)
                    'double-float))
             (fset (fref rdiag (j) ((1 n))) (fref acnorm (j) ((1 n))))
             (fset (fref wa (j) ((1 n))) (fref rdiag (j) ((1 n))))
             (if pivot (fset (fref ipvt (j) ((1 lipvt))) j))
            label10))
      '""
      '"     reduce a to r with householder transformations."
      '""
      (setf minmn (min0 m n))
      (fdo (j 1 (+ j 1))
           ((> j minmn) nil)
           (tagbody
             (if (not pivot) (go label40))
             '""
             '"        bring the column of largest norm into the pivot position."
             '""
             (setf kmax j)
             (fdo (k j (+ k 1))
                  ((> k n) nil)
                  (tagbody
                    (if
                     (> (fref rdiag (k) ((1 n))) (fref rdiag (kmax) ((1 n))))
                     (setf kmax k))
                   label20))
             (if (= kmax j) (go label40))
             (fdo (i 1 (+ i 1))
                  ((> i m) nil)
                  (tagbody
                    (setf temp (fref a (i j) ((1 lda) (1 n))))
                    (fset (fref a (i j) ((1 lda) (1 n)))
                          (fref a (i kmax) ((1 lda) (1 n))))
                    (fset (fref a (i kmax) ((1 lda) (1 n))) temp)
                   label30))
             (fset (fref rdiag (kmax) ((1 n))) (fref rdiag (j) ((1 n))))
             (fset (fref wa (kmax) ((1 n))) (fref wa (j) ((1 n))))
             (setf k (fref ipvt (j) ((1 lipvt))))
             (fset (fref ipvt (j) ((1 lipvt))) (fref ipvt (kmax) ((1 lipvt))))
             (fset (fref ipvt (kmax) ((1 lipvt))) k)
            label40
             '""
             '"        compute the householder transformation to reduce the"
             '"        j-th column of a to a multiple of the j-th unit vector."
             '""
             (setf ajnorm
                     (enorm (+ (- m j) 1)
                      (array-slice a double-float (j j) ((1 lda) (1 n)))))
             (if (= ajnorm zero) (go label100))
             (if (< (fref a (j j) ((1 lda) (1 n))) zero)
                 (setf ajnorm (- ajnorm)))
             (fdo (i j (+ i 1))
                  ((> i m) nil)
                  (tagbody
                    (fset (fref a (i j) ((1 lda) (1 n)))
                          (/ (fref a (i j) ((1 lda) (1 n))) ajnorm))
                   label50))
             (fset (fref a (j j) ((1 lda) (1 n)))
                   (+ (fref a (j j) ((1 lda) (1 n))) one))
             '""
             '"        apply the transformation to the remaining columns"
             '"        and update the norms."
             '""
             (setf jp1 (+ j 1))
             (if (< n jp1) (go label100))
             (fdo (k jp1 (+ k 1))
                  ((> k n) nil)
                  (tagbody
                    (setf sum zero)
                    (fdo (i j (+ i 1))
                         ((> i m) nil)
                         (tagbody
                           (setf sum
                                   (+ sum
                                      (* (fref a (i j) ((1 lda) (1 n)))
                                         (fref a (i k) ((1 lda) (1 n))))))
                          label60))
                    (setf temp (/ sum (fref a (j j) ((1 lda) (1 n)))))
                    (fdo (i j (+ i 1))
                         ((> i m) nil)
                         (tagbody
                           (fset (fref a (i k) ((1 lda) (1 n)))
                                 (- (fref a (i k) ((1 lda) (1 n)))
                                    (* temp (fref a (i j) ((1 lda) (1 n))))))
                          label70))
                    (if (or (not pivot) (= (fref rdiag (k) ((1 n))) zero))
                        (go label80))
                    (setf temp
                            (/ (fref a (j k) ((1 lda) (1 n)))
                               (fref rdiag (k) ((1 n)))))
                    (fset (fref rdiag (k) ((1 n)))
                          (* (fref rdiag (k) ((1 n)))
                             (dsqrt (dmax1 zero (- one (expt temp 2))))))
                    (if
                     (>
                      (* p05
                         (expt
                          (/ (fref rdiag (k) ((1 n))) (fref wa (k) ((1 n))))
                          2))
                      epsmch)
                     (go label80))
                    (fset (fref rdiag (k) ((1 n)))
                          (enorm (- m j)
                           (array-slice a
                                        double-float
                                        (jp1 k)
                                        ((1 lda) (1 n)))))
                    (fset (fref wa (k) ((1 n))) (fref rdiag (k) ((1 n))))
                   label80
                   label90))
            label100
             (fset (fref rdiag (j) ((1 n))) (- ajnorm))
            label110))
      (go end_label)
      '""
      '"     last card of subroutine qrfac."
      '""
     end_label
      (return (values m n a lda pivot ipvt lipvt rdiag acnorm wa)))))

