;;; Compiled by f2cl version 2.0 beta on 2002/01/07 at 17:53:52
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls nil)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t))

(in-package "MINPACK")



(use-package :f2cl)

(let ((p5 0.5d0) (p25 0.25d0) (zero 0.0d0))
  (declare (type double-float zero p25 p5))
  (defun qrsolv (n r ldr ipvt diag qtb x sdiag wa)
    (declare (type (array integer4 (*)) ipvt)
             (type (array double-float (*)) wa sdiag x qtb diag r)
             (type integer4 ldr n))
    (prog ((cos 0.0d0) (cotan 0.0d0) (qtbpj 0.0d0) (sin 0.0d0) (sum 0.0d0)
           (tan 0.0d0) (temp 0.0d0) (i 0) (j 0) (jp1 0) (k 0) (kp1 0) (l 0)
           (nsing 0))
      (declare (type integer4 nsing l kp1 k jp1 j i)
               (type double-float temp tan sum sin qtbpj cotan cos))
      (declare
       (ftype (function (array-double-float) (values double-float)) dabs))
      (declare (ftype (function (double-float) (values double-float)) dsqrt))
      '"     **********"
      '""
      '"     subroutine qrsolv"
      '""
      '"     given an m by n matrix a, an n by n diagonal matrix d,"
      '"     and an m-vector b, the problem is to determine an x which"
      '"     solves the system"
      '""
      '"           a*x = b ,     d*x = 0 ,"
      '""
      '"     in the least squares sense."
      '""
      '"     this subroutine completes the solution of the problem"
      '"     if it is provided with the necessary information from the"
      '"     qr factorization, with column pivoting, of a. that is, if"
      '"     a*p = q*r, where p is a permutation matrix, q has orthogonal"
      '"     columns, and r is an upper triangular matrix with diagonal"
      '"     elements of nonincreasing magnitude, then qrsolv expects"
      '"     the full upper triangle of r, the permutation matrix p,"
      '"     and the first n components of (q transpose)*b. the system"
      '"     a*x = b, d*x = 0, is then equivalent to"
      '""
      '"                  t       t"
      '"           r*z = q *b ,  p *d*p*z = 0 ,"
      '""
      '"     where x = p*z. if this system does not have full rank,"
      '"     then a least squares solution is obtained. on output qrsolv"
      '"     also provides an upper triangular matrix s such that"
      '""
      '"            t   t               t"
      '"           p *(a *a + d*d)*p = s *s ."
      '""
      '"     s is computed within qrsolv and may be of separate interest."
      '""
      '"     the subroutine statement is"
      '""
      '"       subroutine qrsolv(n,r,ldr,ipvt,diag,qtb,x,sdiag,wa)"
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
      '"       x is an output array of length n which contains the least"
      '"         squares solution of the system a*x = b, d*x = 0."
      '""
      '"       sdiag is an output array of length n which contains the"
      '"         diagonal elements of the upper triangular matrix s."
      '""
      '"       wa is a work array of length n."
      '""
      '"     subprograms called"
      '""
      '"       fortran-supplied ... dabs,dsqrt"
      '""
      '"     argonne national laboratory. minpack project. march 1980."
      '"     burton s. garbow, kenneth e. hillstrom, jorge j. more"
      '""
      '"     **********"
      '""
      '"     copy r and (q transpose)*b to preserve input and initialize s."
      '"     in particular, save the diagonal elements of r in x."
      '""
      (fdo (j 1 (+ j 1))
           ((> j n) nil)
           (tagbody
             (fdo (i j (+ i 1))
                  ((> i n) nil)
                  (tagbody
                    (fset (fref r (i j) ((1 ldr) (1 n)))
                          (fref r (j i) ((1 ldr) (1 n))))
                   label10))
             (fset (fref x (j) ((1 n))) (fref r (j j) ((1 ldr) (1 n))))
             (fset (fref wa (j) ((1 n))) (fref qtb (j) ((1 n))))
            label20))
      '""
      '"     eliminate the diagonal matrix d using a givens rotation."
      '""
      (fdo (j 1 (+ j 1))
           ((> j n) nil)
           (tagbody
             '""
             '"        prepare the row of d to be eliminated, locating the"
             '"        diagonal element using p from the qr factorization."
             '""
             (setf l (fref ipvt (j) ((1 n))))
             (if (= (fref diag (l) ((1 n))) zero) (go label90))
             (fdo (k j (+ k 1))
                  ((> k n) nil)
                  (tagbody (fset (fref sdiag (k) ((1 n))) zero) label30))
             (fset (fref sdiag (j) ((1 n))) (fref diag (l) ((1 n))))
             '""
             '"        the transformations to eliminate the row of d"
             '"        modify only a single element of (q transpose)*b"
             '"        beyond the first n, which is initially zero."
             '""
             (setf qtbpj zero)
             (fdo (k j (+ k 1))
                  ((> k n) nil)
                  (tagbody
                    '""
                    '"           determine a givens rotation which eliminates the"
                    '"           appropriate element in the current row of d."
                    '""
                    (if (= (fref sdiag (k) ((1 n))) zero) (go label70))
                    (if
                     (>= (dabs (fref r (k k) ((1 ldr) (1 n))))
                         (dabs (fref sdiag (k) ((1 n)))))
                     (go label40))
                    (setf cotan
                            (/ (fref r (k k) ((1 ldr) (1 n)))
                               (fref sdiag (k) ((1 n)))))
                    (setf sin (/ p5 (dsqrt (+ p25 (* p25 (expt cotan 2))))))
                    (setf cos (* sin cotan))
                    (go label50)
                   label40
                    (setf tan
                            (/ (fref sdiag (k) ((1 n)))
                               (fref r (k k) ((1 ldr) (1 n)))))
                    (setf cos (/ p5 (dsqrt (+ p25 (* p25 (expt tan 2))))))
                    (setf sin (* cos tan))
                   label50
                    '""
                    '"           compute the modified diagonal element of r and"
                    '"           the modified element of ((q transpose)*b,0)."
                    '""
                    (fset (fref r (k k) ((1 ldr) (1 n)))
                          (+ (* cos (fref r (k k) ((1 ldr) (1 n))))
                             (* sin (fref sdiag (k) ((1 n))))))
                    (setf temp (+ (* cos (fref wa (k) ((1 n)))) (* sin qtbpj)))
                    (setf qtbpj
                            (+ (* (- sin) (fref wa (k) ((1 n))))
                               (* cos qtbpj)))
                    (fset (fref wa (k) ((1 n))) temp)
                    '""
                    '"           accumulate the tranformation in the row of s."
                    '""
                    (setf kp1 (+ k 1))
                    (if (< n kp1) (go label70))
                    (fdo (i kp1 (+ i 1))
                         ((> i n) nil)
                         (tagbody
                           (setf temp
                                   (+ (* cos (fref r (i k) ((1 ldr) (1 n))))
                                      (* sin (fref sdiag (i) ((1 n))))))
                           (fset (fref sdiag (i) ((1 n)))
                                 (+ (* (- sin) (fref r (i k) ((1 ldr) (1 n))))
                                    (* cos (fref sdiag (i) ((1 n))))))
                           (fset (fref r (i k) ((1 ldr) (1 n))) temp)
                          label60))
                   label70
                   label80))
            label90
             '""
             '"        store the diagonal element of s and restore"
             '"        the corresponding diagonal element of r."
             '""
             (fset (fref sdiag (j) ((1 n))) (fref r (j j) ((1 ldr) (1 n))))
             (fset (fref r (j j) ((1 ldr) (1 n))) (fref x (j) ((1 n))))
            label100))
      '""
      '"     solve the triangular system for z. if the system is"
      '"     singular, then obtain a least squares solution."
      '""
      (setf nsing n)
      (fdo (j 1 (+ j 1))
           ((> j n) nil)
           (tagbody
             (if (and (= (fref sdiag (j) ((1 n))) zero) (= nsing n))
                 (setf nsing (- j 1)))
             (if (< nsing n) (fset (fref wa (j) ((1 n))) zero))
            label110))
      (if (< nsing 1) (go label150))
      (fdo (k 1 (+ k 1))
           ((> k nsing) nil)
           (tagbody
             (setf j (+ (- nsing k) 1))
             (setf sum zero)
             (setf jp1 (+ j 1))
             (if (< nsing jp1) (go label130))
             (fdo (i jp1 (+ i 1))
                  ((> i nsing) nil)
                  (tagbody
                    (setf sum
                            (+ sum
                               (* (fref r (i j) ((1 ldr) (1 n)))
                                  (fref wa (i) ((1 n))))))
                   label120))
            label130
             (fset (fref wa (j) ((1 n)))
                   (/ (- (fref wa (j) ((1 n))) sum) (fref sdiag (j) ((1 n)))))
            label140))
     label150
      '""
      '"     permute the components of z back to components of x."
      '""
      (fdo (j 1 (+ j 1))
           ((> j n) nil)
           (tagbody
             (setf l (fref ipvt (j) ((1 n))))
             (fset (fref x (l) ((1 n))) (fref wa (j) ((1 n))))
            label160))
      (go end_label)
      '""
      '"     last card of subroutine qrsolv."
      '""
     end_label
      (return (values n r ldr ipvt diag qtb x sdiag wa)))))

