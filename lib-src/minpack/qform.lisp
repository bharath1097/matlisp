;;; Compiled by f2cl version 2.0 beta on 2002/01/07 at 17:54:11
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls nil)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t))

(in-package "MINPACK")



(use-package :f2cl)

(let ((one 1.0d0) (zero 0.0d0))
  (declare (type double-float zero one))
  (defun qform (m n q ldq wa)
    (declare (type (array double-float (*)) wa q) (type integer4 ldq n m))
    (prog ((sum 0.0d0) (temp 0.0d0) (i 0) (j 0) (jm1 0) (k 0) (l 0) (minmn 0)
           (np1 0))
      (declare (type integer4 np1 minmn l k jm1 j i)
               (type double-float temp sum))
      (declare (ftype (function (integer4 integer4) (values integer4)) min0))
      '"     **********"
      '""
      '"     subroutine qform"
      '""
      '"     this subroutine proceeds from the computed qr factorization of"
      '"     an m by n matrix a to accumulate the m by m orthogonal matrix"
      '"     q from its factored form."
      '""
      '"     the subroutine statement is"
      '""
      '"       subroutine qform(m,n,q,ldq,wa)"
      '""
      '"     where"
      '""
      '"       m is a positive integer input variable set to the number"
      '"         of rows of a and the order of q."
      '""
      '"       n is a positive integer input variable set to the number"
      '"         of columns of a."
      '""
      '"       q is an m by m array. on input the full lower trapezoid in"
      '"         the first min(m,n) columns of q contains the factored form."
      '"         on output q has been accumulated into a square matrix."
      '""
      '"       ldq is a positive integer input variable not less than m"
      '"         which specifies the leading dimension of the array q."
      '""
      '"       wa is a work array of length m."
      '""
      '"     subprograms called"
      '""
      '"       fortran-supplied ... min0"
      '""
      '"     argonne national laboratory. minpack project. march 1980."
      '"     burton s. garbow, kenneth e. hillstrom, jorge j. more"
      '""
      '"     **********"
      '""
      '"     zero out upper triangle of q in the first min(m,n) columns."
      '""
      (setf minmn (min0 m n))
      (if (< minmn 2) (go label30))
      (fdo (j 2 (+ j 1))
           ((> j minmn) nil)
           (tagbody
             (setf jm1 (- j 1))
             (fdo (i 1 (+ i 1))
                  ((> i jm1) nil)
                  (tagbody (fset (fref q (i j) ((1 ldq) (1 m))) zero) label10))
            label20))
     label30
      '""
      '"     initialize remaining columns to those of the identity matrix."
      '""
      (setf np1 (+ n 1))
      (if (< m np1) (go label60))
      (fdo (j np1 (+ j 1))
           ((> j m) nil)
           (tagbody
             (fdo (i 1 (+ i 1))
                  ((> i m) nil)
                  (tagbody (fset (fref q (i j) ((1 ldq) (1 m))) zero) label40))
             (fset (fref q (j j) ((1 ldq) (1 m))) one)
            label50))
     label60
      '""
      '"     accumulate q from its factored form."
      '""
      (fdo (l 1 (+ l 1))
           ((> l minmn) nil)
           (tagbody
             (setf k (+ (- minmn l) 1))
             (fdo (i k (+ i 1))
                  ((> i m) nil)
                  (tagbody
                    (fset (fref wa (i) ((1 m))) (fref q (i k) ((1 ldq) (1 m))))
                    (fset (fref q (i k) ((1 ldq) (1 m))) zero)
                   label70))
             (fset (fref q (k k) ((1 ldq) (1 m))) one)
             (if (= (fref wa (k) ((1 m))) zero) (go label110))
             (fdo (j k (+ j 1))
                  ((> j m) nil)
                  (tagbody
                    (setf sum zero)
                    (fdo (i k (+ i 1))
                         ((> i m) nil)
                         (tagbody
                           (setf sum
                                   (+ sum
                                      (* (fref q (i j) ((1 ldq) (1 m)))
                                         (fref wa (i) ((1 m))))))
                          label80))
                    (setf temp (/ sum (fref wa (k) ((1 m)))))
                    (fdo (i k (+ i 1))
                         ((> i m) nil)
                         (tagbody
                           (fset (fref q (i j) ((1 ldq) (1 m)))
                                 (- (fref q (i j) ((1 ldq) (1 m)))
                                    (* temp (fref wa (i) ((1 m))))))
                          label90))
                   label100))
            label110
            label120))
      (go end_label)
      '""
      '"     last card of subroutine qform."
      '""
     end_label
      (return (values m n q ldq wa)))))

