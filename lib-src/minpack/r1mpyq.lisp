;;; Compiled by f2cl version 2.0 beta on 2002/01/07 at 17:54:12
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls nil)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t))

(in-package "MINPACK")



(use-package :f2cl)

(let ((one 1.0d0))
  (declare (type double-float one))
  (defun r1mpyq (m n a lda v w)
    (declare (type (array double-float (*)) w v a) (type integer4 lda n m))
    (prog ((cos 0.0d0) (sin 0.0d0) (temp 0.0d0) (i 0) (j 0) (nmj 0) (nm1 0))
      (declare (type integer4 nm1 nmj j i) (type double-float temp sin cos))
      (declare
       (ftype (function (array-double-float) (values double-float)) dabs))
      (declare (ftype (function (double-float) (values double-float)) dsqrt))
      '"     **********"
      '""
      '"     subroutine r1mpyq"
      '""
      '"     given an m by n matrix a, this subroutine computes a*q where"
      '"     q is the product of 2*(n - 1) transformations"
      '""
      '"           gv(n-1)*...*gv(1)*gw(1)*...*gw(n-1)"
      '""
      '"     and gv(i), gw(i) are givens rotations in the (i,n) plane which"
      '"     eliminate elements in the i-th and n-th planes, respectively."
      '"     q itself is not given, rather the information to recover the"
      '"     gv, gw rotations is supplied."
      '""
      '"     the subroutine statement is"
      '""
      '"       subroutine r1mpyq(m,n,a,lda,v,w)"
      '""
      '"     where"
      '""
      '"       m is a positive integer input variable set to the number"
      '"         of rows of a."
      '""
      '"       n is a positive integer input variable set to the number"
      '"         of columns of a."
      '""
      '"       a is an m by n array. on input a must contain the matrix"
      '"         to be postmultiplied by the orthogonal matrix q"
      '"         described above. on output a*q has replaced a."
      '""
      '"       lda is a positive integer input variable not less than m"
      '"         which specifies the leading dimension of the array a."
      '""
      '"       v is an input array of length n. v(i) must contain the"
      '"         information necessary to recover the givens rotation gv(i)"
      '"         described above."
      '""
      '"       w is an input array of length n. w(i) must contain the"
      '"         information necessary to recover the givens rotation gw(i)"
      '"         described above."
      '""
      '"     subroutines called"
      '""
      '"       fortran-supplied ... dabs,dsqrt"
      '""
      '"     argonne national laboratory. minpack project. march 1980."
      '"     burton s. garbow, kenneth e. hillstrom, jorge j. more"
      '""
      '"     **********"
      '""
      '"     apply the first set of givens rotations to a."
      '""
      (setf nm1 (- n 1))
      (if (< nm1 1) (go label50))
      (fdo (nmj 1 (+ nmj 1))
           ((> nmj nm1) nil)
           (tagbody
             (setf j (- n nmj))
             (if (> (dabs (fref v (j) ((1 n)))) one)
                 (setf cos (/ one (fref v (j) ((1 n))))))
             (if (> (dabs (fref v (j) ((1 n)))) one)
                 (setf sin (dsqrt (- one (expt cos 2)))))
             (if (<= (dabs (fref v (j) ((1 n)))) one)
                 (setf sin (fref v (j) ((1 n)))))
             (if (<= (dabs (fref v (j) ((1 n)))) one)
                 (setf cos (dsqrt (- one (expt sin 2)))))
             (fdo (i 1 (+ i 1))
                  ((> i m) nil)
                  (tagbody
                    (setf temp
                            (- (* cos (fref a (i j) ((1 lda) (1 n))))
                               (* sin (fref a (i n) ((1 lda) (1 n))))))
                    (fset (fref a (i n) ((1 lda) (1 n)))
                          (+ (* sin (fref a (i j) ((1 lda) (1 n))))
                             (* cos (fref a (i n) ((1 lda) (1 n))))))
                    (fset (fref a (i j) ((1 lda) (1 n))) temp)
                   label10))
            label20))
      '""
      '"     apply the second set of givens rotations to a."
      '""
      (fdo (j 1 (+ j 1))
           ((> j nm1) nil)
           (tagbody
             (if (> (dabs (fref w (j) ((1 n)))) one)
                 (setf cos (/ one (fref w (j) ((1 n))))))
             (if (> (dabs (fref w (j) ((1 n)))) one)
                 (setf sin (dsqrt (- one (expt cos 2)))))
             (if (<= (dabs (fref w (j) ((1 n)))) one)
                 (setf sin (fref w (j) ((1 n)))))
             (if (<= (dabs (fref w (j) ((1 n)))) one)
                 (setf cos (dsqrt (- one (expt sin 2)))))
             (fdo (i 1 (+ i 1))
                  ((> i m) nil)
                  (tagbody
                    (setf temp
                            (+ (* cos (fref a (i j) ((1 lda) (1 n))))
                               (* sin (fref a (i n) ((1 lda) (1 n))))))
                    (fset (fref a (i n) ((1 lda) (1 n)))
                          (+ (* (- sin) (fref a (i j) ((1 lda) (1 n))))
                             (* cos (fref a (i n) ((1 lda) (1 n))))))
                    (fset (fref a (i j) ((1 lda) (1 n))) temp)
                   label30))
            label40))
     label50
      (go end_label)
      '""
      '"     last card of subroutine r1mpyq."
      '""
     end_label
      (return (values m n a lda v w)))))

