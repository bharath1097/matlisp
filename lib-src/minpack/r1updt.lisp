;;; Compiled by f2cl version 2.0 beta on 2002/01/07 at 17:54:12
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls nil)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t))

(in-package "MINPACK")



(use-package :f2cl)

(let ((one 1.0d0) (p5 0.5d0) (p25 0.25d0) (zero 0.0d0))
  (declare (type double-float zero p25 p5 one))
  (defun r1updt (m n s ls u v w sing)
    (declare (type logical sing)
             (type (array double-float (*)) w v u s)
             (type integer4 ls n m))
    (prog ((cos 0.0d0) (cotan 0.0d0) (giant 0.0d0) (sin 0.0d0) (tan 0.0d0)
           (tau 0.0d0) (temp 0.0d0) (i 0) (j 0) (jj 0) (l 0) (nmj 0) (nm1 0))
      (declare (type integer4 nm1 nmj l jj j i)
               (type double-float temp tau tan sin giant cotan cos))
      (declare
       (ftype (function (integer4) (values double-float &rest t)) dpmpar))
      (declare
       (ftype
        (function (or double-float array-double-float) (values double-float))
        dabs))
      (declare (ftype (function (double-float) (values double-float)) dsqrt))
      '"     **********"
      '""
      '"     subroutine r1updt"
      '""
      '"     given an m by n lower trapezoidal matrix s, an m-vector u,"
      '"     and an n-vector v, the problem is to determine an"
      '"     orthogonal matrix q such that"
      '""
      '"                   t"
      '"           (s + u*v )*q"
      '""
      '"     is again lower trapezoidal."
      '""
      '"     this subroutine determines q as the product of 2*(n - 1)"
      '"     transformations"
      '""
      '"           gv(n-1)*...*gv(1)*gw(1)*...*gw(n-1)"
      '""
      '"     where gv(i), gw(i) are givens rotations in the (i,n) plane"
      '"     which eliminate elements in the i-th and n-th planes,"
      '"     respectively. q itself is not accumulated, rather the"
      '"     information to recover the gv, gw rotations is returned."
      '""
      '"     the subroutine statement is"
      '""
      '"       subroutine r1updt(m,n,s,ls,u,v,w,sing)"
      '""
      '"     where"
      '""
      '"       m is a positive integer input variable set to the number"
      '"         of rows of s."
      '""
      '"       n is a positive integer input variable set to the number"
      '"         of columns of s. n must not exceed m."
      '""
      '"       s is an array of length ls. on input s must contain the lower"
      '"         trapezoidal matrix s stored by columns. on output s contains"
      '"         the lower trapezoidal matrix produced as described above."
      '""
      '"       ls is a positive integer input variable not less than"
      '"         (n*(2*m-n+1))/2."
      '""
      '"       u is an input array of length m which must contain the"
      '"         vector u."
      '""
      '"       v is an array of length n. on input v must contain the vector"
      '"         v. on output v(i) contains the information necessary to"
      '"         recover the givens rotation gv(i) described above."
      '""
      '"       w is an output array of length m. w(i) contains information"
      '"         necessary to recover the givens rotation gw(i) described"
      '"         above."
      '""
      '"       sing is a logical output variable. sing is set true if any"
      '"         of the diagonal elements of the output s are zero. otherwise"
      '"         sing is set false."
      '""
      '"     subprograms called"
      '""
      '"       minpack-supplied ... dpmpar"
      '""
      '"       fortran-supplied ... dabs,dsqrt"
      '""
      '"     argonne national laboratory. minpack project. march 1980."
      '"     burton s. garbow, kenneth e. hillstrom, jorge j. more,"
      '"     john l. nazareth"
      '""
      '"     **********"
      '""
      '"     giant is the largest magnitude."
      '""
      (setf giant (dpmpar 3))
      '""
      '"     initialize the diagonal element pointer."
      '""
      (setf jj (- (truncate (* n (+ (- (* 2 m) n) 1)) 2) (- m n)))
      '""
      '"     move the nontrivial part of the last column of s into w."
      '""
      (setf l jj)
      (fdo (i n (+ i 1))
           ((> i m) nil)
           (tagbody
             (fset (fref w (i) ((1 m))) (fref s (l) ((1 ls))))
             (setf l (+ l 1))
            label10))
      '""
      '"     rotate the vector v into a multiple of the n-th unit vector"
      '"     in such a way that a spike is introduced into w."
      '""
      (setf nm1 (- n 1))
      (if (< nm1 1) (go label70))
      (fdo (nmj 1 (+ nmj 1))
           ((> nmj nm1) nil)
           (tagbody
             (setf j (- n nmj))
             (setf jj (- jj (+ (- m j) 1)))
             (fset (fref w (j) ((1 m))) zero)
             (if (= (fref v (j) ((1 n))) zero) (go label50))
             '""
             '"        determine a givens rotation which eliminates the"
             '"        j-th element of v."
             '""
             (if (>= (dabs (fref v (n) ((1 n)))) (dabs (fref v (j) ((1 n)))))
                 (go label20))
             (setf cotan (/ (fref v (n) ((1 n))) (fref v (j) ((1 n)))))
             (setf sin (/ p5 (dsqrt (+ p25 (* p25 (expt cotan 2))))))
             (setf cos (* sin cotan))
             (setf tau one)
             (if (> (* (dabs cos) giant) one) (setf tau (/ one cos)))
             (go label30)
            label20
             (setf tan (/ (fref v (j) ((1 n))) (fref v (n) ((1 n)))))
             (setf cos (/ p5 (dsqrt (+ p25 (* p25 (expt tan 2))))))
             (setf sin (* cos tan))
             (setf tau sin)
            label30
             '""
             '"        apply the transformation to v and store the information"
             '"        necessary to recover the givens rotation."
             '""
             (fset (fref v (n) ((1 n)))
                   (+ (* sin (fref v (j) ((1 n))))
                      (* cos (fref v (n) ((1 n))))))
             (fset (fref v (j) ((1 n))) tau)
             '""
             '"        apply the transformation to s and extend the spike in w."
             '""
             (setf l jj)
             (fdo (i j (+ i 1))
                  ((> i m) nil)
                  (tagbody
                    (setf temp
                            (- (* cos (fref s (l) ((1 ls))))
                               (* sin (fref w (i) ((1 m))))))
                    (fset (fref w (i) ((1 m)))
                          (+ (* sin (fref s (l) ((1 ls))))
                             (* cos (fref w (i) ((1 m))))))
                    (fset (fref s (l) ((1 ls))) temp)
                    (setf l (+ l 1))
                   label40))
            label50
            label60))
     label70
      '""
      '"     add the spike from the rank 1 update to w."
      '""
      (fdo (i 1 (+ i 1))
           ((> i m) nil)
           (tagbody
             (fset (fref w (i) ((1 m)))
                   (+ (fref w (i) ((1 m)))
                      (* (fref v (n) ((1 n))) (fref u (i) ((1 m))))))
            label80))
      '""
      '"     eliminate the spike."
      '""
      (setf sing %false%)
      (if (< nm1 1) (go label140))
      (fdo (j 1 (+ j 1))
           ((> j nm1) nil)
           (tagbody
             (if (= (fref w (j) ((1 m))) zero) (go label120))
             '""
             '"        determine a givens rotation which eliminates the"
             '"        j-th element of the spike."
             '""
             (if (>= (dabs (fref s (jj) ((1 ls)))) (dabs (fref w (j) ((1 m)))))
                 (go label90))
             (setf cotan (/ (fref s (jj) ((1 ls))) (fref w (j) ((1 m)))))
             (setf sin (/ p5 (dsqrt (+ p25 (* p25 (expt cotan 2))))))
             (setf cos (* sin cotan))
             (setf tau one)
             (if (> (* (dabs cos) giant) one) (setf tau (/ one cos)))
             (go label100)
            label90
             (setf tan (/ (fref w (j) ((1 m))) (fref s (jj) ((1 ls)))))
             (setf cos (/ p5 (dsqrt (+ p25 (* p25 (expt tan 2))))))
             (setf sin (* cos tan))
             (setf tau sin)
            label100
             '""
             '"        apply the transformation to s and reduce the spike in w."
             '""
             (setf l jj)
             (fdo (i j (+ i 1))
                  ((> i m) nil)
                  (tagbody
                    (setf temp
                            (+ (* cos (fref s (l) ((1 ls))))
                               (* sin (fref w (i) ((1 m))))))
                    (fset (fref w (i) ((1 m)))
                          (+ (* (- sin) (fref s (l) ((1 ls))))
                             (* cos (fref w (i) ((1 m))))))
                    (fset (fref s (l) ((1 ls))) temp)
                    (setf l (+ l 1))
                   label110))
             '""
             '"        store the information necessary to recover the"
             '"        givens rotation."
             '""
             (fset (fref w (j) ((1 m))) tau)
            label120
             '""
             '"        test for zero diagonal elements in the output s."
             '""
             (if (= (fref s (jj) ((1 ls))) zero) (setf sing %true%))
             (setf jj (+ jj (+ (- m j) 1)))
            label130))
     label140
      '""
      '"     move w back into the last column of the output s."
      '""
      (setf l jj)
      (fdo (i n (+ i 1))
           ((> i m) nil)
           (tagbody
             (fset (fref s (l) ((1 ls))) (fref w (i) ((1 m))))
             (setf l (+ l 1))
            label150))
      (if (= (fref s (jj) ((1 ls))) zero) (setf sing %true%))
      (go end_label)
      '""
      '"     last card of subroutine r1updt."
      '""
     end_label
      (return (values m n s ls u v w sing)))))

