;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: :matlisp; Base: 10 -*-
;;;
;;; $Id: geqr.lisp,v 1.2 2001/10/26 13:37:03 rtoy Exp $
;;;
;;; $Log: geqr.lisp,v $
;;; Revision 1.2  2001/10/26 13:37:03  rtoy
;;; Correctly handle the case when rows > cols and we want the [q1 q2]
;;; form.  Fix from M. Koerber.
;;;
;;; Revision 1.1  2001/10/25 21:51:58  rtoy
;;; Initial revision for QR routines.
;;;

(in-package "MATLISP")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up the standard user interface for calling the QR decomposition
(defun qr! (a &optional (skinny t))
  "
  SYNTAX 
  ======
  (QR! A [SKINNY])

  PURPOSE
  =======
  Compute the QR decomposition of A.  See QR.  

  NOTE:  THIS IS A DESTRUCTIVE VERSION.  THE MATRIX A IS OVERWRITTEN WITH Q.
         USE THIS ROUTINE ONLY IF A IS VERY LARGE AND YOU DON'T CARE ABOUT
         IT AFTER THE CALL."

  (cond
   (skinny
    (geqr! a))
   (t
    ;; Okay ... A [Q1 Q2] form was requested, but this only makes sense
    ;; when ROWS > COLS.  When ROWS <= COLS, GEQR! is sufficient
    (if (<= (number-of-rows a) (number-of-cols a))
	(geqr! a)

      ;; Else we do a QR and an SVD to pick up the null-space of A
      (multiple-value-bind (q1 r1)
	  (geqr! a)
	(values 
	 (join q1
	       (matrix-ref (svd a :a)	; the Q2 part
			   (seq 0 (1- (number-of-rows a)))
			   (seq (number-of-cols a) (1- (number-of-rows a))))
	       :horizontal)
	 (join r1 (zeros (- (number-of-rows a) (number-of-cols a))
			 (number-of-cols a)) :vertical)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun qr (a &optional (skinny t))
  "
  SYNTAX 
  ======
  (QR A [SKINNY])
  
  INPUT
  -----
  A       A Matlisp matrix of size M x N
  SKINNY  T of NIL, Default is T which means return Q1 and R1 only.
  
  OUTPUT: (VALUES Q R)
  ------
  Q  A Matlisp matrix of size M x M
  R  A Matlisp matrix of size M x N
  
  PURPOSE
  =======
  Compute the QR decompostion of A.  A = Q*R.  When M > N, the QR decomposition
  can be written as
  A = [Q1 Q2] * [ R1 ]
                [ -- ]
                [ 0  ]
  When SKINNY == T only Q1 and R1 is return. Otherwise Q and R are returned.
  Note that when SKINNY == NULL the value of Q2 is taken from the SVD of A;
  this matches the results of OCTAVE and MATLAB.
"

  (cond
   (skinny
    (geqr! (copy a)))
   (t
    ;; Okay ... A [Q1 Q2] form was requested, but this only makes sense
    ;; when ROWS > COLS.  When ROWS <= COLS, GEQR! is sufficient
    (if (<= (number-of-rows a) (number-of-cols a))
	(geqr! (copy a))

      ;; Else we do a QR and an SVD to pick up the null-space of A
      (multiple-value-bind (q1 r1)
	  (geqr! (copy a))
	(values 
	 (join q1
	       (matrix-ref (svd a :a)	; the Q2 part
			   (seq 0 (1- (number-of-rows a)))
			   (seq (number-of-cols a) (1- (number-of-rows a))))
	       :horizontal)
	 (join r1 (zeros (- (number-of-rows a) (number-of-cols a))
			 (number-of-cols a)) :vertical)))))))

