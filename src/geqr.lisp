;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: :matlisp; Base: 10 -*-
;;;
;;; $Id: geqr.lisp,v 1.1 2001/10/25 21:51:58 rtoy Exp $
;;;
;;; $Log: geqr.lisp,v $
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
    (multiple-value-bind (q1 r1)
	(geqr! a)
      (values 
       (join q1
	     (matrix-ref (svd a :a)	; the Q2 part
			 (seq 0 (1- (number-of-rows a)))
			 (seq (number-of-cols a) (1- (number-of-rows a))))
	     :horizontal)
       (join r1 (zeros (- (number-of-rows a) (number-of-cols a))
		       (number-of-cols a)) :vertical))))))
    

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
    (multiple-value-bind (q1 r1)
	(geqr! (copy a))
      (values 
       (join q1
	     (matrix-ref (svd a :a)	; the Q2 part
			 (seq 0 (1- (number-of-rows a)))
			 (seq (number-of-cols a) (1- (number-of-rows a))))
	     :horizontal)
       (join r1 (zeros (- (number-of-rows a) (number-of-cols a))
		       (number-of-cols a)) :vertical))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up the methods required to handle general matricies of Real
;; and complex types.  There are numerous other special cases, but
;; they will not be considered for this first release.  mak
(defgeneric geqr! (a)
  (:documentation
   "
 Syntax
 ======
 (GEQR! a)

 Purpose:
 ========

 Use QR or QR! for access to this routine.

 Computes the QR factorization of an M-by-N matrix A: A = Q * R where
 R is an M-by-N upper trapezoidal (triangular) matrix and Q is
 M-by-M unitary matrix.

 Return Values:
 ==============

   [1] Q
   [2] R
      
 If the factorization can not be done, Q and R are set to NIL.

 NOTE:  THIS FUNCTION IS DESTRUCTIVE.
"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; One could simply use LWORK = (MAX 1 N), but this call might result
;; in some optimization in performance.  For small matricies this is
;; probably a 'no-net-gain' operation...but I seldom use small matricies
;; in my work ;-) ... mak
(let ((xx (allocate-real-store 1))
      (work (allocate-real-store 1)))

  (defun dgeqrf-workspace-inquiry (m n)
    (multiple-value-bind (store-a store-tau store-work lwork info)
	(lapack::dgeqrf m n xx m xx work -1 0)

      (declare (ignore store-a store-tau store-work lwork info))

      (values (ceiling (realpart (aref work 0)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((xx (allocate-complex-store 1))
      (work (allocate-complex-store 1)))

  (defun zgeqrf-workspace-inquiry (m n)

    (multiple-value-bind (store-a store-tau store-work lwork info)
	(lapack::zgeqrf m n xx m xx work -1 0)

      (declare (ignore store-a store-tau store-work lwork info))
      
      (values (ceiling (realpart (aref work 0)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Okay...now we build up the specific method for real and comples
(defmethod geqr! ((a real-matrix))

  (let* ((m (nrows a))
	 (n (ncols a))
	 (k (min m n))			; THESE ROUTINES ONLY RETURN A MINIMUM Q!
	 (tau (allocate-real-store k))	; reflection factors
	 (lwork (dgeqrf-workspace-inquiry m n))	; optimum work array size
	 (work (allocate-real-store lwork))) ; and the work area

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Do the Householder portion of the decomposition
    (multiple-value-bind (q-r new-tau new-work info)
	(lapack::dgeqrf m n (store a) m tau work lwork 0)

      (declare (ignore new-work))
      ;; Q-R and NEW-TAU aren't needed either since the (STORE A) and WORK
      ;; get modified 

      (if (not (zerop info))
	  ;; If INFO is not zero, then an error occured.  Return Nil
	  ;; for the Q and R and print a warning
	  (progn (warn "QR Decomp failed:  Argument ~d in call to DGEQRF is bad" (- info))
		 (values nil nil))

	;; If we are here, then INFO == 0 and all is well...
	(let ((r (make-real-matrix k n)))
	  ;; Extract the matrix R from Q-R
	  (dotimes (row k)
	    (loop for col from row below n do
		  (setf (matrix-ref r row col)
			(aref q-r (fortran-matrix-indexing row col m)))))

	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ;; Now compute Q via DORGQR and return.  This is always
	  ;; the economy representation of Q.  I.e., Q1 in Q = [Q1 Q2]
	  (multiple-value-bind (new-q-r new-tau new-work info)
	      (lapack::dorgqr m k k q-r m new-tau work lwork 0)

	    (declare (ignore new-work new-tau))

	    (if (not (zerop info))
		(progn (warn "Error in DORGQR in argument ~d.  Returning nil." (- info))
		       (values nil nil))

		(let ((q (make-real-matrix m k)))

		  (dotimes (row m)
		    (dotimes (col k)
		      (setf (matrix-ref q row col)
			    (aref new-q-r (fortran-matrix-indexing row col m)))))

		  ;; We're done!
		  (values q r)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod geqr! ((a complex-matrix))

  (let* ((m (nrows a))
	 (n (ncols a))
	 (k (min m n))			; THESE ROUTINES ONLY RETURN A MINIMUM Q!
	 (tau (allocate-complex-store k))	; reflection factors
	 (lwork (zgeqrf-workspace-inquiry m n))	; optimum work array size
	 (work (allocate-complex-store lwork))) ; and the work area

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Do the Householder portion of the decomposition
    (multiple-value-bind (q-r new-tau new-work info)
	(lapack::zgeqrf m n (store a) m tau work lwork 0)

      (declare (ignore new-work))
      ;; Q-R and NEW-TAU aren't needed either since the (STORE A) and WORK
      ;; get modified 

      (if (not (zerop info))
	  ;; If INFO is not zero, then an error occured.  Return Nil
	  ;; for the Q and R and print a warning
	  (progn (warn "QR Decomp failed:  Argument ~d in call to DGEQRF is bad" (- info))
		 (values nil nil))

	;; If we are here, then INFO == 0 and all is well...
	(let ((r (make-complex-matrix k n))
	      (idx-fortran 0))

	  ;; Extract the matrix R from Q-R
	  (dotimes (row k)
	    (loop for col from row below n do
		  (setf idx-fortran (fortran-complex-matrix-indexing row col m)
			(matrix-ref r row col) (complex (aref q-r idx-fortran)
							(aref q-r (1+ idx-fortran))))))
	  
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ;; Now compute Q via DORGQR and return.  This is always
	  ;; the economy representation of Q.  I.e., Q1 in Q = [Q1 Q2]
	  (multiple-value-bind (new-q-r new-tau new-work info)
	      (lapack::zungqr m k k q-r m new-tau work lwork 0)

	    (declare (ignore new-work new-tau))

	    (if (not (zerop info))
		(progn (warn "Error in DORGQR in argument ~d.  Returning nil." (- info))
		       (values nil nil))

		(let ((q (make-complex-matrix m k)))

		  (dotimes (row m)
		    (dotimes (col k)
		      (setf idx-fortran (fortran-complex-matrix-indexing row col m)
			    (matrix-ref q row col) (complex (aref new-q-r idx-fortran)
							    (aref new-q-r (1+ idx-fortran))))))

		  ;; We're done!
		  (values q r)))))))))


