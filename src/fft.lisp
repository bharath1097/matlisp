;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: :matlisp; Base: 10 -*-
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copyright (c) 2000 The Regents of the University of California.
;;; All rights reserved. 
;;; 
;;; Permission is hereby granted, without written agreement and without
;;; license or royalty fees, to use, copy, modify, and distribute this
;;; software and its documentation for any purpose, provided that the
;;; above copyright notice and the following two paragraphs appear in all
;;; copies of this software.
;;; 
;;; IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY
;;; FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
;;; ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
;;; THE UNIVERSITY OF CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.
;;;
;;; THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
;;; INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE SOFTWARE
;;; PROVIDED HEREUNDER IS ON AN "AS IS" BASIS, AND THE UNIVERSITY OF
;;; CALIFORNIA HAS NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES,
;;; ENHANCEMENTS, OR MODIFICATIONS.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Originally written by Tunc Simsek, Univ. of California, Berkeley
;;; May 5th, 2000
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id: fft.lisp,v 1.3 2000/05/08 17:19:18 rtoy Exp $
;;;
;;; $Log: fft.lisp,v $
;;; Revision 1.3  2000/05/08 17:19:18  rtoy
;;; Changes to the STANDARD-MATRIX class:
;;; o The slots N, M, and NXM have changed names.
;;; o The accessors of these slots have changed:
;;;      NROWS, NCOLS, NUMBER-OF-ELEMENTS
;;;   The old names aren't available anymore.
;;; o The initargs of these slots have changed:
;;;      :nrows, :ncols, :nels
;;;
;;; Revision 1.2  2000/05/05 22:04:13  simsek
;;; o Changed one typo: fftb to ifft
;;;
;;; Revision 1.1  2000/05/05 21:35:54  simsek
;;; o Initial revision
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MATLISP")

(use-package "DFFTPACK")
(use-package "BLAS")
(use-package "LAPACK")
(use-package "FORTRAN-FFI-ACCESSORS")

(export '(fft ffti ifft))

(defgeneric fft (x &optional n wsave)
  (:documentation
  "
  Syntax
  ======
  (FFT x [n] [wsave])

  Purpose
  =======
  Computes the N point discrete Fourier transform (DFT) of X:
   
     For k = 0,...,N-1:            
                               2 pi n
               \\---       -j k ------
                \\                N
    DFT(k) =    /    x(i) e
               /---
            i=0,..,N-1
 
     where the inverse DFT (see IFFT) is:

     For i = 0,...,N-1:            
                                 2 pi k
               \\---         -j i ------
                \\                  N
      X(i) =    /    DFT(k) e
               /---
            k=0,..,N-1
 
  A call to FFT followed by a call to IFFT will multiply the sequency
  by N.

  If X is a vector, it is truncated at the end if it has more 
  than N elements and it is padded with zeros at the end if
  it has less than N elements.

  If X is a matrix, the FFT of each column of X is taken.

  The optional argument defaults to length of X when X is 
  a vector and to the number of rows of X when it is a matrix.

  The optional argument WSAVE, if provided, must be a 
  REAL-MATRIX of length > 4*N+15 as computed by FFTI.

  See IFFT, FFTI
  "))

(defgeneric ifft (x &optional n wsave)
  (:documentation
  "
  Syntax
  ======
  (IFFT x [n] [wsave])

  Purpose
  =======
  Computes the N point inverse discrete Fourier transform (DFT) of X:
   
     For i = 0,...,N-1:            
                                 2 pi k
               \\---         -j i ------
                \\                  N
      IDFT(i) = /      X(k) e
               /---
            k=0,..,N-1

     where the DFT (see FFT) is:

     For k = 0,...,N-1:            
                               2 pi n
               \\---       -j k ------
                \\                N
      X(k) =    /  IDFT(i) e
               /---
            i=0,..,N-1

  A call to FFT followed by a call to IFFT will multiply the sequency
  by N.

  If X is a vector, it is truncated at the end if it has more 
  than N elements and it is padded with zeros at the end if
  it has less than N elements.

  If X is a matrix, the IFFT of each column of X is taken.

  The optional argument defaults to length of X when X is 
  a vector and to the number of rows of X when it is a matrix.

  The optional argument WSAVE, if provided, must be a 
  REAL-MATRIX of length > 4*N+15 as computed by FFTI.

  See FFT, FFTI
  "))

(defun ffti (n &optional wsave)
 "
  Syntax
  ======
  (FFTI n [WSAVE])

  Purpose
  =======
  Initializes the vector WSAVE which is used in FFT and IFFT.
  The prime factorization of N and a tabulation of the
  trigonometric functions are computed and returned in WSAVE.

  The optional argument WSAVE, if provided, must be a REAL-MATRIX
  with length > 4*N+15.  The same WSAVE may be used in FFT and IFFT
  if the arg N given to FFT and IFFT are the same.
"
  (unless (and (integerp n)
	       (> n 0))
    (error "argument N given to FFTI needs to
be an INTEGER > 0"))

  (if wsave
      (progn
	(unless (and (subtypep (type-of wsave) 'real-matrix)
		     (> (number-of-elements wsave) (+ (* 4 n) 15)))
	  (error "argument WSAVE given to FFTI needs
to be a REAL-MATRIX of size > 4*N+15"))
	(zffti n (store wsave))
	wsave)
    (let ((result (make-real-matrix-dim 1 (+ (* 4 n) 15))))
      (zffti n (store result))
      result)))


(defmethod fft :before ((x standard-matrix) &optional n wsave)
  (labels ((wsave-size (i)
		(+ (* 4 i) 15)))
    (if (and n wsave)
	(unless (and (integerp n)
		     (> n 0)
		     (subtypep (type-of wsave) 'real-matrix)
		     (> (number-of-elements wsave) (wsave-size n)))
	  (error "arguments N,WSAVE to FFT need to be
INTEGER > 0 and REAL-MATRIX of size > 4*N+15 respectively"))
      
      (if n
	  (typecase n
	     ((integer 1 *) t)
	     (real-matrix (let ((wsave n)
				(n (nrows x))
				(m (ncols x)))
			    (if (row-or-col-vector-p x)
				(unless (> (number-of-elements wsave) (wsave-size (max n m)))
				  (error "argument WSAVE given to FFT
needs to be a REAL-MATRIX of size > 4*n+15 where n is 
the size of argument X"))
			      (unless (> (number-of-elements wsave) (wsave-size n))
				(error "arguement WSAVE given to FFT
need to be a REAL-MATRIX of size > 4*n+15 where n is the number
of rows in argument X")))))
	     (t (error "second argument to FFT must be
and INTEGER > 0 or a REAL-MATRIX of size > 4*n+15")))))))
  
(defmethod fft ((x standard-matrix) &optional n wsave)
  (let* ((n (or n (if (row-or-col-vector-p x)
		      (max (nrows x) (ncols x))
		    (nrows x))))
	 (wsave (or wsave (ffti n)))
	 (result (cond ((row-vector-p x) 
			(make-complex-matrix-dim 1 n))
		       ((col-vector-p x)
			(make-complex-matrix-dim n 1))
		       (t (make-complex-matrix-dim n (ncols x))))))

    (if (row-or-col-vector-p x)
	(progn
	  (copy! x result)
	  (zfftf n (store result) (store wsave)))

      (dotimes (j (ncols x))
	(declare (type fixnum j))
	 (dotimes (i (nrows x))
	   (declare (type fixnum i))
	   (setf (matrix-ref result i j) (matrix-ref x i j)))
	 (with-vector-data-addresses ((addr-result (store result))
				      (addr-wsave (store wsave)))
	    (incf-sap :complex-double-float addr-result (* j n))
	    (dfftpack::fortran-zfftf n addr-result addr-wsave))))

      result))



(defmethod ifft :before ((x standard-matrix) &optional n wsave)
  (labels ((wsave-size (i)
		(+ (* 4 i) 15)))
    (if (and n wsave)
	(unless (and (integerp n)
		     (> n 0)
		     (subtypep (type-of wsave) 'real-matrix)
		     (> (number-of-elements wsave) (wsave-size n)))
	  (error "arguments N,WSAVE to IFFT need to be
INTEGER > 0 and REAL-MATRIX of size > 4*N+15 respectively"))
      
      (if n
	  (typecase n
	     ((integer 1 *) t)
	     (real-matrix (let ((wsave n)
				(n (nrows x))
				(m (ncols x)))
			    (if (row-or-col-vector-p x)
				(unless (> (number-of-elements wsave) (wsave-size (max n m)))
				  (error "argument WSAVE given to IFFT
needs to be a REAL-MATRIX of size > 4*n+15 where n is 
the size of argument X"))
			      (unless (> (number-of-elements wsave) (wsave-size n))
				(error "arguement WSAVE given to IFFT
need to be a REAL-MATRIX of size > 4*n+15 where n is the number
of rows in argument X")))))
	     (t (error "second argument to IFFT must be
and INTEGER > 0 or a REAL-MATRIX of size > 4*n+15")))))))
  
(defmethod ifft ((x standard-matrix) &optional n wsave)
  (let* ((n (or n (if (row-or-col-vector-p x)
		      (max (nrows x) (ncols x))
		    (nrows x))))
	 (wsave (or wsave (ffti n)))
	 (result (cond ((row-vector-p x) 
			(make-complex-matrix-dim 1 n))
		       ((col-vector-p x)
			(make-complex-matrix-dim n 1))
		       (t (make-complex-matrix-dim n (ncols x))))))

    (if (row-or-col-vector-p x)
	(progn
	  (copy! x result)
	  (zfftb n (store result) (store wsave)))

      (dotimes (j (ncols x))
	(declare (type fixnum j))
	 (dotimes (i (nrows x))
	   (declare (type fixnum i))
	   (setf (matrix-ref result i j) (matrix-ref x i j)))
	 (with-vector-data-addresses ((addr-result (store result))
				      (addr-wsave (store wsave)))
	    (incf-sap :complex-double-float addr-result (* j n))
	    (dfftpack::fortran-zfftb n addr-result addr-wsave))))

      result))
