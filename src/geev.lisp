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
;;; $Id: geev.lisp,v 1.2 2000/05/08 17:19:18 rtoy Exp $
;;;
;;; $Log: geev.lisp,v $
;;; Revision 1.2  2000/05/08 17:19:18  rtoy
;;; Changes to the STANDARD-MATRIX class:
;;; o The slots N, M, and NXM have changed names.
;;; o The accessors of these slots have changed:
;;;      NROWS, NCOLS, NUMBER-OF-ELEMENTS
;;;   The old names aren't available anymore.
;;; o The initargs of these slots have changed:
;;;      :nrows, :ncols, :nels
;;;
;;; Revision 1.1  2000/04/14 00:11:12  simsek
;;; o This file is adapted from obsolete files 'matrix-float.lisp'
;;;   'matrix-complex.lisp' and 'matrix-extra.lisp'
;;; o Initial revision.
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MATLISP")

(use-package "BLAS")
(use-package "LAPACK")
(use-package "FORTRAN-FFI-ACCESSORS")

(export '(eig
	  geev))

(defun eig (a &optional (job :nn))
  "
 Syntax
 ======
 (EIG a [job])

 Purpose
 =======
 Computes the eigenvalues and left/right eigenvector of A.

 EIG is an alias for GEEV, for more help see GEEV.
"
  (geev a job))
 
(defgeneric geev (a &optional job)
  (:documentation
   "
 Syntax
 ======
 (GEEV a [job])

 Purpose:
 ========
 Computes the eigenvalues and left/right eigenvectors of A.

 For an NxN matrix A, its eigenvalues are denoted by:

              lambda(i),   j = 1 ,..., N
 
 The right eigenvectors of A are denoted by v(i) where:

                    A * v(i) = lambda(i) * v(i)

 The left eigenvectors of A are denoted by u(i) where:

                    H                      H
                 u(i) * A = lambda(i) * u(i)

 In matrix notation:
                             -1
                    A = V E V

           and
                          -1
                         H       H
                    A = U    E  U

 where lambda(i) is the ith diagonal of the diagonal matrix E,
 v(i) is the ith column of V and u(i) is the ith column of U.
 
 The computed eigenvectors are normalized to have Euclidean norm
 equal to 1 and largest component real.
 
 Return Values:
 ==============

     JOB            Return Values
 ------------------------------------------------------------------
     :NN (default)  [1] (DIAG E)       An Nx1 vector of eigenvalues
                    [2] INFO       

     :VN or T       [1] V          
                    [2] E                                             
                    [3] INFO       

     :NV            [1] E          
                    [2] U          
                    [3] INFO       

     :VV            [1] V          
                    [2] E
                    [3] U          
                    [3] INFO       
      
  where INFO is T if successful, NIL otherwise.
"))


(defmethod geev :before ((a standard-matrix) &optional (job :NN))
  (if (not (square-matrix-p a))
      (error "argument A given to GEEV is not a square matrix")
    (if (not (member job '(:nn nn :vn vn :nv nv :vv vv t)))
	(error "argument JOB given to GEEV is not recognized"))))

(defun geev-fix-up-eigvec (n real-eig-p eigval eigvec)
  (if real-eig-p
      (make-instance 'real-matrix :nrows n :ncols n :store eigvec)
      ;; We have to carefully handle complex-valued eigenvectors and eigenvalues
      (let ((evec (make-complex-matrix n n)))
	(do ((col 0 (incf col))
	     (posn 0))
	    ((>= col n) evec)
	  (cond ((zerop (aref eigval col))
		 (dotimes (row n)
		   (setf (matrix-ref evec row col) (aref eigvec posn))
		   (incf posn)))
		(t
		 (dotimes (row n)
		   (let ((next-posn (+ posn n)))
		     (setf (matrix-ref evec row col)
			   (complex (aref eigvec posn) (aref eigvec next-posn)))
		     (setf (matrix-ref evec row (1+ col))
			   (complex (aref eigvec posn) (- (aref eigvec next-posn))))
		     (incf posn)))
		 ;; Skip over the next column, which we've already used
		 (incf col)
		 (incf posn n)))))))
  
(defun geev-fix-up-eigen (n wr wi vr vl left-eig right-eig)
  (let ((res nil)
	;; Eigenvalues are real unless the max of wi is not zero.
	(real-eig (zerop (aref wi (1- (blas::idamax n wi 1))))))

    (when right-eig
      (push (geev-fix-up-eigvec n real-eig wi vr) res))

    (if real-eig
	(if (or right-eig left-eig)
	    (let ((eigval (make-real-matrix n n)))
	      (dotimes (k n)
		(setf (matrix-ref eigval k k) (aref wr k)))
	      (push eigval res))
	  (let ((eigval (make-real-matrix n 1)))
	    (dotimes (k n)
	      (setf (matrix-ref eigval k) (aref wr k)))
	    (push eigval res)))
      (if (or right-eig left-eig)
	  (let ((eigval (make-complex-matrix n n)))
	    (dotimes (k n)
	      (setf (matrix-ref eigval k k) (complex (aref wr k) (aref wi k))))
	    (push eigval res))
	(let ((eigval (make-complex-matrix n 1)))
	  (dotimes (k n)
	    (setf (matrix-ref eigval k) (complex (aref wr k) (aref wi k))))
	  (push eigval res))))

    (when left-eig
      (push (geev-fix-up-eigvec n real-eig wi vl) res))

    (push t res)
    (values-list (nreverse res))))
 

(defmethod geev ((a real-matrix) &optional (job :NN))
  (let* ((n (nrows a))
	 (a (copy a))
	 (xxx (make-array 1 :element-type 'real-matrix-element-type))
	 (wr (make-array n :element-type 'real-matrix-element-type))
	 (wi (make-array n :element-type 'real-matrix-element-type)))

    (declare (type fixnum n)
	     (type (simple-array real-matrix-element-type (*)) xxx wr wi))

    (case job
      (:nn
          (let* ((lwork (* 3 n))
		 (work (make-array lwork :element-type 'real-matrix-element-type)))

	    (multiple-value-bind (a wr wi vl vr work info)
		(dgeev "N"       ;; JOBVL
		       "N"       ;; JOBVR
		       n         ;; N
		       (store a) ;; A
		       n         ;; LDA
		       wr        ;; WR
		       wi        ;; WI
		       xxx        ;; VL
		       1         ;; LDVL
		       xxx        ;; VR
		       1         ;; LDVR
		       work      ;; WORK
		       lwork     ;; LWORK
		       0 )       ;; INFO
	      (declare (ignore a work))
	      (if (zerop info)
		  (geev-fix-up-eigen n wr wi vr vl nil nil)
		(values nil nil)))))

      ((:vn t)
          (let* ((vr (make-array (* n n) :element-type 'real-matrix-element-type))
		 (lwork (* 4 n))
		 (work (make-array lwork :element-type 'real-matrix-element-type)))

	    (multiple-value-bind (a wr wi vl vr work info)
		(dgeev "N"       ;; JOBVL
		       "V"       ;; JOBVR
		       n         ;; N
		       (store a) ;; A
		       n         ;; LDA
		       wr        ;; WR
		       wi        ;; WI
		       xxx       ;; VL
		       1         ;; LDVL
		       vr        ;; VR
		       n         ;; LDVR
		       work      ;; WORK
		       lwork     ;; LWORK
		       0 )       ;; INFO
	      (declare (ignore a work))
	      (if (zerop info)
		  (geev-fix-up-eigen n wr wi vr vl nil t)
		(values nil nil)))))

      (:nv
          (let* ((vl (make-array (* n n) :element-type 'real-matrix-element-type))
		 (lwork (* 4 n))
		 (work (make-array lwork :element-type 'real-matrix-element-type)))

	    (multiple-value-bind (a wr wi vl vr work info)
		(dgeev "V"       ;; JOBVL
		       "N"       ;; JOBVR
		       n         ;; N
		       (store a) ;; A
		       n         ;; LDA
		       wr        ;; WR
		       wi        ;; WI
		       vl        ;; VL
		       n         ;; LDVL
		       xxx       ;; VR
		       1         ;; LDVR
		       work      ;; WORK
		       lwork     ;; LWORK
		       0 )       ;; INFO
	      (declare (ignore a work))
	      (if (zerop info)
		  (geev-fix-up-eigen n wr wi vr vl t nil)
		(values nil nil)))))

      (:vv
          (let* ((vl (make-array (* n n) :element-type 'real-matrix-element-type))
		 (vr (make-array (* n n) :element-type 'real-matrix-element-type))
		 (lwork (* 4 n))
		 (work (make-array lwork :element-type 'real-matrix-element-type)))

	    (multiple-value-bind (a wr wi vl vr work info)
		(dgeev "V"       ;; JOBVL
		       "V"       ;; JOBVR
		       n         ;; N
		       (store a) ;; A
		       n         ;; LDA
		       wr        ;; WR
		       wi        ;; WI
		       vl        ;; VL
		       n         ;; LDVL
		       vr       ;; VR
		       n         ;; LDVR
		       work      ;; WORK
		       lwork     ;; LWORK
		       0 )       ;; INFO
	      (declare (ignore a work))
	      (if (zerop info)
		  (geev-fix-up-eigen n wr wi vr vl t t)
		(values nil nil)))))


      )))



(defmethod geev ((a complex-matrix) &optional (job :NN))
  (let* ((n (nrows a))
	 (a (copy a))
	 (w (make-complex-matrix-dim n 1))
	 (xxx   (make-array 2 :element-type 'complex-matrix-element-type))
	 (lwork (* 2 n))
	 (work  (make-array (* 2 n) :element-type 'complex-matrix-element-type))
	 (rwork (make-array (* 2 n) :element-type 'real-matrix-element-type)))

    (declare (type fixnum lwork n)
	     (type (simple-array complex-matrix-element-type (*)) xxx work)
	     (type (simple-array real-matrix-element-type (*)) rwork))

    (case job
      (:nn
          (let* ()

	    (multiple-value-bind (store-a store-w store-vl store-vr work info)
		(zgeev "N"       ;; JOBVL
		       "N"       ;; JOBVR
		       n         ;; N
		       (store a) ;; A
		       n         ;; LDA
		       (store w) ;; W
		       xxx        ;; VL
		       1         ;; LDVL
		       xxx        ;; VR
		       1         ;; LDVR
		       work      ;; WORK
		       lwork     ;; LWORK
		       rwork     ;; RWORK
		       0 )       ;; INFO
	      (declare (ignore store-a store-w store-vl store-vr work))
	      (if (zerop info)
		  (values  w t)
		(values nil nil)))))

      ((:vn t)
          (let* ((vr (make-complex-matrix-dim n n)))

	    (multiple-value-bind (store-a store-w  store-vl store-vr work info)
		(zgeev "N"        ;; JOBVL
		       "V"        ;; JOBVR
		       n          ;; N
		       (store a)  ;; A
		       n          ;; LDA
		       (store w)  ;; W
		       xxx        ;; VL
		       1          ;; LDVL
		       (store vr) ;; VR
		       n          ;; LDVR
		       work       ;; WORK
		       lwork      ;; LWORK
		       rwork      ;; RWORK
		       0 )        ;; INFO
	      (declare (ignore store-a store-w store-vl store-vr work))
	      (if (zerop info)
		  (values vr (diag w) t)
		(values nil nil)))))

      (:nv
          (let* ((vl (make-complex-matrix-dim n n)))

	    (multiple-value-bind (store-a store-w  store-vl store-vr work info)
		(zgeev "V"        ;; JOBVL
		       "N"        ;; JOBVR
		       n          ;; N
		       (store a)  ;; A
		       n          ;; LDA
		       (store w)  ;; W
		       (store vl) ;; VL
		       n          ;; LDVL
		       xxx        ;; VR
		       1          ;; LDVR
		       work       ;; WORK
		       lwork      ;; LWORK
		       rwork      ;; RWORK
		       0 )        ;; INFO
	      (declare (ignore store-a store-w store-vl store-vr work))
	      (if (zerop info)
		  (values (diag w) vl t)
		(values nil nil)))))

      (:vv
          (let* ((vr (make-complex-matrix-dim n n))
		 (vl (make-complex-matrix-dim n n)))
	    

	    (multiple-value-bind (store-a store-w  store-vl store-vr work info)
		(zgeev "V"        ;; JOBVL
		       "V"        ;; JOBVR
		       n          ;; N
		       (store a)  ;; A
		       n          ;; LDA
		       (store w)  ;; W
		       (store vl) ;; VL
		       n          ;; LDVL
		       (store vr) ;; VR
		       n          ;; LDVR
		       work       ;; WORK
		       lwork      ;; LWORK
		       rwork      ;; RWORK
		       0 )        ;; INFO
	      (declare (ignore store-a store-w store-vl store-vr work))
	      (if (zerop info)
		  (values vr (diag w) vl t)
		(values nil nil)))))


      )))


