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
;;; $Id: join.lisp,v 1.1 2000/04/14 00:11:12 simsek Exp $
;;;
;;; $Log: join.lisp,v $
;;; Revision 1.1  2000/04/14 00:11:12  simsek
;;; o This file is adapted from obsolete files 'matrix-float.lisp'
;;;   'matrix-complex.lisp' and 'matrix-extra.lisp'
;;; o Initial revision.
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MATLISP")

(export 'join)

(defgeneric join (a b &optional position)
  (:documentation
   "
  Syntax
  ======
  (JOIN a b [orientation])

  Purpose
  =======
  Append matrix A and matrix B to form one of the following matrices:
 
    [A | B]     [ A ]
                [---]
                [ B ]
 
  The choice is determined by ORIENTATION, which should take the value
  of either :HORIZONTAL, for the left case above, or :VERTICAL for
  the right case above.  For any other value of ORIENTATION, we try to
  figure it out based on the dimensions of the A and B.  For the
  ambiguous case (A and B are square), we join them horizontally by
  default.
"))
  
(defmethod join :before ((a standard-matrix) (b standard-matrix) &optional orientation)
  (let ((n-a (n a))
	(m-a (m a))
	(n-b (n b))
	(m-b (m b)))

    (case orientation
       (:horizontal (unless (= n-a n-b)
				   (error "Cannot horizontally join matrices with ~d rows and ~d rows"
					  n-a n-b)))
       (:vertical  (unless (= m-a m-b)
				(error "Cannot vertically join matrices with ~d columns and ~d columns"
				       m-a m-b)))
       (t  (unless (or (= n-a n-b)
		       (= m-a m-b))
	     (error "Unable to join in any direction a ~d x ~d matrix with a ~d x ~d matrix"
		    n-a m-a
		    n-b m-b))))))

(defmethod join ((a real-matrix) (b real-matrix) &optional orientation)
  (case orientation
    (:horizontal
     (let* ((nrows (n a))
	    (m-a (m a))
	    (ncols (+ m-a (m b)))
	    (res (make-real-matrix nrows ncols)))
       (declare (fixnum nrows m-a ncols))
       ;; Copy A
       (dotimes (r nrows)
	 (declare (fixnum r))
	 (dotimes (c m-a)
	   (declare (fixnum c))
	   (setf (matrix-ref res r c) (matrix-ref a r c))))
       (dotimes (r nrows)
	 (declare (fixnum r))
	 (dotimes (c (m b))
	   (declare (fixnum c))
	   (setf (matrix-ref res r (+ c m-a)) (matrix-ref b r c))))
       res))
    (:vertical
     (let* ((ncols (m a))
	    (n-a (n a))
	    (nrows (+ n-a (n b)))
	    (res (make-real-matrix nrows ncols)))
       (declare (fixnum nrows n-a ncols))
       ;; Copy A
       (dotimes (r n-a)
	 (declare (fixnum r))
	 (dotimes (c ncols)
	   (declare (fixnum c))
	   (setf (matrix-ref res r c) (matrix-ref a r c))))
       (dotimes (r (n b))
	 (declare (fixnum r))
	 (dotimes (c ncols)
	   (declare (fixnum c))
	   (setf (matrix-ref res (+ r n-a) c) (matrix-ref b r c))))
       res))
    (t
     (cond ((= (n a) (n b))
	    (join a b  :horizontal))
	   ((= (m a) (m b))
	    (join a b  :vertical))
	   (t
	    (error "Unable to determine how to join a ~d X ~d matrix with a ~d x ~d matrix"
		   (n a)
		   (m a)
		   (n b)
		   (m b)))))))


(defmethod join ((a complex-matrix) (b complex-matrix) &optional orientation)
  (case orientation
    (:horizontal
     (let* ((nrows (n a))
	    (m-a (m a))
	    (ncols (+ m-a (m b)))
	    (res (make-complex-matrix nrows ncols)))
       ;; Copy A
       (dotimes (r nrows)
	 (dotimes (c m-a)
	   (setf (matrix-ref res r c) (matrix-ref a r c))))
       (dotimes (r nrows)
	 (dotimes (c (m b))
	   (setf (matrix-ref res r (+ c m-a)) (matrix-ref b r c))))
       res))
    (:vertical
     (let* ((ncols (m a))
	    (n-a (n a))
	    (nrows (+ n-a (n b)))
	    (res (make-complex-matrix nrows ncols)))
       ;; Copy A
       (dotimes (r n-a)
	 (dotimes (c ncols)
	   (setf (matrix-ref res r c) (matrix-ref a r c))))
       (dotimes (r (n b))
	 (dotimes (c ncols)
	   (setf (matrix-ref res (+ r n-a) c) (matrix-ref b r c))))
       res))
    (t
     (cond ((= (n a) (n b))
	    (join a b  :horizontal))
	   ((= (m a) (m b))
	    (join a b  :vertical))
	   (t
	    (error "Unable to determine how to join a ~d X ~d matrix with a ~d x ~d matrix"
		   (n a)
		   (m a)
		   (n b)
		   (m b)))))))

