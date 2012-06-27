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
;;; Originally written by Raymond Toy.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id: print.lisp,v 1.9 2010/12/12 02:07:31 rtoy Exp $
;;;
;;; $Log: print.lisp,v $
;;; Revision 1.9  2010/12/12 02:07:31  rtoy
;;; matrix.lisp:
;;;
;;; o Apply patch from Nicolas Neuss for matrices with 0 dimensions.  (See
;;;   <http://sourceforge.net/mailarchive/message.php?msg_id=1124576>)
;;;
;;; print.lisp:
;;; o Apparently the above patch to print was also applied previously.  We
;;;   just fix a bug in printing 0xm matrices.  Just exit early if the
;;;   matrix has no dimensions.
;;;
;;; Revision 1.8  2003/05/31 23:05:39  rtoy
;;; Indent more typically.
;;;
;;; Revision 1.7  2001/06/22 12:52:41  rtoy
;;; Use ALLOCATE-REAL-STORE and ALLOCATE-COMPLEX-STORE to allocate space
;;; instead of using the error-prone make-array.
;;;
;;; Revision 1.6  2001/04/28 13:06:46  rtoy
;;; This is not Fortran.  Instead of printing *'s when the number won't
;;; fit in the given field, print them out!
;;;
;;; Revision 1.5  2001/02/21 19:33:34  simsek
;;; o Added the formatting hack *matrix-indent*.
;;;
;;; Revision 1.4  2000/07/11 18:02:03  simsek
;;; o Added credits
;;;
;;; Revision 1.3  2000/07/11 02:11:56  simsek
;;; o Added support for Allegro CL
;;;
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

;;; Routines for printing a tensors/matrices nicely.

(in-package :matlisp)

(defparameter *print-max-len* 5
"
Maximum number of elements in any particular argument to print.
Set this to T to print all the elements.
")

(defparameter *print-max-args* 5
"
Maximum number of arguments of the tensor to print.
Set this to T to print all the arguments.
")

(defparameter *print-indent* 0
"
Determines how many spaces will be printed before each row
of a matrix (default 0)
")

(defun print-tensor (tensor stream)
  (let ((rank (rank tensor))
	(dims (dimensions tensor))
	(two-print-calls 0))
    (labels ((two-print (tensor subs)
	       (dotimes (i (aref dims (- rank 2)))
		 (format stream (format-to-string "~~~AT" *print-indent*))
		 (if (or (eq *print-max-len* t) (< i *print-max-len*))
		     (progn
		       (dotimes (j (aref dims (- rank 1)))
			 (if (or (eq *print-max-len* t) (< j *print-max-len*))
			     (progn
			       (print-element tensor (tensor-ref tensor (append subs `(,i ,j))) stream)
			       (format stream "~,4T"))
			     (progn
			       (format stream "...")
			       (return nil))))
			 (format stream "~%"))
		     (progn
		       (format stream (format-to-string ".~~%~~~AT:~~%" *print-indent*))
		       (return nil)))))
	     (rec-print (tensor idx subs)
	       (if (< idx (- rank 2))
		   (dotimes (i (aref dims idx) t)
		     (unless (rec-print tensor (1+ idx) (append subs `(,i)))
		       (return nil)))
		   (progn
		     (if (or (eq *print-max-args* t) (< two-print-calls *print-max-args*))
			 (progn
			   (format stream "~A~%" (append subs '(\: \:)))
			   (two-print tensor subs)
			   (format stream "~%")
			   (incf two-print-calls)
			   t)
			 (progn
			   (format stream "~A~%" (make-list rank :initial-element '\:))
			   (format stream (format-to-string "~~~AT..~~%~~~AT::~~%" *print-indent* *print-indent*))
			   nil))))))
			   
	(case rank
	  (1
	   (format stream (format-to-string "~~~AT" *print-indent*))
	   (dotimes (i (aref dims 0))
	     (if (< i *print-max-len*)
		 (progn
		   (print-element tensor (tensor-ref tensor `(,i)) stream)
		   (format stream "~,4T"))
		 (progn
		   (format stream "...")
		   (return nil))))
	   (format stream "~%"))
	  (2
	   (two-print tensor nil))
	  (t
	   (rec-print tensor 0 nil))))))

(defmethod print-object ((tensor standard-tensor) stream)
  (print-unreadable-object (tensor stream :type t)
    (format stream "~A~%" (dimensions tensor))
    (print-tensor tensor stream)))

(defmethod print-object ((tensor standard-matrix) stream)
  (print-unreadable-object (tensor stream :type t)
    (format stream "~A x ~A~%" (nrows tensor) (ncols tensor))
    (print-tensor tensor stream)))
