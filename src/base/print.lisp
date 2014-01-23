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

(in-package #:matlisp)
;; Routines for printing a tensors/matrices nicely.

(defparameter *print-max-len* 10
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
  (let ((rank (order tensor))
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
			       (print-element tensor (ref tensor (append subs `(,i ,j))) stream)
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
	     (if (or (eq *print-max-len* t) (< i *print-max-len*))
		 (progn
		   (print-element tensor (ref tensor i) stream)
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
    (if (slot-value tensor 'parent-tensor)
	(format stream "~A~,4T:DISPLACED~%" (dimensions tensor))
	(format stream "~A~%" (dimensions tensor)))
    (print-tensor tensor stream)))
