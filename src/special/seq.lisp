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

(defun range (start end &optional (h 1d0))
  (let ((quo (ceiling (if (> start end) (- start end) (- end start)) h)))
    (if (= quo 0) nil
	(let* ((ret (zeros quo 'real-tensor))
	       (sto (store ret))
	       (h (coerce (if (> start end) (- h) h) 'double-float)))
	  (declare (type (simple-array double-float (*)) sto)
		   (type double-float h))
	  (very-quickly
	    (loop :for i :from 0 :below quo
	       :for ori := (coerce start 'double-float) :then (+ ori h)
	       :do (t/store-set real-tensor ori sto i)))
	  ret))))

(defun linspace (start end &optional (num-points (1+ (abs (- start end)))))
  (let* ((num-points (floor num-points))
	 (h (/ (- end start) (1- num-points))))
    (range start (+ (/ h 2) end) (abs h))))

(defun list-range (start end &optional (h 1))
  (declare (type real start end h))
  (let ((quo (ceiling (if (> start end) (- start end) (- end start)) h)))
    (if (= quo 0) nil
	(let ((h (if (> start end) (- h) h)))
	  (loop :for i :from 0 :below quo
	     :for ori := start :then (+ ori h)
	     :collect ori)))))

(defun list-linspace (start end &optional (num-points (1+ (abs (- start end)))))  
  (let* ((num-points (floor num-points))
	 (h (/ (- end start) (1- num-points))))
    (list-range start (+ h end) (abs h))))
