;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: cl-user; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This is the configuration file for the MATLISP workspace.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id: config.lisp,v 1.1 2000/07/10 22:38:43 simsek Exp $
;;; $Log: config.lisp,v $
;;; Revision 1.1  2000/07/10 22:38:43  simsek
;;; o Initial revision
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package "COMMON-LISP-USER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Turn off compilation messages.  It is annoying to see 
;;; most messages during compilation.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq *compile-print* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Make I,J available as the complex identity.
;;; Use DEFPARAMETER and not DEFVAR so that they
;;; may be redefined.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(unexport '(i j))
(export '(i j))
(defparameter i #c(0.0d0 1.0d0))
(defparameter j #c(0.0d0 1.0d0))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Read all floating points numbers as DOUBLE-FLOAT,
;;; using SINGLE-FLOAT does not account for efficiency
;;; since most machines are optimized for DOUBLE-FLOAT
;;; operations.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq *read-default-float-format* 'double-float)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Make QUIT, EXIT, BYE available in all packages that
;;; use the COMMON-LISP-USER package.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+:cmu
(progn
  (unexport 'quit "COMMON-LISP")
  (export 'quit "COMMON-LISP")
  (unexport '(exit bye))
  (export '(exit bye))

  (unless (fboundp 'bye)
    (defun bye ()
      (common-lisp::quit)))

  (unless (fboundp 'exit)
    (defun exit ()
      (common-lisp::quit))))

#+:allegro
(progn
  (unexport '(quit bye exit))
  (export '(quit bye exit))
  
  (unless (fboundp 'quit)
    (defun quit ()
      (common-lisp-user::exit)))

  (unless (fboundp 'bye)
    (defun bye ()
      (common-lisp-user::exit))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Turn off GC messages for CMU, it is annoying 
;;; during compilation.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+:cmu (setq ext:*gc-verbose* nil) 
#+:allegro (setq excl:*global-gc-behavior* :auto)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Make sure that PCL compiles and evaluates DEFCLASS
;;; and DEFGENERIC forms, otherwise you encounter too
;;; many misleading warnings.
;;;
;;; And similarly for Allegro ...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+:allegro (setq comp:*cltl1-compile-file-toplevel-compatibility-p* t)
#+:allegro (setq excl:*enable-package-locked-errors* nil)

#+:cmu (pushnew 'compile pcl::*defclass-times*)
#+:cmu (pushnew 'compile pcl::*defgeneric-times*)


