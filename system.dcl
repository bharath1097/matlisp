;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10 -*-
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
;;; $Id: system.dcl,v 1.5 2000/07/11 02:45:15 simsek Exp $
;;;
;;; $Log: system.dcl,v $
;;; Revision 1.5  2000/07/11 02:45:15  simsek
;;; o Changed version from 1.0a to 1.0b
;;;
;;; Revision 1.4  2000/07/11 02:04:50  simsek
;;; o Added support for Allegro CL
;;; o Moved configuration code to config.lisp
;;;
;;; Revision 1.3  2000/05/05 21:57:33  simsek
;;; o Removed ysmm from matlisp-lapack-wrappers
;;;    we're not doing symmetric matrices yet
;;;
;;; Revision 1.2  2000/05/05 21:34:03  simsek
;;; o Updated defsystem form to include dfftpack stuff
;;;
;;; Revision 1.1  2000/04/13 20:43:15  simsek
;;; o Initial revision.
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package "COMMON-LISP-USER")

(deflogicalpath "matlisp")

(eval-when (load eval compile)
(defparameter *matlisp-version* "1.0b")
#-(or :cmu :allegro) (error 
		      "MATLISP version ~a requires CMUCL or ALLEGRO CL" 
		      *matlisp-version*)
(defun matlisp-version () *matlisp-version*)
(defun matlisp-herald () (format nil "    MATLISP/~a" (matlisp-version)))
#+:cmu (setf (getf ext:*herald-items* :matlisp)
	     (list (matlisp-herald))))

(require "FORTRAN-FFI-ACCESSORS" "matlisp:packages")
(require "BLAS" "matlisp:packages")
(require "LAPACK" "matlisp:packages")
(require "DFFTPACK" "matlisp:packages")
(require "MATLISP" "matlisp:packages")
(require "MAKE" "matlisp:defsystem")

(mk::defsystem lazy-loader
      :source-pathname "matlisp:lib;"
      :source-extension "lisp"
      :binary-pathname "matlisp:bin;"
      :components
      ((:file "lazy-loader"
	:load-only t)))

(mk::defsystem matlisp
      :source-pathname "matlisp:src;"
      :source-extension "lisp"
      :binary-pathname "matlisp:bin;"
      :depends-on ("lazy-loader")
      :components
      ((:module "foreign-interface"
	:source-pathname ""
	:source-extension "lisp"
	:binary-pathname ""
	:components (#+:cmu "ffi-cmu"
			#+:allegro "ffi-acl"))
       (:module "foreign-functions"
	:source-pathname ""
	:source-extension "lisp"
	:binary-pathname ""
	:depends-on ("foreign-interface")
	:components ("blas"
		     "lapack"
		     #-:mswindows "dfftpack"))
       (:module "matlisp-essentials"
	:source-pathname ""
	:source-extension "lisp"
	:binary-pathname ""
	:depends-on ("foreign-interface" 
		     "foreign-functions")
	:components ("matrix"
		     "ref"
		     "print"
		     "copy"))

       (:module "matlisp-blas-wrappers"
	:source-pathname ""
	:source-extension "lisp"
	:binary-pathname ""
	:depends-on ("foreign-interface" 
		     "foreign-functions"
		     "matlisp-essentials")
	:components ("axpy"
		     "scal"
		     "swap"
		     "gemm"))

       (:module "matlisp-lapack-wrappers"
	:source-pathname ""
	:source-extension "lisp"
	:binary-pathname ""
	:depends-on ("foreign-interface" 
		     "foreign-functions"
		     "matlisp-essentials")
	:components ("gesv"
		     "geev"
		     "getrf"))

       (:module "matlisp-functions"
        :source-pathname ""
	:source-extension "lisp"
	:binary-pathname ""
	:depends-on ("foreign-interface"
		     "foreign-functions"
		     "matlisp-essentials"
		     "matlisp-blas-wrappers"
		     "matlisp-lapack-wrappers")
	:components ("compat"
		     "help"
		     "diag"
		     "special"
		     "reader"
		     "trans"
		     "realimag"
		     "reshape"
		     "join"
		     "svd"
		     "sum"
		     "norm"
		     "dot"
		     "trace"
		     "seq"
		     "vec"
		     "map"
		     "mplus"
		     "mminus"
		     "mtimes"
		     "mdivide"
		     "msqrt"
		     #-:mswindows "fft"))))

