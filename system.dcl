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
;;;  If you're a first time user, please browse through the documentation
;;;  in this file.  The impatient, however, may simply:
;;;
;;;      from the shell prompt:
;;;
;;;                 $ ./configure
;;;                 $ make
;;;
;;;      and from within lisp:
;;;
;;;                (load "system.dcl")
;;;                (matlisp:load-matlisp)
;;;
;;;      or,
;;;
;;;               (load "start.lisp")
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id: system.dcl,v 1.1 2000/04/13 20:43:15 simsek Exp $
;;;
;;; $Log: system.dcl,v $
;;; Revision 1.1  2000/04/13 20:43:15  simsek
;;; o Initial revision.
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  The following commands are optional and are by preference of the
;;;  author, they may freely be disabled/removed.
;;;
;;;  Their purpose is to stop CMUCL from printing out GC operations,
;;;  to ensure that DEFCLASS and DEFGENERIC are evaluated at 
;;;  compile time  etc ...
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "COMMON-LISP")

(export 'quit)

(eval-when (load eval compile)
    (setf *read-default-float-format* 'double-float)
    (setf *compile-print* nil)
    #+:cmu (setf ext::*gc-verbose* nil)
    #+:cmu (pushnew 'compile pcl::*defclass-times*)
    #+:cmu (pushnew 'compile pcl::*defgeneric-times*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  You may check whether MATLISP is loaded to your system with:
;;;
;;;              (member :matlisp *features*)
;;;
;;;  MATLISP exists in the "MATLISP" package:
;;;
;;;              (find-package "MATLISP") 
;;;
;;;  To check for a particular version, say 2.3.2 you will need to do:
;;;
;;;              (and (member :matlisp *features*)
;;;                   (string= (matlisp::matlisp-version) "2.3.2"))
;;;
;;;  Once this file is loaded the MATLISP directory/files will be accessible
;;;  via the logical pathname "matlisp", for example:
;;;
;;;              (load "matlisp:src;swap.lisp")
;;;
;;;  MATLISP requires defsystem to load/compile, see below.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(pushnew :matlisp *features*)

(unless (find-package "MATLISP")
  (defpackage "MATLISP"
    (:use "COMMON-LISP")
    (:nicknames "MATRIX" "M")))

(in-package "MATLISP")

(export '(matlisp-version
	  matlisp-herald
	  save-matlisp
	  load-matlisp))


(eval-when (load eval compile)
(defparameter *matlisp-version* "1.0a"))

#-:cmu
(eval-when (load eval compile)
    (error "MATLISP version ~a requires CMUCL" *matlisp-version*))

(defun matlisp-version ()
  *matlisp-version*)
(defun matlisp-herald ()
  (format nil "    MATLISP/~a" (matlisp-version)))

#+:cmu
(defmacro save-matlisp (core-file-name)
  "
  Syntax
  ======
  (SAVE-MATLISP core-file-name)

  Purpose
  =======
  Saves a Lisp core image with MATLISP.
  CORE-FILE-NAME should be a string
  or a pathname specifying the name
  of the core file
"
  `(progn
     (in-package "MATLISP")
     (ext:save-lisp ,core-file-name)))

#+:cmu
(eval-when (compile eval load)
   (setf (getf ext:*herald-items* :matlisp)
      (list (matlisp-herald))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  The following code does:
;;;
;;;  1. Defines the logical pathname "matlisp"
;;;  2. Uses defsystem to define the MATLISP system.
;;;
;;;  Once this file is loaded you may load/compile MATLISP
;;;  with the command:
;;;
;;;                 (matlisp:load-matlisp)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (fboundp 'deflogicalpath)
    (progn
      (deflogicalpath "matlisp" "")
      (require "MAKE" (namestring 
		       (translate-logical-pathname "matlisp:defsystem.lisp"))))
  (flet ((default-dir ()
	   #+:cmu (ext:default-directory)
	   #+:allegro (current-directory)))
     (flet ((load-pathname ()
	       (merge-pathnames 
		(if *load-pathname* 
		    (make-pathname :directory (pathname-directory *load-pathname*))
		  "") (default-dir))))

	 (setf (logical-pathname-translations "matlisp")
	       `(("**;*.*.*"  ,(namestring (merge-pathnames "**/*.*.*" (load-pathname))))
		 ("*.*.*" "*.*.*")))
	 (require "MAKE" (namestring (merge-pathnames "defsystem.lisp" (load-pathname)))))))

(mk::defsystem lazy-loader
      :source-pathname "matlisp:lib"
      :source-extension "lisp"
      :binary-pathname "matlisp:bin;"
      :components
      ("lazy-loader"))

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
	:components ("fortran"))
       (:module "foreign-functions"
	:source-pathname ""
	:source-extension "lisp"
	:binary-pathname ""
	:depends-on ("foreign-interface")
	:components ("blas"
		     "lapack"))
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
		     "msqrt"))))


(defun load-matlisp ()
  (mk::operate-on-system 'matlisp 
			 'load
			 :minimal-load t
			 :verbose nil
			 :compile-during-load t)
  (format t "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
")
  (if (not (eq *package* (find-package "MATLISP")))
      (let ((symbols nil)
	    (count 0))
	(format t "~&;;; MATLISP is importing its external symbols to package ~a~%"
		(package-name *package*))
	(do-symbols (r *package*)
	     (if (eq (symbol-package r) *package*)
		 (push (symbol-name r) symbols)))
	(do-external-symbols (s "MATLISP")
	    (if (member (symbol-name s) symbols :test #'equal)
		    (format t "~&;;; WARNING: overriding symbol ~a in package ~a~%"
			    (symbol-name s)
			    (package-name *package*)))
	    (incf count)
	    (unintern s *package*)
	    (import s *package*))
	(format t "~&;;; ~d symbols imported from MATLISP to package ~a~%"
		count
		(package-name *package*))))
  (format t ";;;
;;; MATLISP loaded, type (HELP matlisp) to see available symbols
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
")
  (values))

(unintern 'matlisp-version "COMMON-LISP-USER")
(unintern 'matlisp-herald "COMMON-LISP-USER")
(unintern 'save-matlisp "COMMON-LISP-USER")
(unintern 'load-matlisp  "COMMON-LISP-USER")

(import '(matlisp-version 
	  matlisp-herald
	  save-matlisp
	  load-matlisp) "COMMON-LISP-USER")

