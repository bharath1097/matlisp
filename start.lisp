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
;;; Originally written by Tunc Simsek, University of California, Berkeley,
;;; 2000, simsek@eecs.berkeley.edu
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  To compile and load MATLISP:
;;;
;;;      from the shell prompt (this needs to be done only once):
;;;
;;;                 $ make
;;;
;;;      and from within lisp:
;;;
;;;               (load "start.lisp")
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id: start.lisp,v 1.4 2000/07/11 18:02:34 simsek Exp $
;;;
;;; $Log: start.lisp,v $
;;; Revision 1.4  2000/07/11 18:02:34  simsek
;;; o Added credits
;;;
;;; Revision 1.3  2000/07/11 06:19:28  simsek
;;; o Fixed up some badly placed paranthesis
;;; in DEFLOGICALPATH
;;;
;;; Revision 1.2  2000/07/11 02:05:48  simsek
;;; o Added support for Allegro CL
;;; o Fixed up DEFLOGICALPATH
;;;
;;; Revision 1.1  2000/04/13 20:47:28  simsek
;;; o Initial revision.
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package "COMMON-LISP-USER")


(pushnew :matlisp *features*)

(unintern 'deflogicalpath)
(defun deflogicalpath (name)
  "
  Syntax
  ======
  (DEFLOGICALPATH name)

  Purpose
  =======
  Defines a reasonable logical pathname translation for NAME, which
  must be a string.
  The translations are defined for the directory and subdirectories
  in which the file that contained the form was loaded.
"
  (flet ((default-dir ()
	     #+:cmu (ext:default-directory)
	     #+:allegro (user::current-directory)))
    (flet ((load-pathname ()
	     (merge-pathnames 
	      (if *load-pathname* 
		  (if (subtypep (type-of *load-pathname*) 'logical-pathname)
		      (let ((pathname 
			     (namestring
			      (translate-logical-pathname *load-pathname*))))
			(make-pathname 
			 ;; Perhaps we don't need the conditional here,
			 ;; that is, the :device arg is needed on Allegro/Win
			 ;; but it may do no harm on Unix/Linux etc ...
			 #+(and :allegro :mswindows) :device
			 #+(and :allegro :mswindows) (pathname-device pathname)
			 :directory (pathname-directory pathname)))
		    (make-pathname
		     #+(and :allegro :mswindows) :device
		     #+(and :allegro :mswindows) (pathname-device *load-pathname*)		       
		     :directory (pathname-directory *load-pathname*)))
	      "") (default-dir))))

      #+:cmu
      (setf (logical-pathname-translations name)
	    (list (list "**;*.*.*"  
			(namestring (merge-pathnames "**/*.*.*" (load-pathname))))
		  (list "*.*.*" "*.*.*")))
      
      #+:allegro
      (setf (logical-pathname-translations name)
	    (list (list "**;*.*"  
			(namestring (merge-pathnames "**/*.*" (load-pathname))))
		  (list "*.*" "*.*")))

      )))


(deflogicalpath "matlisp")

(load "matlisp:config")
(load "matlisp:system.dcl")

(mk::operate-on-system 'matlisp
		       'load
		       :minimal-load t
		       :verbose nil
		       :compile-during-load 
		       #+:allegro-cl-lite nil
		       #-:allegro-cl-lite t)

(format t "

 ** MATLISP is loaded.  Type (HELP MATLISP)
    to see a list of available symbols.
    To use matlisp:

          (use-package \"MATLISP\")

    or

          (in-package \"MATLISP\")

 ** The logical pathname matlisp has been
    set to:

          ~a

"
	(namestring
	 (translate-logical-pathname "matlisp:")))
