;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: :fortran-ffi-accessors; Base: 10 -*-
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
;;; $Id: ffi-sbcl.lisp,v 1.1 2003/12/09 04:28:31 rtoy Exp $
;;;
;;; $Log: ffi-sbcl.lisp,v $
;;; Revision 1.1  2003/12/09 04:28:31  rtoy
;;; Add support for SBCL.  I did not test if SBCL works, but CMUCL still
;;; works.
;;;
;;; From Robbie Sedgewick on matlisp-users, 2003-11-13.
;;;
;;; Revision 1.7  2002/07/26 21:38:02  rtoy
;;; Fix a bug in generating the Fortran inteface when a complex number is
;;; returned.  Use an array instead of a complex number for the result and
;;; create a complex from the array elements for the function value.
;;;
;;; Revision 1.6  2001/07/26 15:44:54  rtoy
;;; Moved the Fortran name mangling stuff to its own file.  Some common
;;; things from ffi-acl and ffi-cmu also moved there.
;;;
;;; Revision 1.5  2001/02/26 22:54:23  rtoy
;;; It appears to be ok to inline the def-alien-routine and
;;; vector-data-addresses.  The copy! bug isn't tickled.
;;;
;;; Revision 1.4  2001/02/21 19:40:52  simsek
;;; o Added the :long keyword (equivalent to :long)
;;;
;;; Revision 1.3  2000/10/04 01:11:19  simsek
;;; o Removed inlines (see comments in code)
;;;
;;; Revision 1.2  2000/07/11 18:02:03  simsek
;;; o Added credits
;;;
;;; Revision 1.1  2000/07/11 02:11:56  simsek
;;; o Added support for Allegro CL
;;;
;;; Revision 1.13  2000/06/19 22:21:45  rtoy
;;; Define packages elsewhere.
;;;
;;; Revision 1.12  2000/05/08 15:28:20  rtoy
;;; Removed the variable capture of hidden-complex-return-value by
;;; gensym'ing a new var.
;;;
;;; Revision 1.11  2000/05/05 18:56:51  rtoy
;;; o Try to add comments to routines and stuff.
;;; o Some minor simplification of the code
;;; o Clean up def-fortran-interface.  (Use only one form for the basic
;;;   function.)  Remove one source of macro variable capture.  Still have
;;;   one with hidden-complex-return-variable, though.
;;; o Cleaned up the comments and doc-string for def-fortran-routine.
;;; o incf-sap: don't need to special case n = 1 because CMUCL is smart
;;;   enough to fold the multiplication.
;;; o Add matlisp-specialized-array type.
;;; o Use vector-sap if possible.
;;; o Remove obsolete with-vector-data-addresses
;;;
;;; Revision 1.10  2000/05/02 14:32:13  rtoy
;;; Convert CR/LF to standard Unix LF.
;;;
;;; Revision 1.9  2000/05/02 13:48:34  rtoy
;;; Turn off invalid trap when calling out to Fortran routines.  Needed
;;; to fix a problem with SVD stopping with an invalid exception.
;;;
;;; Revision 1.8  2000/04/14 00:04:55  simsek
;;; o Added INCF-SAP so that the size of a
;;;   double, single, etc .. need not be known in any
;;;   other lisp files.
;;; o In future revisions, these machine dependent sizes should be determined
;;;   by configure (e.g. configure can check whether a Fortran DOUBLE PRECISION
;;;   is a C double and check the size of C double and so on.
;;;
;;; Revision 1.7  2000/28/01 17:44:46  simsek
;;; o Using SYSTEM::WITHOUT-GCING instead of GC-ON and GC-OFF
;;;   in WITH-VECTOR-DATA-ADDRESSES. 
;;;
;;; Revision 1.6  2000/20/01 09:18:24  simsek
;;; o Added DEFPACKAGE.
;;; o Reworked DEF-FORTRAN-INTERFACE.  The CMUCL FFI always returns a value 
;;;   for a function even if it is void (in this case NIL).  Also, hacked
;;;   the STYLE specifier to accept :OUTPUT, :INPUT, :INPUT-OUTPUT, 
;;;   :INPUT-OR-OUTPUT, :WORKSPACE, :WORKSPACE-OUTPUT, :WORKSPACE-OR-OUTPUT
;;;   to establish some form of semantics for interfacing Lisp (a functional
;;;   language) to the BLAS/LAPACK Fortran routines (a pass-by-reference structure).
;;; o Added CAT, SCAT, PARSE-DOC-&-PARAMETERS, CAST-AS-ARRAY-P, GET-READ-IN-TYPE,
;;;   GET-READ-OUT-TYPE, GET-READ-IN-STYLE and GET-READ-OUT-STYLE.
;;; o Renames MAKE-LISP-NAME to MAKE-FORTRAN-FFI-NAME and renamed
;;;   HANDLE-FORTRAN-PARAMETERS to PARSE-FORTRAN-PARAMETERS.
;;;
;;; Revision 1.5  1999/08/05 15:00:49  toy
;;; Add support for simple arrays of any dimension.
;;;
;;; Revision 1.4  1999/08/04 22:12:46  toy
;;; o Fixup a compiler warning in def-fortran-interface.
;;; o Fix the doc string in vector-data-address.
;;; o Change vector-data-address so we always check for the type.  If we
;;;   screw this up, we can really hose the whole lisp, so make sure the
;;;   argument has the right type!
;;;
;;; Revision 1.3  1999/08/04 13:36:19  toy
;;; o  Fix up some comment errors.
;;; o  VECTOR-DATA-ADDRESS returns an SAP instead of an integer now.  Make
;;;    WITH-VECTOR-DATA-ADDRESSES work with this.
;;;
;;; Revision 1.2  1999/08/02 21:47:22  toy
;;; o  Handle the style parameter.
;;; o  We want all return values, in case some are set by the Fortran
;;;    routine.
;;;
;;; Revision 1.1  1999/08/02 17:15:04  toy
;;; Initial revision
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Alien function interface to FORTRAN (BLAS/LAPACK)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "FORTRAN-FFI-ACCESSORS")

(defun parse-doc-&-parameters (body &optional header footer)
  (if (stringp (first body))
      (values `(,(%cat% header (first body) footer)) (rest body))
    (values (if (or header footer)
		(%cat% header "" footer)
	      nil)
	    body)))

;; If TYPE is some kind of array, return non-NIL to indicate that we
;; need to cast this as an array type for the alien function
;; interface.
(defun cast-as-array-p (type)
  (or (listp type)
      (eq type :complex-single-float)
      (eq type :complex-double-float)))

;; Convert the Fortran TYPE to the underlying alien type.
(defun get-read-in-type (type)
  (flet ((convert (type)
	   ;; Fortran wants, essentially, the complex number to look
	   ;; like a 2-element array consisting of the real and
	   ;; imaginary parts.
	   (ecase type
	     (:integer 'sb-c::int)
	     (:long 'sb-c::int)
	     ((:single-float :complex-single-float) 'sb-c::single-float)
	     ((:double-float :complex-double-float) 'sb-c::double-float))))
    
    (if (cast-as-array-p type)
	`(* ,(convert (if (listp type)
			  (second type)
			type)))
      (convert type))))

;; Convert the Fortran return value to the corresponding alien type.
(defun get-read-out-type (type)
  (ecase type
    (:void 'void)
    (:integer 'sb-c::int)
    (:long 'sb-c::int)
    (:single-float 'sb-c::single-float)
    (:double-float 'sb-c::double-float)))


;; Convert the Fortran style parameter into the corresponding alien
;; style parameter.
(defun get-read-in-style (style type)
  (if (or (cast-as-array-p type)
	  (eq type :string))
      :in
      (ecase style
	((nil :input :workspace) :copy)
	((:output :input-output :input-or-output
		  :workspace-output :workspace-or-output) :in-out)
	((:out :in :copy :in-out) style))))

;; Return non-NIL if STYLE is designates some type of output variable.
(defun get-read-out-style (style)
  (member style '(:in-out :out :output :input-output :input-or-output
		  :workspace-output :workspace-or-output)))

;; Parse the parameter list of the Fortran routine and return a new
;; list appropriate for use in defining the alien function.
(defun parse-fortran-parameters (body)

  (multiple-value-bind (doc pars)
      (parse-doc-&-parameters body)
    (declare (ignore doc))

    (let* ((aux-pars nil)
	   (new-pars
	    (mapcar #'(lambda (par)
			(destructuring-bind (name type &optional (style :input))
			    par
			  (case type
			    (:string
			     (pushnew `(,(scat "LEN-" name) sb-c::int :copy) aux-pars)
			     `(,name sb-c::c-string ,(get-read-in-style style type)))
			    (t
			     `(,name ,(get-read-in-type type) ,(get-read-in-style style type))))))
		    pars)))
      `(;; don't want documentation for direct interface, not useful
	;; ,@doc 
	,@new-pars ,@aux-pars))))

;; Create a form specifying a simple Lisp function that calls the
;; underlying Fortran routine of the same name.
(defun def-fortran-interface (name return-type body hidden-var-name)
  (multiple-value-bind (doc pars)
      (parse-doc-&-parameters body)

    ;; Hmm, this passes over pars many, many times.  Should we
    ;; rearrange it so that we pass over pars just once and collect
    ;; the various pieces at the same time?
    (let* (
	   (return-value `(,(gensym "RETURN-VAL-")))
	   ;; Names of all the args
	   (args (remove hidden-var-name (mapcar #'first pars)))
	   ;; A list of pairs suitable for use with
	   ;; with-vector-data-addresses
	   (saps (mapcar #'(lambda (p)
			     `(,(scat "ADDR-" (first p)) ,(first p)))
			 (remove-if-not #'(lambda (p)
					    (cast-as-array-p (second p)))
					pars)))
	   ;; The actual name of the underlying Fortran routine
	   (ffi-fn (make-fortran-ffi-name name))
	   ;; The FFI return variables
	   (ffi-rvs (mapcar #'(lambda (p)
				(scat "NEW-" (first p)))
			    (remove-if-not #'(lambda (p)
					       (and (not (cast-as-array-p (second p)))
						    (not (eq (second p) :string))
						    (get-read-out-style (third p))))
					   pars)))
	   ;; The FFI arguments
	   (ffi-args (mapcar #'(lambda (p)
				 (if (cast-as-array-p (second p))
				     (scat "ADDR-" (first p))
				     (first p)))
			     pars))
	   ;; Extra arguments for string handling (the length
	   ;; of the string), if needed.
	   (aux-ffi-args (mapcar #'(lambda (p)
				     `(length (the string ,(first p))))
				 (remove-if-not #'(lambda (p)
						    (eq (second p) :string))
						pars)))
	   ;; The return variable(s)
	   (rvs (mapcar #'(lambda (p)
			    (if (or (cast-as-array-p (second p))
				    (eq (second p) :string))
				(first p)
				(scat "NEW-" (first p))))
			(remove-if-not #'(lambda (p)
					   (get-read-out-style (third p)))
				       pars)))
	   ;; The definition of the Lisp interface function we want.
	   (defun-body
	       `(
		 ;; Too hard to debug if inlined.
		 ;;(declaim (inline ,name))
		 (with-vector-data-addresses (,@saps)
		   (multiple-value-bind (,@return-value
					 ,@ffi-rvs)
		       (,ffi-fn ,@ffi-args ,@aux-ffi-args)
		   
		     (declare (ignore ,@(and (eq return-type :void) return-value)))
		     (values ,@(and (not (eq return-type :void))  return-value)
			     ,@(mapcar #'(lambda (s)
					   (if (eq s hidden-var-name)
					       hidden-var-name
					       s)) rvs)))))))
				      

      (if (find hidden-var-name (mapcar #'first pars))
	  `(
	    ;; Too hard to debug if inlined.
	    ;;(declaim (inline ,name))
	    ;;
	    ;; This used to create a complex number for
	    ;; hidden-var-name, but seemed to be causing some problems
	    ;; for CMUCL.  Therefore, we now create a 2-element array
	    ;; to hold the result (which is ok by Fortran rules), can
	    ;; create the complex result from the contents of the
	    ;; array.
	    ;;
	    ;; The problem seems to be that CMUCL was creating a
	    ;; "static" storage area for hidden-var-name.  Since CMUCL
	    ;; doesn't seem to know that it was being modified in the
	    ;; body, it returns the address of the static storage. So,
	    ;; for example, (/ (dot a b) (dot c d)) would have the
	    ;; results of the first dot overwritten by the second,
	    ;; making the division always return #c(1.0 0.0).  This is
	    ;; the theory---I'm not 100% sure it's right, but this
	    ;; change fixes the bug.
	    (let ((,hidden-var-name (make-array 2 :element-type 'double-float)))
	      (defun ,name ,args
		,@doc
		,@defun-body
		(complex (aref ,hidden-var-name 0) (aref ,hidden-var-name 1)))))
	  `(
	    ;; Too hard to debug if inlined.
	    ;;(declaim (inline ,name))
	    (defun ,name ,args
	      ,@doc
	      ,@defun-body))))))


;;
;; DEF-FORTRAN-ROUTINE
;;
;; A macro similar to DEF-ALIEN-ROUTINE but specialized to the Fortran
;; BLAS/LAPACK libraries.
;;
;; An external Fortran routine definition form (DEF-FORTRAN-ROUTINE
;; MY-FUN ...) creates two functions:
;;
;;   1. a raw FFI (foreign function interface),
;;   2. an easier to use lisp interface to the raw interface.
;;
;; The documentation  given here relates in the most part to the
;; simplified lisp interface.
;;
;; Example:
;; ========
;; libblas.a contains the fortran subroutine DCOPY(N,X,INCX,Y,INCY)
;; which copies the vector Y of N double-float's to the vector X.
;; The function name in libblas.a is \"dcopy_\" (by Fortran convention).
;;
;; (DEF-FORTRAN-ROUTINE DCOPY :void 
;;   (N :integer :input)
;;   (X (* :double-float) :output)
;;   (INCX :integer :input)
;;   (Y (* :double-float) :input)
;;   (INCY :integer :input))
;;
;; will expand into:
;;
;; (DEF-ALIEN-ROUTINE ("dcopy_" FORTRAN-DCOPY) void
;;    (N :int :copy)
;;    (X (* double-float))
;;    ...
;;
;; and
;; 
;; (DEFUN DCOPY (N,X,INCX,Y,INCY)
;;    ...
;;
;; In turn, the lisp function DCOPY calls FORTRAN-DCOPY which calls
;; the Fortran function "dcopy_" in libblas.a.
;;
;;

