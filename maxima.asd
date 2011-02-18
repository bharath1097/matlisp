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

(in-package #:common-lisp-user)

;;(matlisp-start::deflogicalpath "matlisp")

(asdf:defsystem matlisp-packages
      :pathname #.(translate-logical-pathname "matlisp:srcdir;")
      ;;;;:binary-pathname (translate-logical-pathname "matlisp:bin;")
      ;;:source-extension "lisp"
      :components
      ((:file "packages")))

(asdf:defsystem lazy-loader
      :pathname #.(translate-logical-pathname "matlisp:lib;")
      ;;;;:source-extension "lisp"
      ;;;;:binary-pathname (translate-logical-pathname "matlisp:bin;")
      :depends-on ("matlisp-packages")
      :components
      ((:file "lazy-loader"
	      ;; you need the load-only here,
	      ;; otherwise, Allegro tries to
	      ;; load the DLL (SO)'s twice
	      ;; and fails.
	      )))

(asdf:defsystem fortran-names
      :pathname #.(translate-logical-pathname "matlisp:src;")
      ;;:binary-pathname (translate-logical-pathname "matlisp:bin;")
      ;;:source-extension "lisp"
      :depends-on ("matlisp-packages")
      :components
      ((:file "f77-mangling")))

(asdf:defsystem matlisp-f2cl-macros
  :pathname #.(translate-logical-pathname "matlisp:srcdir;lib-src;")
  ;;:binary-pathname (translate-logical-pathname "matlisp:bin;")
  ;;:source-extension "l"
  :depends-on ("matlisp-packages")
  :components
  ((:file "macros" :type "l")))

(asdf:defsystem matlisp
      :pathname #.(translate-logical-pathname "matlisp:srcdir;")
      ;;:binary-pathname (translate-logical-pathname "matlisp:bin;")
      ;;:source-extension "lisp"
      :depends-on ("lazy-loader"
                   "matlisp-packages"
		   "fortran-names"
		   "matlisp-f2cl-macros")
      :components
      ((:module "foreign-interface"
	:pathname "src"
	;;:source-extension "lisp"
	:components ((:file
		      #+:cmu "ffi-cmu"
		      #+:sbcl "ffi-sbcl"
		      #+:allegro "ffi-acl"
		      )))
       (:module "foreign-functions"
	:pathname "src"
	;;:source-extension "lisp"
	:depends-on ("foreign-interface")
	:components ((:file "blas")
		     (:file "lapack")
		     (:file "dfftpack")
		     #+nil (:file "ranlib")))
       (:module "matlisp-essentials"
	:pathname "src"
	;;:source-extension "lisp"
	:depends-on ("foreign-interface" 
		     "foreign-functions")
	:components ((:file "conditions")
		     (:file "matrix")
		     (:file "ref")
		     (:file "print")
		     (:file "copy")))

       (:module "matlisp-blas-wrappers"
	:pathname "src"
	;;:source-extension "lisp"
	:depends-on ("foreign-interface" 
		     "foreign-functions"
		     "matlisp-essentials")
	:components ((:file "axpy")
		     (:file "scal")
		     (:file "swap")
		     (:file "gemm")))

       (:module "matlisp-lapack-wrappers"
	:pathname "src"
	;;:source-extension "lisp"
	:depends-on ("foreign-interface" 
		     "foreign-functions"
		     "matlisp-essentials")
	:components ((:file "gesv")
		     (:file "geev")
		     (:file "getrf")
		     (:file "getrs")
		     (:file "potrf")
		     (:file "potrs")))

       (:module "matlisp-functions"
        :pathname "src"
	;;:source-extension "lisp"
	:depends-on ("foreign-interface"
		     "foreign-functions"
		     "matlisp-essentials"
		     "matlisp-blas-wrappers"
		     "matlisp-lapack-wrappers")
	:components ((:file "compat")
		     (:file "help")
		     (:file "diag")
		     (:file "special")
		     (:file "reader")
		     (:file "trans")
		     (:file "realimag")
		     (:file "reshape")
		     (:file "join")
		     (:file "svd")
		     (:file "sum")
		     (:file "norm")
		     (:file "dot")
		     (:file "trace")
		     (:file "seq")
		     (:file "vec")
		     (:file "map")
		     (:file "mplus")
		     (:file "mminus")
		     (:file "mtimes")
		     (:file "mdivide")
		     (:file "msqrt")
		     (:file "fft")
		     (:file "geqr")))
       (:module "special-functions"
		:pathname "src"
		:depends-on ("matlisp-functions")
		:components
		((:file "specfun")))
       ;; Various add-on packages for matlisp
       ;; This is just the f2cl macros we need, not all of f2cl.
       #+nil
       (:module "f2cl-macros"
		:pathname "lib-src"
		;;:source-extension "l"
		:components
		((:file "macros")))
       ;; This is Quadpack, converted from the Fortran
       ;; implementation to Lisp via f2cl.
       #+nil
       (:module "quadpack-functions"
		:depends-on ("f2cl-macros")
		:components
		((:module "quadpack-interface"
			  :pathname "src"
			  :components
			  ((:file "quadpack")))
		 (:module "quadpack-lib"
			  :pathname "lib-src/quadpack/"
			  :package "QUADPACK"
			  :components
			  (
			   #+nil
			   (:module mach-par
				    :pathname ""
				    ;;:source-extension "lisp"
				    ;;:binary-pathname "matlisp:bin;"
				    :components
				    ((:file "d1mach")
				     (:file "i1mach")))
			   (:module src
				    :pathname "lib-src/quadpack/"
				    ;; :depends-on ("mach-par")
				    :components
				    (
				     ;; Support
				     (:file "dqwgtf")
				     (:file "dqcheb")
				     (:file "dqk15w")
				     (:file "dqwgts")
				     (:file "dqwgtc")
				     (:file "dgtsl")
				     (:file "xerror")
	       
				     ;; Core integration routines
				     (:file "dqk15")
				     (:file "dqk31")
				     (:file "dqk41")
				     (:file "dqk51")
				     (:file "dqk61")
				     (:file "dqk21")
				     (:file "dqk15i")
				     (:file "dqelg")
				     (:file "dqpsrt")
				     (:file "dqc25s"
					    :depends-on ("dqcheb" "dqk15w"))
				     (:file "dqmomo")
				     (:file "dqc25c"
					    :depends-on ("dqcheb"
							 "dqk15w"))
				     (:file "dqc25f"
					    :depends-on ("dgtsl"
							 "dqcheb"
							 "dqk15w"
							 "dqwgtf"))
				     ;; Basic integrators
				     (:file "dqage"
					    :depends-on ("dqk15"
							 "dqk31"
							 "dqk41"
							 "dqk51"
							 "dqk61"
							 "dqk21"
							 "dqpsrt"))
				     (:file "dqagie"
					    :depends-on ("dqelg"
							 "dqk15i"
							 "dqpsrt"))
				     (:file "dqagpe"
					    :depends-on ("dqelg"
							 "dqpsrt"
							 "dqk21"
							 ))
				     (:file "dqagse"
					    :depends-on ("dqk21"
							 "dqelg"
							 "dqpsrt"))
				     (:file "dqawfe"
					    :depends-on ("dqagie"
							 "dqawoe"
							 "dqelg"))
				     (:file "dqawoe"
					    :depends-on ("dqc25f"
							 "dqpsrt"
							 "dqelg"))
				     (:file "dqawse"
					    :depends-on ("dqc25s"
							 "dqmomo"
							 "dqpsrt"))
				     (:file "dqawce"
					    :depends-on ("dqc25c"
							 "dqpsrt"))
				     ;; Simplified interface routines
				     (:file "dqng"
					    :depends-on ("xerror"))
				     (:file "dqag"
					    :depends-on ("dqage"
							 "xerror"))
				     (:file "dqags"
					    :depends-on ("dqagse"
							 "xerror"))
				     (:file "dqagi"
					    :depends-on ("dqagie"
							 "xerror"))
				     (:file "dqawf"
					    :depends-on ("dqawfe"
							 "xerror"))
				     (:file "dqawo"
					    :depends-on ("dqawoe"
							 "xerror"))
				     (:file "dqaws"
					    :depends-on ("dqawse"
							 "xerror"))
				     (:file "dqawc"
					    :depends-on ("dqawce"
							 "xerror"))))))))
       #+nil
       (:module "minpack-functions"
		:depends-on ("f2cl-macros")
		:components
		((:module "minpack-lib"
			  :pathname "lib-src/minpack/"
			  :package "MINPACK"
			  :components
			  ((:file "dpmpar")
			   (:file "enorm")
			   (:file "fdjac2")
			   (:file "qrsolv")
			   (:file "lmpar")
			   (:file "qrfac")
			   (:file "lmdif")
			   (:file "lmdif1")
			   (:file "lmder")
			   (:file "lmder1")
			   (:file "dogleg")
			   (:file "qform")
			   (:file "r1mpyq")
			   (:file "r1updt")
			   (:file "hybrj" :depends-on ("dogleg" "qform" "r1mpyq" "r1updt"))
			   (:file "hybrj1" :depends-on ("hybrj"))
			   ))))
       #+nil
       (:module "lib-src"
		:components
		(#+nil
		 (:file "d1mach"
			:package "MATLISP-LIB")
		 (:module "cpoly"
			  :pathname "lib-src/cpoly"
			  ;;:source-extension "lisp"
			  :components
			  ((:file "cpoly")
			   (:file "zeroin"
				  :package "MATLISP-LIB")))
		 #+(or :cmu :sbcl)
		 (:module "gnuplot"
			  :pathname "lib-src/gnuplot"
			  ;;:source-extension "lisp"
			  :components
			  ((:file "gnuplot")))))))


;; Add-on packages
(asdf:defsystem matlisp-quadpack
  :pathname #.(translate-logical-pathname "matlisp:srcdir;")
  ;;:binary-pathname (translate-logical-pathname "matlisp:bin;")
  ;;:source-extension "lisp"
  :depends-on ("matlisp-f2cl-macros")
  :components
  ((:module "quadpack-interface"
	    :pathname "src"
	    :components
	    ((:file "quadpack")))
   (:module "lib-src"
	    ;;:source-pathname ""
	    :components
	    ((:module "quadpack"
		      ;;:source-pathname "lib-src/quadpack/"
		      ;;:package "QUADPACK"
		      :components
		      (
		       ;; Support
		       (:file "dqwgtf")
		       (:file "dqcheb")
		       (:file "dqk15w")
		       (:file "dqwgts")
		       (:file "dqwgtc")
		       (:file "dgtsl")
		       (:file "xerror")
	       
		       ;; Core integration routines
		       (:file "dqk15")
		       (:file "dqk31")
		       (:file "dqk41")
		       (:file "dqk51")
		       (:file "dqk61")
		       (:file "dqk21")
		       (:file "dqk15i")
		       (:file "dqelg")
		       (:file "dqpsrt")
		       (:file "dqc25s"
			      :depends-on ("dqcheb" "dqk15w"))
		       (:file "dqmomo")
		       (:file "dqc25c"
			      :depends-on ("dqcheb"
					   "dqk15w"))
		       (:file "dqc25f"
			      :depends-on ("dgtsl"
					   "dqcheb"
					   "dqk15w"
					   "dqwgtf"))
		       ;; Basic integrators
		       (:file "dqage"
			      :depends-on ("dqk15"
					   "dqk31"
					   "dqk41"
					   "dqk51"
					   "dqk61"
					   "dqk21"
					   "dqpsrt"))
		       (:file "dqagie"
			      :depends-on ("dqelg"
					   "dqk15i"
					   "dqpsrt"))
		       (:file "dqagpe"
			      :depends-on ("dqelg"
					   "dqpsrt"
					   "dqk21"
					   ))
		       (:file "dqagse"
			      :depends-on ("dqk21"
					   "dqelg"
					   "dqpsrt"))
		       (:file "dqawfe"
			      :depends-on ("dqagie"
					   "dqawoe"
					   "dqelg"))
		       (:file "dqawoe"
			      :depends-on ("dqc25f"
					   "dqpsrt"
					   "dqelg"))
		       (:file "dqawse"
			      :depends-on ("dqc25s"
					   "dqmomo"
					   "dqpsrt"))
		       (:file "dqawce"
			      :depends-on ("dqc25c"
					   "dqpsrt"))
		       ;; Simplified interface routines
		       (:file "dqng"
			      :depends-on ("xerror"))
		       (:file "dqag"
			      :depends-on ("dqage"
					   "xerror"))
		       (:file "dqags"
			      :depends-on ("dqagse"
					   "xerror"))
		       (:file "dqagi"
			      :depends-on ("dqagie"
					   "xerror"))
		       (:file "dqawf"
			      :depends-on ("dqawfe"
					   "xerror"))
		       (:file "dqawo"
			      :depends-on ("dqawoe"
					   "xerror"))
		       (:file "dqaws"
			      :depends-on ("dqawse"
					   "xerror"))
		       (:file "dqawc"
			      :depends-on ("dqawce"
					   "xerror"))))))))

(asdf:defsystem matlisp-minpack
  :pathname #.(translate-logical-pathname "matlisp:srcdir;")
  ;;:binary-pathname (translate-logical-pathname "matlisp:bin;")
  ;;:source-extension "lisp"
  :depends-on ("matlisp-f2cl-macros")
  :components
  ((:module "lib-src"
	    :components
	    ((:module "minpack"
		      ;;:package "MINPACK"
		      :components
		      ((:file "dpmpar")
		       (:file "enorm")
		       (:file "fdjac2")
		       (:file "qrsolv")
		       (:file "lmpar")
		       (:file "qrfac")
		       (:file "lmdif")
		       (:file "lmdif1")
		       (:file "lmder")
		       (:file "lmder1")
		       (:file "dogleg")
		       (:file "qform")
		       (:file "r1mpyq")
		       (:file "r1updt")
		       (:file "hybrj" :depends-on ("dogleg" "qform" "r1mpyq" "r1updt"))
		       (:file "hybrj1" :depends-on ("hybrj"))
		       ))))))
