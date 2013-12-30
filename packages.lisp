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
;;; Define the packages and symbols for Matlisp.

(defpackage "MATLISP-CONDITIONS"
  (:use #:common-lisp)
  (:export
   ;;<conditon {accessors*}>
   ;;Generic errors
   #:generic-error
   #:dimension-mismatch
   #:assumption-violated
   #:invalid-type
   #:invalid-arguments
   #:invalid-value
   #:unknown-token
   #:parser-error
   #:coercion-error
   #:out-of-bounds-error
   #:non-uniform-bounds-error
   ;;Permutation conditions
   #:permutation
   #:permutation-invalid-error
   #:permutation-permute-error
   ;;Tensor conditions
   #:tensor-error
   #:tensor-store-index-out-of-bounds
   #:tensor-insufficient-store
   #:tensor-not-matrix
   #:tensor-not-vector
   #:tensor-index-out-of-bounds
   #:tensor-index-rank-mismatch
   #:tensor-invalid-head-value
   #:tensor-invalid-dimension-value
   #:tensor-invalid-stride-value
   #:tensor-cannot-find-counter-class
   #:tensor-cannot-find-optimization
   #:tensor-dimension-mismatch
   #:tensor-type-mismatch
   #:tensor-store-not-consecutive
   #:tensor-method-does-not-exist
   #:tensor-abstract-class
))

(defpackage "MATLISP-UTILITIES"
  (:use #:common-lisp #:matlisp-conditions)
  (:export #:ensure-list #:id #:ieql
	   #:vectorify #:copy-n
	   #:ensure-args #:repsym #:findsym #:find-tag
	   #:zip #:zip-eq #:zipsym
	   #:list-eq #:setadd #:setrem #:set-eq
	   #:cut-cons-chain!
	   #:slot-values #:remmeth
	   #:recursive-append #:unquote-args #:flatten
	   #:format-to-string #:string+
	   #:linear-array-type
	   #:list-dimensions
	   #:lvec-foldl #:lvec-foldr #:lvec-max #:lvec-min #:lvec-eq
	   #:lvec-map-foldl! #:lvec-map-foldr!
	   #:lvec->list #:lvec->list!
	   #:compile-and-eval
	   #:getcons #:mapcons
	   ;;Macros
	   #:when-let #:if-let #:if-ret #:with-gensyms #:let-rec #:using-gensyms #:with-marking
	   #:mlet* #:make-array-allocator #:let-typed #:let*-typed
	   #:nconsc #:define-constant
	   #:macrofy #:looped-mapcar #:defun-compiler-macro
	   ;;
	   #:inlining #:definline
	   #:with-optimization #:quickly #:very-quickly #:slowly #:quickly-if))

(defpackage "MATLISP-TEMPLATE"
  (:use #:common-lisp #:matlisp-utilities)
  (:export #:deft/generic #:deft/method #:remt/method))

;;Modified version of Mark Kantrowitz' infix package.
(defpackage "MATLISP-INFIX"
  (:use #:common-lisp #:matlisp-conditions #:matlisp-utilities)
  (:export #:test-infix #:string->prefix))

(defpackage "MATLISP-FFI"
  (:use #:common-lisp #:cffi #:matlisp-utilities #:matlisp-conditions)
  ;; TODO: Check if this is implementation-agnostic.
  ;; #+:cmu (:use :common-lisp :c-call :cffi :utilities)
  ;; #+:sbcl (:use :common-lisp :cffi :utilities)
  ;; Works with ccl.
  ;; #+:allegro (:use :common-lisp :cffi :utilities)
  ;; #+(not (or sbcl cmu allegro)) (:use :common-lisp :cffi :utilities)
  (:export
   ;;Foreign-pointer enclosing structure.
   #:foreign-vector #:make-foreign-vector #:foreign-vector-p
   #:fv-ref #:fv-pointer #:fv-size #:fv-type
   ;;Interface functions
   #:def-fortran-routine
   #:with-vector-data-addresses
   )
  (:documentation "Fortran foreign function interface"))

(defpackage "MATLISP-BLAS"
  (:use #:common-lisp #:matlisp-ffi)
  (:export
   ;;BLAS Level 1
   ;;------------
   ;;Real-double
   #:ddot #:dnrm2 #:dasum #:dscal #:daxpy #:drot
   #:dswap #:dcopy #:idamax
   ;;Complex-double
   #:zdotc #:zdotu #:zdscal #:zscal #:zswap #:zcopy #:zaxpy
   #:dcabs1 #:dzasum #:dznrm2 #:izamax
   ;;BLAS Level 2
   ;;------------
   ;;Real-double
   #:dgemv #:dsymv  #:dtrmv #:dtrsv #:dger #:dsyr #:dsyr2
   ;;Complex-double
   #:zgemv #:zhemv #:ztrmv #:ztrsv #:zgerc #:zgeru #:zher2
   ;;BLAS Level 3
   ;;------------
   ;;Real-double
   #:dgemm #:dsyrk #:dsyr2k #:dtrmm #:dtrsm
   ;;Complex-double  
   #:zgemm #:ztrmm #:ztrsm #:zherk #:zher2k)
  (:documentation "BLAS routines"))

(defpackage "MATLISP-LAPACK"
  (:use #:common-lisp #:matlisp-ffi)
  (:export
   #:dgesv #:dgeev #:dgetrf #:dgetrs #:dgesvd
   #:zgesv #:zgeev #:zgetrf #:zgetrs #:zgesvd
   #:dgeqrf #:zgeqrf #:dgeqp3 #:zgeqp3
   #:dorgqr #:zungqr
   #:dpotrs #:zpotrs #:dpotrf #:zpotrf
   #:dgelsy #:zgelsy)
  (:documentation "LAPACK routines"))

(defpackage "MATLISP-DFFTPACK"
  (:use #:common-lisp #:matlisp-ffi)
  (:export #:zffti #:zfftf #:zfftb #:zffti #:zfftf #:zfftb)
  (:documentation "FFT routines"))

(defpackage "MATLISP-LIBMATLISP"
  (:use #:common-lisp #:matlisp-ffi)
  (:export
   #:descal #:dediv
   #:zescal #:zediv)
  (:documentation "BLAS routines"))

(defpackage "MATLISP"
  (:use #:common-lisp
	#:matlisp-conditions #:matlisp-utilities #:matlisp-ffi #:matlisp-template
	#:matlisp-blas #:matlisp-lapack #:matlisp-dfftpack #:matlisp-libmatlisp)
  (:export #:index-type #:index-array #:allocate-index-store #:make-index-store
	   ;;Standard-tensor
	   #:standard-tensor
	   #:rank #:dimensions #:number-of-elements
	   #:head #:strides #:store-size #:store
	   #:parent-tensor
	   ;;Sub-tensor
	   #:subtensor~ #:subtensor
	   ;;Store indexers
	   #:store-indexing
	   #:store-indexing-vec #:store-indexing-lst
	   ;;Store accessors
	   #:ref #:store-ref
	   ;;Type checking
	   #:tensor-typep #:tensor-vectorp #:tensor-matrixp #:tensor-squarep)
  (:documentation "MATLISP routines"))

;;Transitioning to using the tensor-datastructures; eventually move things back to :matlisp

;; Stolen from f2cl.  
;; (defpackage :f2cl-lib
;;   (:use :cl)
;;   (:documentation "The package holding all symbols used by the fortran to lisp library.")
;;   (:nicknames :fortran-to-lisp-library)
;;   (:export
;;    ;; constants
;;    #:%false% #:%true%
;;    ;; user-settable runtime options
;;    #:*check-array-bounds*
;;    ;; types
;;    #:integer4 #:integer2 #:integer1 #:real8 #:real4 #:complex8 #:complex16
;;    #:array-double-float #:array-single-float #:array-integer4 #:array-strings
;;    #:logical
;;    ;; macros
;;    #:fref #:fset #:with-array-data #:with-multi-array-data
;;    #:f2cl-init-string #:fref-string #:fset-string #:f2cl-set-string
;;    #:f2cl-// #:fstring-/= #:fstring-= #:fstring-> #:fstring->= #:fstring-< #:fstring-<=
;;    #:fortran_comment #:fdo #:f2cl/ #:arithmetic-if #:computed-goto
;;    #:assigned-goto
;;    #:fformat
;;    #:data-implied-do
;;    #:int-add #:int-sub #:int-mul
;;    ;; utilities
;;    #:array-slice #:array-initialize
;;    ;; intrinsic functions
;;    #:abs #:acos #:aimag #:aint #:alog #:alog10 #:amax0 #:amax1
;;    #:amin1 #:amod #:anint #:asin #:atan #:atan2
;;    #:cabs #:cexp #:fchar #:clog #:cmplx #:conjg #:ccos
;;    #:csin #:csqrt #:dabs #:dacos #:dasin
;;    #:datan #:datan2 #:dble #:dcos #:dcosh #:dexp #:dim
;;    #:dint #:dlog #:dlog10 #:dmax1 #:dmin1 #:dmod
;;    #:dnint #:dprod #:dsign #:dsin #:dsinh #:dsqrt #:dtan
;;    #:dtanh #:ffloat #:iabs #:ichar #:idim #:idint
;;    #:idnint #:ifix #:index #:int #:isign #:le #:len
;;    #:lge #:lgt #:flog #:log10 #:lt #:max #:max0
;;    #:max1 #:min0 #:min1 #:nint #:freal
;;    #:sign #:sngl #:fsqrt
;;    ;; other functions
;;    #:d1mach #:r1mach #:i1mach
;;    ))

;; (defpackage :fortran-to-lisp
;;     (:use :cl)
;;   (:documentation "the package holding all symbols need by the fortran to lisp converter")
;;   (:nicknames :f2cl)
;;   (:export
;;    ;; main routines
;;    #:f2cl
;;    #:f2cl-compile
;;    ))

;; (defpackage "QUADPACK"
;;   (:use "COMMON-LISP" "FORTRAN-TO-LISP")
;;   (:export
;;    ;; Do we want to export the core integration routines too?

;;    ;; The basic integrators
;;    "DQAGE" "DQAGIE" "DQAGPE" "DQAGSE" "DQAWFE" "DQAWOE" "DQAWSE" "DQAWCE"
;;    ;; Simplified interface routines
;;    "DQNG" "DQAG" "DQAGS" "DQAGI" "DQAWS" "DQAWC")
;;   (:documentation "QUADPACK routines for numerical integration"))

;; (defpackage "MINPACK"
;;   (:use "COMMON-LISP" "FORTRAN-TO-LISP")
;;   (:export
;;    "LMDIF1")
;;   (:documentation "MINPACK routines for minimization"))

;; (defpackage "MATLISP-LIB"
;;   (:use "COMMON-LISP" "F2CL")
;;   (:export
;;    "ZEROIN")
;;   (:documentation "Other useful routines"))


;; (defpackage :matlisp
;;   (:use :common-lisp :fortran-ffi-accessors :blas :lapack :dfftpack :quadpack :matlisp-lib :utilities)
;;   (:shadow #:real)
;;   (:export #:*print-matrix*
;; 	   ;;
;; 	   #:integer4-type #:integer4-array #:allocate-integer4-store
;; 	   #:index-type #:index-array #:allocate-index-store #:make-index-store
;; 	   ;;Standard-tensor
;; 	   #:standard-tensor
;; 	   #:rank #:dimensions #:number-of-elements
;; 	   #:head #:strides #:store-size #:store
;; 	   ;;Sub-tensor
;; 	   #:sub-tensor
;; 	   #:parent-tensor
;; 	   ;;Store indexers
;; 	   #:store-indexing
;; 	   #:store-indexing-internal #:store-indexing-vec #:store-indexing-lst
;; 	   ;;Store accessors
;; 	   #:tensor-store-ref
;; 	   #:tensor-ref
;; 	   ;;Type checking
;; 	   #:tensor-type-p #:vector-p #:matrix-p #:square-p
	   
;; 	   ;;Level 1 BLAS
;; 	   #:axpy! #:axpy
;; 	   #:copy! #:copy
;; 	   #:scal! #:scal
;; 	   ;;Level 2 BLAS
;; 	   #:gemv! #:gemv
;; 	   ;;Level 3 BLAS
;; 	   #:gemm! #:gemm	   
;; 	   ;;Fortran stuff
;; 	   #:blas-copyable-p #:blas-matrix-compatible-p
;; 	   #:fortran-op #:fortran-nop #:fortran-snop
;; 	   ;;Standard-matrix
;; 	   #:standard-matrix
;; 	   #:nrows #:ncols #:number-of-elements
;; 	   #:head #:row-stride #:col-stride
;; 	   #:store #:store-size	  
;; 	   ;;Generic functions on standard-matrix
;; 	   #:fill-matrix
;; 	   #:row-or-col-vector-p #:row-vector-p #:col-vector-p
;; 	   ;;Submatrix ops
;; 	   #:row~ #:row
;; 	   #:col~ #:col
;; 	   #:diag~ #:diag
;; 	   #:sub-matrix~ #:sub-matrix
;; 	   ;;Transpose
;; 	   #:transpose~ #:transpose! #:transpose
;; 	   #:ctranspose! #:ctranspose	   
;; 	   ;;Real-double-matrix
;; 	   #:real-matrix #:real-matrix-element-type #:real-matrix-store-type
;; 	   ;;Complex-double-matrix
;; 	   #:complex-matrix #:complex-matrix-element-type #:complex-matrix-store-type #:complex-coerce #:complex-double-float
;; 	   ;;Real and imaginary parts
;; 	   #:mrealpart~ #:mrealpart #:real
;; 	   #:mimagpart~ #:mimagpart #:imag
;; 	   ;;
;; 	   "CONVERT-TO-LISP-ARRAY"
;;    "DOT"
;;    "EIG"
;;    "EYE"
;;    "FFT"
;;    "FFT"
;;    "GEEV"
;;    "GELSY!"
;;    "GELSY"
;;    "GESV!"
;;    "GESV"
;;    "GETRF!"
;;    "GETRS"
;;    "GETRS!"
;;    "HELP"
;;    "IFFT"
;;    "JOIN"
;;    "LOAD-BLAS-&-LAPACK-BINARIES"
;;    "LOAD-BLAS-&-LAPACK-LIBRARIES"
;;    "LOAD-MATLISP"
;;    "LU"
;;    "M*!"
;;    "M*"
;;    "M+!"
;;    "M+"
;;    "M-"
;;    "M.*!"
;;    "M.*"
;;    "M.+!"
;;    "M.+"
;;    "M.-"
;;    "M./!"
;;    "M./"
;;    "M/!"
;;    "M/"
;;    "MACOS"
;;    "MACOSH"
;;    "MAKE-COMPLEX-MATRIX"
;;    "MAKE-COMPLEX-MATRIX-DIM"
;;    "MAKE-FLOAT-MATRIX"
;;    "MAKE-FLOAT-MATRIX-ARRAY"
;;    "MAKE-FLOAT-MATRIX-DIM"
;;    "MAKE-FLOAT-MATRIX-SEQ"
;;    "MAKE-FLOAT-MATRIX-SEQ-OF-SEQ"
;;    "MAKE-FLOAT-MATRIX-SEQUENCE"
;;    "MAKE-REAL-MATRIX"
;;    "MAKE-REAL-MATRIX-DIM"
;;    "MAP-MATRIX!"
;;    "MAP-MATRIX"
;;    "MASIN"
;;    "MASINH"
;;    "MATAN"
;;    "MATANH"
;;    "MATRIX-REF"
;;    "MCOS"
;;    "MCOSH"
;;    "MEXP"
;;    "MLOG"
;;    "MLOG10"
;;    "MREF"
;;    "MSIN"
;;    "MSINH"
;;    "MSQRT"
;;    "MTAN"
;;    "MTANH"
;;    "NCOLS"
;;    "NORM"
;;    "ONES"
;;    "PRINT-ELEMENT"
;;    "QR"
;;    "QR!"
;;    "GEQR!"
;;    "POTRF!"
;;    "POTRS!"
;;    "RAND"
;;    "RESHAPE!"
;;    "RESHAPE"
;;    "SAVE-MATLISP"
;;    "SEQ"
;;    "SET-M*!-SWAP-SIZE"
;;    "SIZE"
;;    "SQUARE-MATRIX-P"
;;    "STORE-INDEXING"
;;    "SUM"
;;    "SVD"
;;    "SWAP!"
;;    "TR"
;;    "UNLOAD-BLAS-&-LAPACK-LIBRARIES"
;;    "ZEROS"
;;    ;; From Quadpack
;;    "INTEGRATE-QNG"
;;    "INTEGRATE-QAG"
;;    "INTEGRATE-QAGS"
;;    "INTEGRATE-QAGI"
;;    "INTEGRATE-QAWS"
;;    "INTEGRATE-QAWC"
;;    ;; From CPOLY
;;    "POLYROOTS"
;;    ;; From TOMS-715
;;    "M-NORMAL-CDF"
;;    "M-BESSEL-SCALED-I0" "M-BESSEL-SCALED-I1"
;;    "M-BESSEL-SCALED-K0" "M-BESSEL-SCALED-K1"
;;    "M-BESSEL-I0" "M-BESSEL-I1"
;;    "M-BESSEL-J0" "M-BESSEL-J1"
;;    "M-BESSEL-K0" "M-BESSEL-K1"
;;    "M-BESSEL-Y0" "M-BESSEL-Y1"
;;    "M-DAWSON-INTEGRAL"
;;    "M-ERF" "M-ERFC" "M-ERFCX"
;;    "M-GAMMA" "M-LOG-GAMMA"
;;    "M-BESSEL-SERIES-I"
;;    "M-BESSEL-SERIES-J"
;;    "M-BESSEL-SERIES-K"
;;    "M-BESSEL-SERIES-Y")
;;   (:documentation "MATLISP routines"))



;; (defpackage "MATLISP-USER"
;;   (:use "COMMON-LISP"
;;         "MATLISP"
;;         #+:allegro "EXCL"
;;         #+:cmu "EXT"
;;         #+:sbcl "SB-EXT")
;;   (:shadowing-import-from "MATLISP" "REAL")
;;   (:documentation "Matlisp user package meant for interacting with matlisp"))

;; (in-package "MATLISP")

;; ;; We've shadowed CL's REAL.  Re-establish the real type.
;; (deftype real (&optional low high)
;;   `(cl::real ,low ,high))
