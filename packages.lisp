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
;;; $Id: packages.lisp,v 1.14 2002/01/20 00:41:52 simsek Exp $
;;;
;;; $Log: packages.lisp,v $
;;; Revision 1.14  2002/01/20 00:41:52  simsek
;;; o exporting some forgotton symbols from LAPACK
;;;
;;; Revision 1.13  2002/01/08 00:32:52  rtoy
;;; Add defpackage for the new MINPACK package.
;;;
;;; Revision 1.12  2001/10/25 21:52:57  rtoy
;;; Export QR, QR!, and GEQR!.
;;;
;;; Revision 1.11  2001/07/26 15:47:15  rtoy
;;; Updated version number to "Pre 2.0" since this isn't 1.0b anymore!
;;;
;;; Revision 1.10  2001/05/01 13:11:06  rtoy
;;; o Export I1MACH, R1MACH, D1MACH from the F2CL package.
;;; o Export POLYROOTS.
;;;
;;; Revision 1.9  2001/04/29 15:52:19  rtoy
;;; Add the external symbols from TOMS 715.
;;;
;;; Revision 1.8  2001/04/26 21:49:15  rtoy
;;; Add MATLISP-LIB package.
;;;
;;; Revision 1.7  2001/02/23 18:00:11  rtoy
;;; Add defpackages for FORTRAN-TO-LISP and QUADPACK for quadpack
;;; routines.  Update MATLISP package accordingly.
;;;
;;; Revision 1.6  2000/10/04 23:54:47  simsek
;;; o Importing EXCL (EXT) for CMUCL (Allegro) in Matlisp-user package
;;;
;;; Revision 1.5  2000/10/04 22:49:52  simsek
;;; o Added matlisp-user package
;;;
;;; Revision 1.4  2000/10/04 15:40:46  simsek
;;; o Added unload-blas-&-lapack-binaries
;;;   to symbols exported from matlisp
;;;
;;; Revision 1.3  2000/10/04 01:20:44  simsek
;;; o Moved version related code from system.dcl
;;;   to here.  This code should be the first bit of code loaded
;;;   but only after the system is defined (furthermore, in this
;;;   way we avoid interning symbols in packages other than the
;;;   matlisp package
;;;
;;; Revision 1.2  2000/07/11 02:03:51  simsek
;;; o Added support for Allegro CL
;;;
;;; Revision 1.1  2000/06/19 22:19:33  rtoy
;;; Initial revision.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Define the packages and symbols for Matlisp.

#+:cmu
(defpackage "FORTRAN-FFI-ACCESSORS"
  (:use "COMMON-LISP" "ALIEN" "C-CALL")
  (:export
   ;; Interface functions
   "DEF-FORTRAN-ROUTINE"
   "VECTOR-DATA-ADDRESS"
   "INCF-SAP"
   "WITH-VECTOR-DATA-ADDRESSES"))

#+:allegro
(defpackage "FORTRAN-FFI-ACCESSORS"
  (:use "COMMON-LISP" "FOREIGN-FUNCTIONS")
  (:export 
   "DEF-FORTRAN-ROUTINE"))

(defpackage "BLAS"
#+:cmu  (:use "COMMON-LISP" "ALIEN" "C-CALL" "FORTRAN-FFI-ACCESSORS")
#+:allegro  (:use "COMMON-LISP" "FOREIGN-FUNCTIONS" "FORTRAN-FFI-ACCESSORS")
  (:export
   "IDAMAX" "DASUM" "DDOT" "DNRM2"
   "DROT" "DSCAL" "DSWAP" "DCOPY" "DAXPY"
   "DCABS1" "DZASUM" "DZNRM2" "IZAMAX"
   "ZDSCAL" "ZSCAL" "ZSWAP" "ZCOPY" "ZAXPY" "ZDOTC" "ZDOTU"
   "DGEMV" "DSYMV" "DTRMV" "DTRSV" "DGER" "DSYR" "DSYR2"
   "ZGEMV" "ZHEMV" "ZTRMV" "ZTRSV" "ZGERC" "ZGERU" "ZHER2"
   "DGEMM" "DSYRK" "DSYR2K" "DTRMM" "DTRSM"
   "ZGEMM" "ZTRMM" "ZTRSM" "ZHERK" "ZHER2K" ))

(defpackage "LAPACK"
#+:cmu  (:use "COMMON-LISP" "ALIEN" "C-CALL" "FORTRAN-FFI-ACCESSORS")
#+:allegro  (:use "COMMON-LISP" "FOREIGN-FUNCTIONS" "FORTRAN-FFI-ACCESSORS")
  (:export
   "DGESV" "DGEEV" "DGETRF" "DGESVD"
   "ZGESV" "ZGEEV" "ZGETRF" "ZGESVD" 
   "DGEQRF" "ZGEQRF" "DGEQP3" "ZGEQP3"
   "DORGQR" "ZUNGQR"))

(defpackage "DFFTPACK"
#+:cmu  (:use "COMMON-LISP" "ALIEN" "C-CALL" "FORTRAN-FFI-ACCESSORS")
#+:allegro  (:use "COMMON-LISP" "FOREIGN-FUNCTIONS" "FORTRAN-FFI-ACCESSORS")
  (:export "ZFFTI" "ZFFTF" "ZFFTB"))

;; Stolen from f2cl.  
(defpackage "FORTRAN-TO-LISP"
  (:use "CL")
  (:documentation "The package holding all symbols need by the Fortran to Lisp converter")
  (:nicknames "F2CL")
  (:export
   ;; Constants
   "%FALSE%" "%TRUE%"
   ;; Types
   "INTEGER4" "INTEGER2" "INTEGER1" "REAL8" "REAL4" "COMPLEX8" "COMPLEX16"
   "ARRAY-DOUBLE-FLOAT" "ARRAY-SINGLE-FLOAT" "ARRAY-INTEGER4" "LOGICAL"
   ;; Macros
   "FREF" "FSET"
   "F2CL-INIT-STRING" "FREF-STRING" "FSET-STRING" "F2CL-SET-STRING"
   "F2CL-//" "FSTRING-/=" "FSTRING-=" "FSTRING->" "FSTRING->=" "FSTRING-<" "FSTRING-<="
   "FORTRAN_COMMENT" "FDO" "F2CL/" "ARITHMETIC-IF" "COMPUTED-GOTO"
   "ASSIGNED-GOTO"
   "FFORMAT"
   "DATA-IMPLIED-DO"
   ;; Utilities
   "ARRAY-SLICE" "ARRAY-INITIALIZE"
   ;; Intrinsic functions
   "ABS" "ACOS" "AIMAG" "AINT" "ALOG" "ALOG10" "AMAX0" "AMAX1"
   "AMIN1" "AMOD" "ANINT" "ASIN" "ATAN" "ATAN2"
   "CABS" "CEXP" "FCHAR" "CLOG" "CMPLX" "CONJG" "CCOS"
   "CSIN" "CSQRT" "DABS" "DACOS" "DASIN"
   "DATAN" "DATAN2" "DBLE" "DCOS" "DCOSH" "DEXP" "DIM"
   "DINT" "DLOG" "DLOG10" "DMAX1" "DMIN1" "DMOD"
   "DNINT" "DPROD" "DSIGN" "DSIN" "DSINH" "DSQRT" "DTAN"
   "DTANH" "FFLOAT" "IABS" "ICHAR" "IDIM" "IDINT"
   "IDNINT" "IFIX" "INDEX" "INT" "ISIGN" "LE" "LEN"
   "LGE" "LGT" "FLOG" "LOG10" "LT" "MAX" "MAX0"
   "MAX1" "MIN0" "MIN1" "NINT" "FREAL"
   "SIGN" "SNGL" "FSQRT"
   ;; Utilities
   "R1MACH" "D1MACH" "I1MACH"
   ))
    
(defpackage "QUADPACK"
  (:use "COMMON-LISP" "FORTRAN-TO-LISP")
  (:export
   ;; Do we want to export the core integration routines too?

   ;; The basic integrators
   "DQAGE" "DQAGIE" "DQAGPE" "DQAGSE" "DQAWFE" "DQAWOE" "DQAWSE" "DQAWCE"
   ;; Simplified interface routines
   "DQNG" "DQAG" "DQAGS" "DQAGI" "DQAWS" "DQAWC"))

(defpackage "MINPACK"
  (:use "COMMON-LISP" "FORTRAN-TO-LISP")
  (:export
   "LMDIF1"))

(defpackage "MATLISP-LIB"
  (:use "COMMON-LISP" "F2CL")
  (:export
   "ZEROIN"))

(defpackage "MATLISP"
    (:use "COMMON-LISP" "FORTRAN-FFI-ACCESSORS" "BLAS" "LAPACK" "DFFTPACK" "QUADPACK" "MATLISP-LIB")
    (:nicknames "MATRIX" "M")
    (:export
     "*PRINT-MATRIX*"
     "AXPY!"
     "AXPY"
     "COL-VECTOR-P"
     "COMPLEX-COERCE"
     "COMPLEX-MATRIX"
     "COMPLEX-MATRIX-ARRAY-TYPE"
     "COMPLEX-MATRIX-ELEMENT-TYPE"
     "COMPLEX-MATRIX-STORE-TYPE"
     "COPY!"
     "COPY"
     "CTRANSPOSE"
     "DIAG"
     "DOT"
     "EIG"
     "EYE"
     "FFT"
     "FFT"
     "FILL-MATRIX"
     "FLOAT-MATRIX"
     "FLOAT-MATRIX-ARRAY-TYPE"
     "FLOAT-MATRIX-ELEMENT-TYPE"
     "FORTRAN-COMPLEX-MATRIX-INDEXING"
     "FORTRAN-MATRIX-INDEXING"
     "GEEV"
     "GEMM!"
     "GEMM"
     "GESV!"
     "GESV"
     "GETRF!"
     "HELP"
     "IFFT"
     "IMAG"
     "JOIN"
     "LOAD-BLAS-&-LAPACK-BINARIES"
     "LOAD-BLAS-&-LAPACK-LIBRARIES"
     "LOAD-MATLISP"
     "LU"
     "M*!"
     "M*"
     "M+!"
     "M+"
     "M-"
     "M.*!"
     "M.*"
     "M.+!"
     "M.+"
     "M.-"
     "M./!"
     "M./"
     "M/!"
     "M/"
     "MACOS"
     "MACOSH"
     "MAKE-COMPLEX-MATRIX"
     "MAKE-COMPLEX-MATRIX-DIM"
     "MAKE-FLOAT-MATRIX"
     "MAKE-FLOAT-MATRIX-ARRAY"
     "MAKE-FLOAT-MATRIX-DIM"
     "MAKE-FLOAT-MATRIX-SEQ"
     "MAKE-FLOAT-MATRIX-SEQ-OF-SEQ"
     "MAKE-FLOAT-MATRIX-SEQUENCE"
     "MAKE-REAL-MATRIX"
     "MAKE-REAL-MATRIX-DIM"
     "MAP-MATRIX!"
     "MAP-MATRIX"
     "MASIN"
     "MASINH"
     "MATAN"
     "MATANH"
     "MATLISP-HERALD"
     "MATLISP-VERSION"
     "MATRIX-REF"
     "MCOS"
     "MCOSH"
     "MEXP"
     "MLOG"
     "MLOG10"
     "MREF"
     "MSIN"
     "MSINH"
     "MSQRT"
     "MTAN"
     "MTANH"
     "NCOLS"
     "NORM"
     "NROWS"
     "NUMBER-OF-COLS"
     "NUMBER-OF-ELEMENTS"
     "NUMBER-OF-ELEMS"
     "NUMBER-OF-ROWS"
     "ONES"
     "PRINT-ELEMENT"
     "QR"
     "QR!"
     "GEQR!"
     "RAND"
     "REAL"
     "REAL-MATRIX"
     "REAL-MATRIX-ELEMENT-TYPE"
     "REAL-MATRIX-STORE-TYPE"
     "RESHAPE!"
     "RESHAPE"
     "ROW-OR-COL-VECTOR-P"
     "ROW-VECTOR-P"
     "SAVE-MATLISP"
     "SCAL!"
     "SCAL"
     "SEQ"
     "SET-M*!-SWAP-SIZE"
     "SIZE"
     "SQUARE-MATRIX-P"
     "STANDARD-MATRIX"
     "SUM"
     "SVD"
     "SWAP!"
     "TR"
     "TRANSPOSE"
     "VEC"
     "UNLOAD-BLAS-&-LAPACK-LIBRARIES"
     "ZEROS"
     ;; From Quadpack
     "INTEGRATE-QNG"
     "INTEGRATE-QAG"
     "INTEGRATE-QAGS"
     "INTEGRATE-QAGI"
     "INTEGRATE-QAWS"
     "INTEGRATE-QAWC"
     ;; From CPOLY
     "POLYROOTS"
     ;; From TOMS-715
     "M-NORMAL-CDF"
     "M-BESSEL-SCALED-I0" "M-BESSEL-SCALED-I1"
     "M-BESSEL-SCALED-K0" "M-BESSEL-SCALED-K1"
     "M-BESSEL-I0" "M-BESSEL-I1"
     "M-BESSEL-J0" "M-BESSEL-J1"
     "M-BESSEL-K0" "M-BESSEL-K1"
     "M-BESSEL-Y0" "M-BESSEL-Y1"
     "M-DAWSON-INTEGRAL"
     "M-ERF" "M-ERFC" "M-ERFCX"
     "M-GAMMA" "M-LOG-GAMMA"
     "M-BESSEL-SERIES-I"
     "M-BESSEL-SERIES-J"
     "M-BESSEL-SERIES-K"
     "M-BESSEL-SERIES-Y"
     ))

(defpackage "MATLISP-USER"
  (:use "COMMON-LISP" "MATLISP" #+:allegro "EXCL" #+:cmu "EXT"))

(in-package "MATLISP")

(eval-when (load eval compile)
(defparameter *matlisp-version* "Pre 2.0")
#-(or :cmu :allegro) (error 
		      "MATLISP version ~a requires CMUCL or ALLEGRO CL" 
		      *matlisp-version*)
(defun matlisp-version () *matlisp-version*)
(defun matlisp-herald () (format nil "    MATLISP/~a" (matlisp-version)))
#+:cmu (setf (getf ext:*herald-items* :matlisp)
	     (list (matlisp-herald))))
