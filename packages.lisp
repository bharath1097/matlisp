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
;;; $Id: packages.lisp,v 1.1 2000/06/19 22:19:33 rtoy Exp $
;;;
;;; $Log: packages.lisp,v $
;;; Revision 1.1  2000/06/19 22:19:33  rtoy
;;; Initial revision.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Define the packages and symbols for Matlisp.

(defpackage "FORTRAN-FFI-ACCESSORS"
  (:use "COMMON-LISP" "ALIEN" "C-CALL")
  (:export
   ;; Interface functions
   "DEF-FORTRAN-ROUTINE"
   "VECTOR-DATA-ADDRESS"
   "INCF-SAP"
   "WITH-VECTOR-DATA-ADDRESSES"))

(defpackage "BLAS"
  (:use "COMMON-LISP" "ALIEN" "C-CALL" "FORTRAN-FFI-ACCESSORS")
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
  (:use "COMMON-LISP" "ALIEN" "C-CALL" "FORTRAN-FFI-ACCESSORS")
  (:export
   "DGESV" "DGEEV" "DGETRF" "DGESVD"
   "ZGESV" "ZGEEV" "ZGETRF" "ZGESVD" ))

(defpackage "DFFTPACK"
  (:use "COMMON-LISP" "ALIEN" "C-CALL" "FORTRAN-FFI-ACCESSORS")
  (:export "ZFFTI" "ZFFTF" "ZFFTB"))

(defpackage "MATLISP"
    (:use "COMMON-LISP" "FORTRAN-FFI-ACCESSORS" "BLAS" "LAPACK" "DFFTPACK")
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
     "ZEROS"
     ))