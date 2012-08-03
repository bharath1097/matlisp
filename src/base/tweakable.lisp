(in-package #:matlisp)

;;Level 1--------------------------------------------------------;;
(defparameter *real-l1-fcall-lb* 20000
  "If the size of the array is less than this parameter, the
   lisp version of axpy is called in order to avoid FFI overheads")

(defparameter *complex-l1-fcall-lb* 10000
  "If the size of the array is less than this parameter, the
   lisp version of axpy is called in order to avoid FFI overheads")

;;Level 2--------------------------------------------------------;;
(defparameter *real-l2-fcall-lb* 1000
  "
  If the maximum dimension in the MV is lower than this
  parameter, then the lisp code is used by default, instead of
  calling BLAS. Used to avoid the FFI overhead when calling
  MM with small matrices.
  Default set with SBCL on x86-64 linux. A reasonable value
  is something between 800 and 2000.")

(defparameter *complex-l2-fcall-lb* 600
  "
  If the maximum dimension in the MV is lower than this
  parameter, then the lisp code is used by default, instead of
  calling BLAS. Used to avoid the FFI overhead when calling
  MM with small matrices.
  Default set with SBCL on x86-64 linux. A reasonable value
  is something between 400 and 1000.")
;;Level 3--------------------------------------------------------;;
(defparameter *real-l3-fcall-lb* 100
  "
  If the maximum dimension in the MM is lower than this
  parameter, then the lisp code is used by default, instead of
  calling BLAS. Used to avoid the FFI overhead when calling
  MM with small matrices.
  Default set with SBCL on x86-64 linux. A reasonable value
  is something between 20 and 200.")

(defparameter *complex-l3-fcall-lb* 60
  "
  If the maximum dimension in the MM is lower than this
  parameter, then the lisp code is used by default, instead of
  calling BLAS. Used to avoid the FFI overhead when calling
  MM with small matrices.
  Default set with SBCL on x86-64 linux. A reasonable value
  is something between 20 and 200.")
