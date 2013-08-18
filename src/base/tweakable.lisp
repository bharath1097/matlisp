(in-package #:matlisp)

;;These describe some global defaults governing the
;;runtime behavior of Matlisp. Although you can change
;;these by doing a setf during runtime, it is recommended
;;that you use lexical scoping to affect local changes to
;;code (global variables are only bad if you overwrite them :)

;;Default ordering of strides
(defparameter *default-stride-ordering* :col-major
  "
  Determines whether strides are row or column major by default.
  Doing:
  > (let ((*default-stride-ordering* :col-major))
      (make-real-tensor 10 10))
  returns a 10x10 matrix with Column major order.
")

(defparameter *default-tensor-type* 'real-tensor)

(defparameter *check-after-initializing?* t
  "
  If t, then check for invalid values in the field of
  the class in the :after specialized method (if defined),
  else do nothing. One ought to be very carful when doing,
  much of Matlisp's code is written on the assumption that
  the fields of a tensor don't take invalid values; failing
  which case, may lead to memory error. Use at your own risk.
")

;;Level 1--------------------------------------------------------;;
(defparameter *real-l1-fcall-lb* 5000
  "If the size of the array is less than this parameter, the
   lisp version of axpy is called in order to avoid FFI overheads.
   The Fortran function is not called if the tensor does not have
   a consecutive store (see blas-helpers.lisp/consecutive-store-p).")

(defparameter *complex-l1-fcall-lb* 2500
  "If the size of the array is less than this parameter, the
   lisp version of axpy is called in order to avoid FFI overheads.
   The Fortran function is not called if the tensor does not have
   a consecutive store (see blas-helpers.lisp/consecutive-store-p).")

;;Level 2--------------------------------------------------------;;
(defparameter *real-l2-fcall-lb* 1000
  "
  If the maximum dimension in the MV is lower than this
  parameter, then the lisp code is used by default, instead of
  calling BLAS. Used to avoid the FFI overhead when calling
  MM with small matrices. Note that if the dimensions do exceed
  this lower  bound, then the Fortran function is called even if
  the matrix has a BLAS incompatible stride (by doing a copy).

  Default set with SBCL on x86-64 linux. A reasonable value
  is something between 800 and 2000.")

(defparameter *complex-l2-fcall-lb* 600
  "
  If the maximum dimension in the MV is lower than this
  parameter, then the lisp code is used by default, instead of
  calling BLAS. Used to avoid the FFI overhead when calling
  MM with small matrices. Note that if the dimensions do exceed
  this lower bound, then the Fortran function is called even when
  the matrices have a BLAS incompatible stride (by using a copy).

  Default set with SBCL on x86-64 linux. A reasonable value
  is something between 400 and 1000.")
;;Level 3--------------------------------------------------------;;
(defparameter *real-l3-fcall-lb* 50
  "
  If the maximum dimension in the MM is lower than this
  parameter, then the lisp code is used by default, instead of
  calling BLAS. Used to avoid the FFI overhead when calling
  MM with small matrices.
  Default set with SBCL on x86-64 linux. A reasonable value
  is something between 20 and 200.")

(defparameter *complex-l3-fcall-lb* 30
  "
  If the maximum dimension in the MM is lower than this
  parameter, then the lisp code is used by default, instead of
  calling BLAS. Used to avoid the FFI overhead when calling
  MM with small matrices.
  Default set with SBCL on x86-64 linux. A reasonable value
  is something between 20 and 200.")
