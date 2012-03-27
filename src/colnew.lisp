;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: :matlisp; Base: 10 -*-

(in-package #:matlisp)

(cffi:use-foreign-library colnew)

(defun m* (ncomp m)
  (loop for k from 0 below ncomp
	sum (aref m k)))

(def-fortran-routine colnew :void
  "COLNEW"
  (ncomp :integer :input)
  (m (* :integer) :input)
  (aleft :double-float :input)
  (aright :double-float :input)
  (zeta (* :double-float) :input)
  (ipar (* :integer) :input)
  (ltol (* :integer) :input)
  (tol (* :double-float) :input)
  (fixpnt (* :double-float) :input)
  (ispace (* :integer) :input-output)
  (fspace (* :double-float) :input-output)
  (iflag :integer :output)
  (fsub (:callback :void
		   (x :double-float :input)
		   (z (* :double-float :size (m* ncomp m)) :input)
		   (f (* :double-float :size ncomp) :output)))
  (dfsub (:callback :void
		    (x :double-float :input)
		    (z (* :double-float :size (m* ncomp m)) :input)
		    (df (* :double-float :size (* ncomp (m* ncomp m))) :output)))
  (gsub (:callback :void
		   (i :integer :input)
		   (z (* :double-float :size (m* ncomp m)) :input)
		   (g (* :double-float :size (m* ncomp m)) :output)))
  (dgsub (:callback :void
		    (i :integer :input)
		    (z (* :double-float :size (m* ncomp m)) :input)
		    (dg (* :double-float :size (expt (m* ncomp m) 2)) :output)))
  (guess (:callback :void
		    (x :double-float :input)
		    (z (* :double-float :size (m* ncomp m)) :output)
		    (dmval (* :double-float :size ncomp) :output))))


(def-fortran-routine appsln :void
  (x :double-float :input)
  (z (* :double-float) :output)
  (fspace (* :double-float) :input)
  (ispace (* :double-float) :input))
    
