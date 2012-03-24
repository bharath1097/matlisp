;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: :matlisp; Base: 10 -*-

(in-package #:matlisp)

(cffi:use-foreign-library colnew)

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
		   (z (* :double-float :size (aref m 0)) :input)
		   (f (* :double-float :size (aref m 0)) :output)))
  (dfsub (:callback :void
		    (x :double-float :input)
		    (z (* :double-float :size (aref m 0)) :input)
		    (df (* :double-float :size (aref m 0)) :output)))
  (gsub (:callback :void
		   (i :integer :input)
		   (z (* :double-float :size (aref m 0)) :input)
		   (g (* :double-float :size (aref m 0)) :output)))
  (dgsub (:callback :void
		    (i :integer :input)
		    (z (* :double-float :size (aref m 0)) :input)
		    (dg (* :double-float :size (aref m 0)) :output)))
  (guess (:callback :void
		    (x :double-float :input)
		    (z (* :double-float) :output)
		    (dmval (* :double-float) :output))))


(def-fortran-routine appsln :void
  (x :double-float :input)
  (z (* :double-float) :output)
  (fspace (* :double-float) :input)
  (ispace (* :double-float) :input))
    
