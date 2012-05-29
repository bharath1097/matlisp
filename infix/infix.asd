;;;; -*- Mode: Lisp; Package: User; -*-

(defpackage #:infix-system (:use #:asdf #:cl))
(in-package #:infix-system)

(defsystem infix
  :components ((:file "src")))
