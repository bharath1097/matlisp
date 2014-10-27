(in-package #:matlisp-ffi)

(eval-every
  (push (pathname #+darwin"/System/Library/Frameworks/Accelerate.framework/Frameworks/vecLib.framework/Versions/A/"
		  #+linux"/usr/lib/")
	cffi:*foreign-library-directories*)
  (map nil #'(lambda (x) (cffi:load-foreign-library `(:or (:framework :veclib)
							  (:default ,x))))
       '("libblas" "liblapack"))
  #+sbcl (setf sb-ext:*inline-expansion-limit* (max 1000 sb-ext:*inline-expansion-limit*)))
