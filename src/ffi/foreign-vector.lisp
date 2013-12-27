(in-package #:matlisp-ffi)

(defstruct (foreign-vector
	     (:conc-name fv-))
  (pointer (cffi:null-pointer)
	   :type cffi:foreign-pointer)
  (size 0 :type fixnum)
  (type nil :type symbol))

(defmethod print-object ((obj foreign-vector) stream)
  (format stream "#F(")
  (dotimes (i (fv-size obj))
    (format stream "~A " (fv-ref obj i)))
  (format stream ")"))

(defun fv-ref (x n)
  (declare (type foreign-vector x)
	   (type fixnum n))
  (assert (< -1 n (fv-size x)) nil 'out-of-bounds-error :requested n :bound (fv-size x) :message "From inside fv-ref.")
  (cffi:mem-aref (fv-pointer x) (fv-type x) n))

(defun (setf fv-ref) (value x n)
  (declare (type foreign-vector x)
	   (type fixnum n))
  (assert (< -1 n (fv-size x)) nil 'out-of-bounds-error :requested n :bound (fv-size x) :message "From inside fv-ref.")
  (setf (cffi:mem-aref (fv-pointer x) (fv-type x) n) value))

;;; Rudimentary support for making it a bit easier to deal with Fortran
;;; arrays in callbacks.

;; If the Array dimensions are (d1, d2, d3, ...)
;;
;; Then x(n1, n2, n3, ...) means the index is, essentially,
;;
;; n1 + d1*(n2 + d2*(n3 + d3*(n4 + d4*(n5))))
;;
;; Return an expression that computes the column major index given the
;; indices (a list) and a list of the bounds on each dimension.  Each
;; bound is a list of the upper and lower bounds for each dimension.
;; For example, for the Fortran array declared as x(3:10, -4:2), the
;; bounds would be written as ((3 10) (-4 2)).  If the lower bound is
;; the default of 1, you can omit the lower bound.
(defun col-major-index (indices dims)
  ;; Return a form that computes the column major index of a Fortran array.
  (flet ((get-offset (n bound)
	   (let ((lo (first bound)))
	     (if (and (numberp lo) (zerop lo))
		 n
		 `(the fixnum (- (the fixnum ,n) (the fixnum ,lo))))))
	 (get-size (bound)
	   (destructuring-bind (lo hi)
	       bound
	     (cond ((numberp lo)
		    (cond ((numberp hi)
			   (1+ (- hi lo)))
			  ((= lo 1)
			   hi)
			  (t
			   `(- ,hi ,(- lo 1)))))
		   (t
		    `(the fixnum (- ,hi (the fixnum (- (the fixnum ,lo) 1)))))))))
    (let* ((rev-idx (reverse indices))
	   (rev-dim (reverse dims))
	   (idx (get-offset (first rev-idx) (first rev-dim))))
      (do ((d (rest rev-dim) (rest d))
	   (n (rest rev-idx) (rest n)))
	  ((endp d)
	   idx)
	(setf idx `(the fixnum (+ ,(get-offset (first n) (first d))
				  (the fixnum (* ,(get-size (first d)) ,idx)))))))))

(defmacro with-fortran-matrix ((name fv &rest dims) &body body)
  (let ((indices (gensym (symbol-name '#:indices-))))
    `(macrolet ((,name (&rest ,indices)
		  `(fv-ref ,',fv ,(col-major-index ,indices ',dims))))
       ,@body)))

;; WITH-FORTRAN-MATRICES is a convenience macro for accessing Fortran
;; arrays that have been passed in as parameters of a callback.
;;
;; For example, Fortran callback function that might be
;;
;;   subroutine sub(z, f)
;;   real z(4), df(2, 4)
;;   df(1,4) = 3*z(2)
;;   end
;;
;; This can be written in a Lisp call back as
;;
;;   (defun fsub (z-arg f-arg)
;;     (with-fortran-matrices ((z z-arg (1 4))
;;                             (f f-arg ((1 2) (1 4))))
;;       (setf (f 1 4) (* 3 (z 2)))))
;;

(defmacro with-fortran-matrices ((&rest array-list) &body body)
  (if (cdr array-list)
      `(with-fortran-matrix ,(car array-list)
	 (with-fortran-matrices ,(cdr array-list)
	   ,@body))
      `(with-fortran-matrix ,(car array-list)
	 ,@body)))
