(in-package #:matlisp)

(cffi:define-foreign-library libodepack
  #+nil(:unix #.(translate-logical-pathname
	    (merge-pathnames "matlisp:lib;libodepack"
			     *shared-library-pathname-extension*)))
  (t (:default "libodepack")))

(cffi:use-foreign-library libodepack)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+nil
(def-fortran-routine testde :void
  (field (:callback :void
		    (c-neq :integer :input)
		    (c-t :double-float :input)
		    (c-y (* :double-float :size c-neq) :input)
		    (c-ydot (* :double-float :size c-neq) :output)))
  (neq :integer :input)
  (y (* :double-float) :input-output))


(def-fortran-routine dlsode :void
  "DLSODE in ODEPACK"
  (field (:callback :void
		    (c-neq :integer :input)
		    (c-t :double-float :input)
		    (c-y (* :double-float :size c-neq) :input)
		    (c-ydot (* :double-float :size c-neq) :output)))
  (neq :integer :input)
  (y (* :double-float) :input-output)
  (ts :double-float :input-output)
  (tout :double-float :input)
  (itol :integer :input)
  (rtol (* :integer) :input)
  (atol (* :integer) :input)
  (itask :integer :input)
  (istate :integer :input-output)
  (iopt :integer :input)
  (rwork (* :double-float) :input-output)
  (lrw :integer :input)
  (iwork (* :integer) :input-output)
  (liw :integer :input)
  (jacobian (:callback :void
		       (c-neq :integer :input)
		       (c-t :double-float :input)
		       (c-y (* :double-float :size c-neq) :input)
		       (c-upper-bandwidth :integer :input)
		       (c-lower-bandwidth :integer :input)
		       (c-pd (* :double-float :size (* c-neq c-neq)) :output)
		       (c-nrowpd :integer :input)))
  (mf :integer :input))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lsode-evolve (field y t-array report)
  ;;
  (let* ((neq (length y))
	 (lrw (+ 22 (* 9 neq) (* neq neq) 5))
	 (liw (+ 20 neq 5))
	 (tout 0d0)
	 (ts (aref t-array 0))
	 (tout (aref t-array 0))
	 (itol 1)
	 (atol (make-array 1 :element-type 'double-float :initial-element 1d-8))
	 (rtol (make-array 1 :element-type 'double-float :initial-element 1d-12))
	 (itask 1)
	 (istate 1)
	 (iopt 0)
	 (mf 22)
	 (rwork (make-array lrw :element-type 'double-float :initial-element 0d0))
	 (iwork (make-array liw :element-type '(signed-byte 32) :initial-element 0)))
    (loop for i from 1 below (length t-array)
       do (progn
	    (setq tout (aref t-array i))
	    (multiple-value-bind (y-out ts-out istate-out rwork-out iwork-out)
		(dlsode field neq y ts tout itol rtol atol itask istate iopt rwork lrw iwork liw #'(lambda (&rest th) (declare (ignore th))) mf)
	      (setq ts ts-out)
	      (setq istate istate-out))
	    (funcall report ts y)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pend-field (neq time y ydot)
	(setf (fv-ref ydot 0) (fv-ref y 1)
	      (fv-ref ydot 1) (- (sin (fv-ref y 0)))))

(defun pend-report (ts y)
  (format t "~A ~A ~A ~%" ts (aref y 0) (aref y 1)))


(defun pcart-field (neq time y ydot)
  (declare (ignore neq time))
  (very-quickly
  (destructuring-bind (x theta xdot thetadot) (mapcar #'(lambda (n) (fv-ref y n)) '(0 1 2 3))
    (declare (type double-float x theta xdot thetadot))
    (setf (fv-ref ydot 0) xdot
	  (fv-ref ydot 1) thetadot
	  (fv-ref ydot 2) (/ (+ (* (cos theta) (sin theta)) (* (sin theta) (expt thetadot 2))) (- 2 (expt (cos theta) 2)))
          (fv-ref ydot 3) (/ (+ (* 2 (sin theta)) (* (cos theta) (sin theta) (expt thetadot 2))) (- (expt (cos theta) 2) 2))))))
			
(defun pcart-report (ts y)
  (declare (ignore ts y))
  #+nil
  (format t "~A ~A ~A ~A ~A ~%" ts (aref y 0) (aref y 1) (aref y 2) (aref y 3)))

#+nil
(let ((y (make-array 2 :element-type 'double-float :initial-contents `(,(/ pi 2) 0d0))))
  (lsode-evolve #'pend-field y #(0d0 1d0 2d0) #'pend-report))

(let ((y (make-array 4 :element-type 'double-float :initial-contents `(0d0 ,(/ pi 3) 0d0 0d0)))
      (ts (make-array 200 :element-type 'double-float :initial-contents (loop :for i :from 0 :below 200
									  :collect (coerce i 'double-float)))))
  (time (lsode-evolve #'pcart-field y ts #'pcart-report)))

;; Should return
;; 1.0d0 1.074911802207049d0 -0.975509986605856d0
;; 2.0d0 -0.20563950412081608d0 -1.3992359518735706d0
