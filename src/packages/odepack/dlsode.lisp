(in-package #:matlisp)

(cffi:define-foreign-library libodepack
  (t (:default "libodepack")))

(cffi:use-foreign-library libodepack)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-fortran-routine dlsode :void
  "DLSODE in ODEPACK"
  (field (:callback dlsode-field-callback
	  :void
	  (c-neq :integer :input)
	  (c-t :double-float :input)
	  (c-y (* :double-float :size c-neq) :input)
	  (c-ydot (* :double-float :size c-neq) :output)))
  (neq :integer :input)
  (y (* :double-float :inc head-y) :input-output)
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
  (jacobian (:callback dlsode-jacobian-callback
	     :void
	     (c-neq :integer :input)
	     (c-t :double-float :input)
	     (c-y (* :double-float :size c-neq) :input)
	     (c-upper-bandwidth :integer :input)
	     (c-lower-bandwidth :integer :input)
	     (c-pd (* :double-float :size (* c-neq c-neq)) :output)
	     (c-nrowpd :integer :input)))
  (mf :integer :input))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ode-evolve (field y0 t-array)
  (declare (type real-tensor y0 t-array))
  (assert (and (tensor-vectorp t-array) (tensor-vectorp y0)) nil 'tensor-dimension-mismatch)
  (let* ((neq (size y0))
	 (nt (size t-array))
	 (t0 (ref t-array 0))
	 (ts t0)
	 (ret (zeros (list neq nt) 'real-tensor))
	 ;;
	 (lrw (+ 22 (* 9 neq) (* neq neq) 5))
	 (liw (+ 20 neq 5))
	 (itol 1)
	 (atol (make-array 1 :element-type 'double-float :initial-element 1d-12))
	 (rtol (make-array 1 :element-type 'double-float :initial-element 1d-12))
	 (itask 1)
	 (istate 1)
	 (iopt 0)
	 (mf 22)
	 (rwork (make-array lrw :element-type 'double-float :initial-element 0d0))
	 (iwork (make-array liw :element-type '(signed-byte 32) :initial-element 0))
	 ;;
	 (view (slice~ ret 1))
	 (stv (aref (strides ret) 1))
	 ;;
	 (y-tmp (zeros neq 'real-tensor))
	 (stoy (store y-tmp)))
    (copy! y0 view)
    (incf (slot-value view 'head) stv)
    (labels ((field-sugar (neq time yf ydotf)
	       (loop :for i :from 0 :below neq
		  :do (t/store-set real-tensor (fv-ref yf i) stoy i))
	       ;;Because of some black magic, this does not seem to
	       ;;affect the amount of memory allocated!
	       (let ((ydot (funcall field time y-tmp)))
		 (loop :for i :from 0 :below neq
		    :do (setf (fv-ref ydotf i) (ref ydot i))))
	       nil))
      (loop :for i :from 1 :below nt
	 :do (let ((tout (ref t-array i)))
	       (multiple-value-bind (y-out ts-out istate-out rwork-out iwork-out)
		   (dlsode #'field-sugar neq (store y0) ts tout itol rtol atol itask istate iopt rwork lrw iwork liw #'(lambda (&rest th) (declare (ignore th))) mf (head y0))
		 (declare (ignore y-out rwork-out iwork-out))
		 (setq ts ts-out)
		 (setq istate istate-out))
	       (copy! y0 view)
	       (incf (slot-value view 'head) stv))))
    ret))
;;
  
(defun lsode-evolve (field y t-array report)
  ;;
  (let* ((neq (length y))
	 (lrw (+ 22 (* 9 neq) (* neq neq) 5))
	 (liw (+ 20 neq 5))
	 (tout 0d0)
	 (ts (aref t-array 0))
	 (tout (aref t-array 0))
	 (itol 1)
	 (atol (make-array 1 :element-type 'double-float :initial-element 1d-12))
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
		(dlsode field neq (store y) ts tout itol rtol atol itask istate iopt rwork lrw iwork liw #'(lambda (&rest th) (declare (ignore th))) mf)
	      (setq ts ts-out)
	      (setq istate istate-out))
	    (funcall report ts y)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pend-field (neq time y ydot)
  (setf (fv-ref ydot 0) (fv-ref y 1)
	(fv-ref ydot 1) (- (sin (fv-ref y 0)))))

(defun pend-report (ts y)
  (format t "~A ~A ~A ~%" ts (aref y 0) (aref y 1)))

(defun pcart-field (time y ydot)
  (declare (ignore time))
  (destructuring-bind (x theta xdot thetadot) (mapslice #'id y)
    (setf (ref ydot 0) xdot
	  (ref ydot 1) thetadot
	  (ref ydot 2) (/ (+ (* (cos theta) (sin theta)) (* (sin theta) (expt thetadot 2))) (- 2 (expt (cos theta) 2)))
          (ref ydot 3) (/ (+ (* 2 (sin theta)) (* (cos theta) (sin theta) (expt thetadot 2))) (- (expt (cos theta) 2) 2)))))

(defun pcart-field (time y)
  (declare (ignore time))
  (let ((ydot (zeros 4 'real-tensor)))
    (destructuring-bind (x theta xdot thetadot) (mapslice #'id y)
      (setf (ref ydot 0) xdot
	    (ref ydot 1) thetadot
	    (ref ydot 2) (/ (+ (* (cos theta) (sin theta)) (* (sin theta) (expt thetadot 2))) (- 2 (expt (cos theta) 2)))
	    (ref ydot 3) (/ (+ (* 2 (sin theta)) (* (cos theta) (sin theta) (expt thetadot 2))) (- (expt (cos theta) 2) 2))))
    ydot))


(defun pcart-report (ts y)
  (format t "~A ~A ~A ~A ~A ~%" ts (aref y 0) (aref y 1) (aref y 2) (aref y 3)))


(defun pcart-field (time y ydot)
  (declare (ignore time))
    (destructuring-bind (x theta xdot thetadot) (mapcar #'(lambda (n) (fv-ref y n)) '(0 1 2 3))
    (declare (type double-float x theta xdot thetadot))
    (setf (fv-ref ydot 0) xdot
	  (fv-ref ydot 1) thetadot
	  (fv-ref ydot 2) (/ (+ (* (cos theta) (sin theta)) (* (sin theta) (expt thetadot 2))) (- 2 (expt (cos theta) 2)))
          (fv-ref ydot 3) (/ (+ (* 2 (sin theta)) (* (cos theta) (sin theta) (expt thetadot 2))) (- (expt (cos theta) 2) 2))))))
			
(defun pcart-report (ts y)
  (format t "~A ~A ~A ~A ~A ~%" ts (aref y 0) (aref y 1) (aref y 2) (aref y 3)))


#+nil
(let ((y (make-array 2 :element-type 'double-float :initial-contents `(,(/ pi 2) 0d0))))
  (lsode-evolve #'pend-field y #(0d0 1d0 2d0) #'pend-report))
;; Should return
;; 1.0d0 1.074911802207049d0 -0.975509986605856d0
;; 2.0d0 -0.20563950412081608d0 -1.3992359518735706d0

(ode-evolve #'pcart-field (copy! (list 0 (/ pi 3) 0 0) (zeros 4)) (range 0 10))


#+nil
(let ((y (make-array 4 :element-type 'double-float :initial-contents `(0d0 ,(/ pi 3) 0d0 0d0)))
      (ts (make-array 11 :element-type 'double-float :initial-contents (loop :for i :from 0 :to 10
									  :collect (coerce i 'double-float)))))
  (time (lsode-evolve #'pcart-field y ts #'pcart-report)))
;; Should return
;; 1.0 0.1794874587619304 0.5317592902679298 0.46247076075331184 -1.073122136936815 
;; 2.0 0.7546873547963733 -0.6988651690131225 0.3317944848774514 -0.8667875783856668 
;; 3.0 0.8622986451947015 -1.032477629437877 -0.04382530605214124 0.1709611368887168 
;; 4.0 0.5946940935220925 -0.32928104576674966 -0.6014830191620928 1.2712646406898929 
;; 5.0 0.06363367930511842 0.8312257489444103 -0.22610367475002707 0.6709599188446416 
;; 6.0 0.015513214905789278 0.9881309198657862 0.09468174702976984 -0.3441398956696562 
;; 7.0 0.38441646507049126 0.09734614673612564 0.6971343020352996 -1.4009010238635045 
;; 8.0 0.8340714151347116 -0.9308326125800139 0.14520534336799193 -0.4863145646496502 
;; 9.0 0.8288509626732954 -0.913548305194868 -0.15954504310515494 0.5222932357142753 
;; 10.0 0.36071471240686237 0.14510464752601557 -0.6851579582975589 1.3848698186034156 
