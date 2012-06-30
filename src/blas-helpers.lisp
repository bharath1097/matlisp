(in-package :matlisp)

(definline idx-max (seq)
  (declare (type (index-array *) seq))
  (reduce #'max seq))

(definline idx-min (seq)
  (declare (type (index-array *) seq))
  (reduce #'min seq))

(defun idx= (a b)
  (declare (type (index-array *) a b))
  (when (= (length a) (length b))
    (very-quickly
      (loop
	 for ele-a across a
	 for ele-b across b
	 unless (= ele-a ele-b)
	 do (return nil)
	 finally (return t)))))

(definline idx->list (a)
  (declare (type (index-array *) a))
  (loop for ele across a
       collect ele))

(defun blas-copyable-p (ten-a ten-b)
  ;; (declare (type standard-tensor ten-a ten-b))
  ;; (let ((stdi-a (very-quickly
  ;; 		  (sort (apply #'vector
  ;; 			       (loop
  ;; 				  for std across (strides ten-a)
  ;; 				  and dim across (dimensions ten-a)
  ;; 				  collect `(,std ,dim)))
  ;; 			#'< :key #'first))))
  ;;   t))


   (let ((stdi-list (very-quickly
		     (loop
			for ten of-type standard-tensor in tensors
			and pten = nil then ten
			for i of-type index-type = 0 then (1+ i)
			when (> i 0)
			do (unless (idx= (dimensions ten) (dimensions pten))
			     (return nil))
			collect (progn
				  (assert (typep ten 'standard-tensor) nil
					  'invalid-type :given (type-of ten) :expected 'standard-tensor)
				  (very-quickly
				    (sort (apply #'vector
						 (loop
						    for std of-type index-type across (strides ten)
						    and dim of-type index-type across (dimensions ten)
						    collect `(,std ,dim)))
					  #'< :key #'car)))))))
    (if (null stdi-list) (values nil nil)
	(very-quickly
	  (loop
	     for stdi in stdi-list
	     and p-stdi = (first stdi-list) then stdi
	     for i of-type index-type = 0 then (1+ i)
	     when (> i 0)
	     do (unless (loop
			   for a-stdi across stdi
			   and a-aoff = (first (aref stdi 0)) then (* a-aoff (second a-stdi))
			   for b-stdi across p-stdi
			   and b-aoff = (first (aref p-stdi 0)) then (* b-aoff (second b-stdi))
			   do (unless (and (= (first a-stdi) a-aoff)
					   (= (first b-stdi) b-aoff)
					   (= (second a-stdi) (second b-stdi)))
				(return nil))
			   finally (return t))
		  (return (values t nil)))
	     finally (return (values t (mapcar #'(lambda (x) (first (aref x 0))) stdi-list))))))))

(defun consecutive-store-p (tensor)
  (declare (type standard-tensor tensor))
  (let ((strides (strides tensor))
	(dims (dimensions tensor)))
    (declare (type (index-array *) strides dims))
    (let* ((stride-dims (very-quickly
			  (sort (apply #'vector
				       (loop
					  for std across strides
					  and dim across dims
					  collect `(,std ,dim)))
				#'< :key #'car)))
	   (stride-min (first (aref stride-dims 0))))
      (declare (type index-type stride-min)
	       (type (simple-vector *) stride-dims))
      (very-quickly
	(loop
	   for st-di across stride-dims
	   and accumulated-off = stride-min then (* accumulated-off (second st-di))
	   unless (= (first st-di) accumulated-off) do (return nil)
	   finally (return stride-min))))))


;; (defun blas-matrix-compatible-p (matrix &optional (op :n))
;;   (declare (optimize (safety 0) (speed 3))
;; 	   (type (or real-matrix complex-matrix) matrix))
;;   (mlet* (((rs cs) (slot-values matrix '(row-stride col-stride))
;; 	   :type (fixnum fixnum)))
;; 	 (cond
;; 	   ((= cs 1) (values :row-major rs (fortran-nop op)))
;; 	   ((= rs 1) (values :col-major cs (fortran-op op)))
;; 	   ;;Lets not confound lisp's type declaration.
;; 	   (t (values nil -1 "?")))))

;; (definline fortran-op (op)
;;   (ecase op (:n "N") (:t "T")))

;; (definline fortran-nop (op)
;;   (ecase op (:t "N") (:n "T")))

;; (defun fortran-snop (sop)
;;   (cond
;;     ((string= sop "N") "T")
;;     ((string= sop "T") "N")
;;     (t (error "Unrecognised fortran-op."))))
