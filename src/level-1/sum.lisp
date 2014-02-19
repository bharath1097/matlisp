(in-package #:matlisp)

(deft/generic (t/sum #'subtypep) sym (x ret &optional axis))
(deft/method t/sum (sym standard-tensor) (x ret &optional (axis 0))
  (if (null ret)
      `(dot ,x (t/fid* ,(field-type sym)) nil)
      (using-gensyms (decl (x axis ret))
	(with-gensyms (view argstd)
	  `(let* (,@decl)
	     (declare (type ,sym ,x ,ret)
		      (type index-type ,axis))
	     (let ((,view (let ((slst (make-list (order ,x) :initial-element '(* * *))))
			    (rplaca (nthcdr ,axis slst) (list 0 '* 1))
			    (subtensor~ ,x slst nil)))
		   (,argstd (aref (the index-store-vector (strides ,x)) ,axis)))
	       (declare (type ,sym ,view)
			(type index-type ,argstd))
	       (loop :for i :from 0 :below (aref (the index-store-vector (dimensions ,x)) ,axis)
		  :do (progn
			(axpy! (t/fid* ,(field-type sym)) ,view ,ret)
			(incf (slot-value ,view 'head) ,argstd)))
	       ,ret))))))

(defgeneric sum! (x y &optional axis)
  (:documentation "
  (SUM! x y [axis 0])
        
       --            
  y <- \  x(:, : ..., i, :, :..)
       /_
        i
  where the index to be summed over is chosen using @arg{axis}.
")
  (:method :before ((x standard-tensor) (y standard-tensor) &optional (axis 0))
	   (assert (and
		    (= (1- (order x)) (order y))
		    (let ((dims-x (dimensions x))
			  (dims-y (dimensions y)))
		      (declare (type index-store-vector dims-x dims-y))
		      (loop
			 :for i :from 0 :below (order x)
			 :and j := 0 :then (if (= i axis) j (1+ j)) 
			 :do (unless (or (= i axis) (= (aref dims-x i) (aref dims-y j)))
			       (return nil))
			 :finally (return t))))
		   nil 'tensor-dimension-mismatch))
  (:method :before ((x standard-tensor) (y (eql nil)) &optional (axis 0))
	   (declare (ignore axis))
	   (assert (tensor-vectorp x) nil 'tensor-dimension-mismatch)))

(defmethod sum! ((x standard-tensor) (y t) &optional (axis 0))
  (let ((clx (class-name (class-of x))))
    (assert (member clx *tensor-type-leaves*)	    
	    nil 'tensor-abstract-class :tensor-class (list clx))
    (compile-and-eval
     `(progn
	(defmethod sum! ((x ,clx) (y ,clx) &optional (axis 0))
	  (t/sum ,clx x y axis))
	(defmethod sum! ((x ,clx) (y (eql nil)) &optional (axis 0))
	  (declare (ignore axis))
	  (t/sum ,clx x nil))))
    (sum! x y axis)))
