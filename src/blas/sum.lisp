(in-package #:matlisp)

(deft/generic (t/sum #'subtypep) sym (x ret &optional axis))
(deft/method t/sum (sym standard-tensor) (x ret &optional (axis 0))
  (if (null ret)
      (using-gensyms (decl (x) (ret idx ref))
	`(let (,@decl
	       (,ret (t/fid+ ,(field-type sym))))
	   (dorefs (,idx (dimensions ,x))
		   ((,ref ,x :type ,sym))
		   (setf ,ret (t/f+ ,(field-type sym) ,ret ,ref)))
	   ,ret))
      (using-gensyms (decl (x axis ret))
	(with-gensyms (view argstd)
	  `(let* (,@decl)
	     (declare (type ,sym ,x ,ret)
		      (type index-type ,axis))
	     (let ((,view (slice~ ,x ,axis))
		   (,ret (if (= (order ,ret) (order ,x)) (slice~ ,ret ,axis) ,ret))
		   (,argstd (strides ,x ,axis)))
	       (declare (type ,sym ,view)
			(type index-type ,argstd))
	       (loop :for i :from 0 :below (dimensions ,x ,axis)
		  :do (progn
			(axpy! (t/fid* ,(field-type sym)) ,view ,ret)
			(incf (slot-value ,view 'head) ,argstd))))
	     ,ret)))))
;;
(defun reduce-check (x y axis)
  (declare (base-tensor x y))
  (let ((dims-x (dimensions x))	(dims-y (dimensions y))
	(axis (modproj (or axis 0) (order x) nil 0)))
    (declare (type index-store-vector dims-x dims-y))
    (cond
      ((= (1- (order x)) (order y))
       (loop :for i :from 0 :below (order x)
	  :and j := 0 :then (if (= i axis) j (1+ j))
	  :always (or (= i axis) (= (aref dims-x i) (aref dims-y j)))))
      ((= (order x) (order y))
       (loop :for i :from 0 :below (order x)
	  :always (if (= i axis) (= (aref dims-y i) 1) (= (aref dims-x i) (aref dims-y i))))))))

(defgeneric sum! (x y &optional axis beta)
  (:documentation "
  (SUM! x y [axis 0] [beta 0])

       --
  y <- \  x(:, : ..., i, :, :..)
       /_
	i
  where the index to be summed over is chosen using @arg{axis}.
")
  (:method :before ((x standard-tensor) (y standard-tensor) &optional (axis 0) beta)
	   (declare (ignore beta))
	   (assert (reduce-check x y axis) nil 'tensor-dimension-mismatch)))

(define-tensor-method sum! ((x standard-tensor :input) (y standard-tensor :output) &optional (axis 0) beta)
  `(t/sum ,(cl y) x (if beta (scal! beta y) (copy! (t/fid+ ,(field-type (cl y))) y)) axis))

(define-tensor-method sum! ((x standard-tensor :input) (y (eql nil)) &optional (axis 0) beta)
  `(declare (ignore axis beta))
  `(t/sum ,(cl x) x nil))
;;
(defgeneric prod! (x y &optional axis beta)
  (:documentation "
  (PROD! x y [axis 0])
       __
  y <- || x(:, : ..., i, :, :..)
	i
  where the index to be summed over is chosen using @arg{axis}.
")
  (:method :before ((x standard-tensor) (y standard-tensor) &optional (axis 0) beta)
	   (declare (ignore beta))
	   (assert (reduce-check x y axis) nil 'tensor-dimension-mismatch)))

(labels ((*-ify (code)
	   (maptree `(t/fid+ t/f+ axpy!)
	     #'(lambda (x) (values (ecase (car x)
				     (t/fid+ `(t/fid* ,@(cdr x)))
				     (axpy! `(scal! ,@(cddr x)))
				     (t/f+ `(t/f* ,@(cdr x))))
				   #'mapcar))
	     (macroexpand-1 code))))
  ;;Don't you just love lisp :)
  (define-tensor-method prod! ((x standard-tensor :input) (y standard-tensor :output) &optional (axis 0) beta)
    (*-ify `(t/sum ,(cl y) x (if beta (scal! beta y) (copy! (t/fid* ,(field-type (cl y))) y)) axis)))
  (define-tensor-method prod! ((x standard-tensor :input) (y (eql nil)) &optional axis beta)
    `(declare (ignore axis beta))
    (*-ify `(t/sum ,(cl x) x nil))))
;;
(defgeneric sum (x &optional axis preserve-rank?)
  (:method ((x standard-tensor) &optional (axis 0) (preserve-rank? nil))
    (if axis
	(let ((axis (modproj axis (order x))))
	  (sum! x (let ((dims (loop :for ele :across (dimensions x)
				 :for i := 0 :then (1+ i)
				 :when (if preserve-rank? t (/= i axis)) :collect (if (= i axis) 1 ele))))
		    (and dims (zeros dims (class-of x))))
		axis))
	(sum! x nil)))
  (:method ((x number) &optional axis preserve-rank?)
    (declare (ignore axis preserve-rank?))
    x))

(defgeneric mean (x &optional axis preserve-rank?)
  (:method ((x standard-tensor) &optional (axis 0) (preserve-rank? nil))
    (let ((s (sum x axis preserve-rank?))
	  (n.d (if axis (/ (dimensions x axis)) (size x))))
      (if (numberp s) (* s n.d)
	  (scal! n.d s))))
  (:method ((x number) &optional axis preserve-rank?)
    (declare (ignore axis preserve-rank?))
    x))

(defgeneric prod (x &optional axis preserve-rank?)
  (:method ((x standard-tensor) &optional (axis 0) (preserve-rank? nil))
    (if axis
	(let ((axis (modproj axis (order x))))
	  (prod! x (let ((dims (loop :for ele :across (dimensions x)
				 :for i := 0 :then (1+ i)
				 :when (if preserve-rank? t (/= i axis)) :collect (if (= i axis) 1 ele))))
		    (and dims (zeros dims (class-of x))))
		axis))
	(prod! x nil))))

(definline normalize! (x) (scal! (/ (sum x nil)) x))
