(in-package #:matlisp)

;;
(defclass compressed-sparse-matrix (sparse-tensor)
  ((transpose? :initform nil :initarg :transpose? :reader transpose? :type boolean
	       :documentation "If NIL the matrix is in CSC, else if T, then matrix is CSR.")
   (neighbour-start :initarg :neighbour-start :reader neighbour-start :type index-store-vector
		    :documentation "Start index for ids and store.")
   (neighbour-id :initarg :neighbour-id :reader neighbour-id :type index-store-vector
		 :documentation "Row id.")))

(defun compressed-sparse-indexing (subs tensor)
  (declare (type compressed-sparse-matrix tensor)
	   (type (or index-store-vector cons) subs))
  (let-typed ((row 0 :type index-type)
	      (col 0 :type index-type))
     (etypecase subs
       (cons
	(assert (null (cddr subs)) nil 'tensor-index-rank-mismatch)
	(setf row (the index-type (car subs))
	      col (the index-type (cadr subs))))
       (index-store-vector
	(assert (= (length subs) 2) nil 'tensor-index-rank-mismatch)
	(setf row (the index-type (aref subs 0))
	      col (the index-type (aref subs 1)))))
     (when (transpose? tensor)
       (rotatef row col))     
     (let*-typed ((nst (neighbour-start tensor) :type index-store-vector)
		  (nid (neighbour-id tensor) :type index-store-vector)
		  (lb (aref nst col) :type index-type)
		  (ub (aref nst (1+ col)) :type index-type))
       (declare (type index-type row col))
       (if (or (= lb ub) (< row (aref nid lb)) (> row (aref nid (1- ub)))) -1
	   (very-quickly
	     (loop :with j := (ash (+ lb ub) -1)
		:repeat 64
		:do (progn
		      #+nil(format t "~a, ~a, ~a~%" lb j ub)
		      (cond
			((= (aref nid j) row) (return j))
			((>= lb (1- ub)) (return -1))
			(t
			 (if (< row (aref nid j))
			     (setf ub j)
			     (setf lb (1+ j)))
			 (setf j (ash (+ lb ub) -1)))))))))))

(deft/method t/store-allocator (sym compressed-sparse-matrix) (size &optional initial-element)
  (let ((sto-type (store-element-type sym)))
    (using-gensyms (decl (size))
      `(let (,@decl)
	 (destructuring-bind (ni nz) (t/compute-store-size ,sym ,size)
	   (list
	    (allocate-index-store (1+ ni))
	    (allocate-index-store nz)
	    (make-array nz :element-type ',sto-type :initial-element ,(if (subtypep sto-type 'number) `(t/fid+ ,sto-type) nil))))))))

(deft/method t/compute-store-size (sym compressed-sparse-matrix) (size)
  `(destructuring-bind (nr nc &optional nz) ,size
     (list nc (or nz (min (ceiling (* nr nc *default-sparsity*)) *max-sparse-size*)))))

(deft/method t/store-type (sym compressed-sparse-matrix) (&optional (size '*))
  `(simple-array ,(store-element-type sym) (,size)))

(deft/method t/store-ref (sym compressed-sparse-matrix) (store &rest idx)
   (assert (null (cdr idx)) nil "given more than one index for compressed-store")
  `(aref (the ,(store-type sym) ,store) (the index-type ,(car idx))))

(deft/method t/store-size (sym compressed-sparse-matrix) (ele)
  `(length ,ele))

(deft/method t/store-element-type (sym compressed-sparse-matrix) ()
  (macroexpand `(t/field-type ,sym)))

;;
(deft/method t/store-set (sym compressed-sparse-matrix) (value store &rest idx)
   (assert (null (cdr idx)) nil "given more than one index for compressed store")
  `(setf (aref (the ,(store-type sym) ,store) (the index-type ,(car idx))) (the ,(field-type sym) ,value)))
;; (deft/method t/store-set (sym coordinate-sparse-tensor) (value store &rest idx)
;;    (assert (null (cdr idx)) nil "given more than one index for hashtable.")
;;    (with-gensyms (val)
;;      `(let-typed ((,val ,value :type ,(field-type sym)))
;; 	(unless (t/f= ,(field-type sym) ,val (t/fid+ ,(field-type sym)))
;; 	  (setf (gethash ,(car idx) ,store) (the ,(field-type sym) ,value))))))


;;
(defmethod ref ((tensor compressed-sparse-matrix) &rest subscripts)
  (let ((clname (class-name (class-of tensor))))
    (assert (member clname *tensor-type-leaves*) nil 'tensor-abstract-class :tensor-class clname)
    (compile-and-eval
     `(defmethod ref ((tensor ,clname) &rest subscripts)	
	(let ((idx (compressed-sparse-indexing (if (numberp (car subscripts)) subscripts (car subscripts)) tensor)))
	  (if (< idx 0)
	      (t/sparse-fill ,clname)
	      (t/store-ref ,clname (store tensor) idx)))))
    (apply #'ref (cons tensor subscripts))))

#+nil
(defmethod (setf ref) (value (tensor compressed-sparse-matrix) &rest subscripts)
  (let ((clname (class-name (class-of tensor))))
    (assert (member clname *tensor-type-leaves*) nil 'tensor-abstract-class :tensor-class clname)
    (compile-and-eval
     `(defmethod (setf ref) (value (tensor ,clname) &rest subscripts)
	(let* ((subs (if (numberp (car subscripts)) subscripts (car subscripts)))
	       (idx (store-indexing subs tensor))
	       (sto (store tensor)))
	  (t/store-set ,clname (t/coerce ,(field-type clname) value) sto idx)
	  (t/store-ref ,clname sto idx))))
    (setf (ref tensor (if (numberp (car subscripts)) subscripts (car subscripts))) value)))
;;
(defleaf real-compressed-sparse-matrix (compressed-sparse-matrix) ())
(deft/method t/field-type (sym real-compressed-sparse-matrix) ()
  'double-float)

;;
(defmethod :before copy! ((a sparse-tensor) (b sparse-tensor))
  (assert (< (store-size a) (store-size b)) nil 'tensor-insufficient-store))

(deft/method t/zeros (class compressed-sparse-matrix) (dims &optional nz)
  `(destructuring-bind (vi vr vd) (t/store-allocator ,class (append ,dims ,@(when nz `((list ,nz)))))
     (make-instance ',class
		    :dimensions (make-index-store ,dims)
		    :neighbour-start vi
		    :neighbour-id vr
		    :store vd)))

(defmethod copy! ((x coordinate-sparse-tensor) (y compressed-sparse-matrix))
  (let ((clx (class-name (class-of x)))
	(cly (class-name (class-of y))))
    (assert (and (member clx *tensor-type-leaves*)
		 (member cly *tensor-type-leaves*))
	    nil 'tensor-abstract-class :tensor-class (list clx cly))
    (compile-and-eval
     `(defmethod copy! ((x ,clx) (y ,cly))
	(let-typed ((stds (strides x) :type index-store-vector))
	  (assert (and (tensor-matrixp x) (= (aref stds 0) 1)) nil 'tensor-invalid-stride-value)
	  (let ((col-stride (aref stds 1))
		(row-data (make-array (ncols x) :initial-element nil)))
	    (very-quickly
	      (loop :for key :being :the :hash-keys :of (store x)
		 :using (hash-value value)
		 :do (multiple-value-bind (c r) (floor (the index-type key) col-stride)
		       (push (cons r value) (aref row-data c)))))
	    (let-typed ((vi (neighbour-start y) :type index-store-vector)
			(vr (neighbour-id y) :type index-store-vector)
			(vd (store y) :type ,(store-type cly)))
	      (setf (aref vi 0) 0)
	      (very-quickly
		(loop :for i :from 0 :below (ncols x)
		   :with col-stop := 0
		   :do (let ((rowd (sort (aref row-data i) #'(lambda (x y) (< (the index-type x) (the index-type y))) :key #'car)))
			 (loop :for (r . v) :in rowd
			    :do (locally
				    (declare (type ,(field-type clx) v))
				  (setf (aref vr col-stop) r)
				  (t/store-set real-compressed-sparse-matrix (t/coerce ,(field-type cly) v) vd col-stop)
				  (incf col-stop)))
			 (setf (aref vi (1+ i)) col-stop)))))
	    y))))
    (copy! x y)))

(defmethod copy-generic ((a sparse-tensor) (type (eql 'real-compressed-sparse-matrix)))
  (let-typed ((stds (strides a) :type index-store-vector))
    (assert (and (tensor-matrixp a) (= (aref stds 0) 1)) nil 'tensor-not-matrix)
    (let ((col-stride (aref stds 1))
	  (row-data (make-array (ncols a) :initial-element nil)))
      (loop :for key :being :the :hash-keys :of (store a)
	 :using (hash-value value)
	 :do (multiple-value-bind (c r) (floor key col-stride)
	       (push (cons r value) (aref row-data c))))
      (destructuring-bind (vi vr vd) (t/store-allocator real-compressed-sparse-matrix (append (dims a) (list (store-size a))))
	(setf (aref vi 0) 0)
	(loop :for i :from 0 :below (ncols a)
	   :with col-stop := 0
	   :do (let ((rowd (sort (aref row-data i) #'< :key #'car)))
		 (loop :for (r . v) :in rowd
		    :do (progn
			  (setf (aref vr col-stop) r
				(aref vd col-stop) v)
			  (incf col-stop)))
		 (setf (aref vi (1+ i)) col-stop)))
	(make-instance 'real-compressed-sparse-matrix
		       :dimensions (copy-seq (dimensions a))
		       :neighbour-start vi
		       :neighbour-id vr
		       :store vd)))))
