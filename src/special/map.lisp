(in-package #:matlisp)

(defgeneric mapsor! (func x y)
  (:documentation
"
    Syntax
    ======
    (MAPSOR! func x y)

    Purpose
    =======
    Applies the function element-wise on x, and sets the corresponding
    elements in y to the value returned by the function.

    Example
    =======
    > (mapsor! #'(lambda (idx x y)
		  (if (= (car idx) (cadr idx))
		      (sin x)
		      y))
       (randn '(2 2)) (zeros '(2 2)))
    #<REAL-TENSOR #(2 2)
    -9.78972E-2  0.0000
     0.0000     -.39243
    >
    >
")
  (:method :before ((func function) (x standard-tensor) (y standard-tensor))
	   (assert (very-quickly (lvec-eq (dimensions x) (dimensions y))) nil 'tensor-dimension-mismatch)))

(defmethod mapsor! ((func function) (x standard-tensor) (y standard-tensor))
  (let ((clx (class-name (class-of x)))
	(cly (class-name (class-of y))))
    (assert (and
	     (member clx *tensor-type-leaves*)
	     (member cly *tensor-type-leaves*))
	    nil 'tensor-abstract-class :tensor-class (list clx cly))
    (compile-and-eval
     `(defmethod mapsor! ((func function) (x ,clx) (y ,cly))
	(let-typed ((sto-x (store x) :type ,(store-type clx))
		    (sto-y (store y) :type ,(store-type cly)))
	  (mod-dotimes (idx (dimensions x))
	    :with (linear-sums
		   (of-x (strides x) (head x))
		   (of-y (strides y) (head y)))
	    :do (t/store-set ,cly (funcall func (lvec->list idx) (t/store-ref ,clx sto-x of-x) (t/store-ref ,cly sto-y of-y)) sto-y of-y)))
	y)))
  (mapsor! func x y))

(definline mapsor (func x &optional output-type)
  (let ((ret (zeros (dimensions x) (or output-type (class-of x)))))
    (mapsor! #'(lambda (idx x y) (declare (ignore idx y)) (funcall func x)) x ret)))

;;
;; (defmacro fapply ((ref tensa &key ttype) &optional (loop-optimizations '(progn)) &rest body)
;;   (with-gensyms (tens sto idx of)
;;     `(let ((,tens ,tensa))
;;        (declare (type ,ttype ,tens))
;;        (let ((,sto (store ,tens)))
;;	 (declare (type ,(store-type ttype) ,sto))
;;	 (,@loop-optimizations
;;	   (mod-dotimes (,idx (dimensions ,tens))
;;	     :with (linear-sums
;;		    (,of (strides ,tens) (head ,tens)))
;;	     :do (let ((,ref (t/store-ref ,ttype ,sto ,of)))
;;		   (declare (type ,(field-type ttype) ,ref))
;;		   (t/store-set ,ttype
;;				(progn
;;				  ,@body)
;;				,sto ,of))))
;;	 ,tens))))

;;
(defun check-dims (axlst tensors)
  (let ((axlst (if (numberp axlst) (make-list (length tensors) :initial-element axlst) axlst)))
    (iter (for x in tensors)
	  (for axis in axlst)
	  (with dims = nil)
	  (cond
	    ((typep x 'standard-tensor)
	     (let-typed ((xdims (dimensions x) :type index-store-vector))
			(assert (< axis (order x)) nil 'tensor-dimension-mismatch)
			(if (null dims)
			    (setf dims (aref xdims (mod axis (order x))))
			    (setf dims (min (aref xdims (mod axis (order x))) dims))))
	     (collect (aref (strides x) (mod axis (order x))) into strides)
	     (collect (slice~ x axis 0 (if (> (order x) 1) nil t)) into slices))
	    ((eq x nil)
	     (collect nil into strides)
	     (collect nil into slices))
	    (t (error 'invalid-arguments)))
	  (finally (return (values dims strides slices))))))

(defun mapslice (axis func tensor &rest more-tensors)
  (multiple-value-bind (d.axis strides slices) (check-dims axis (cons tensor more-tensors))
    (loop :for i :from 0 :below d.axis
       :collect (prog1 (apply func (mapcar #'copy slices))
		  (when (< i (1- d.axis))
		    (loop :for slc :in slices
		       :for std :in strides
		       :do (when slc (incf (slot-value slc 'head) std))))))))

(defun mapslice~ (axis func tensor &rest more-tensors)
  (multiple-value-bind (d.axis strides slices) (check-dims axis (cons tensor more-tensors))
   (loop :for i :from 0 :below d.axis
       :collect (prog1 (apply func slices)
		  (when (< i (1- d.axis))
		    (loop :for slc :in slices
		       :for std :in strides
		       :do (when slc (incf (slot-value slc 'head) std))))))))

(defun mapslicec~ (axis func tensor &rest more-tensors)
  (multiple-value-bind (d.axis strides slices) (check-dims axis (cons tensor more-tensors))
    (loop :for i :from 0 :below d.axis
       :do (prog1 (apply func slices)
	     (when (< i (1- d.axis))
	       (loop :for slc :in slices
		  :for std :in strides
		  :do (when slc (incf (slot-value slc 'head) std)))))))
  (values-list (cons tensor more-tensors)))
;;

(defmacro tensor-foldl (type func ten init &key (init-type (field-type type)) (key nil))
  (using-gensyms (decl (ten init))
    (with-gensyms (sto idx of funcsym keysym)
    `(let* (,@decl
	    ,@(unless (symbolp func)
		`((,funcsym ,func)))
	    ,@(unless (symbolp key)
		`((,keysym ,key)))
	    (,sto (store ,ten)))
       (declare (type ,type ,ten)
		,@(unless (symbolp func) `((type function ,funcsym)))
		,@(unless (symbolp key) `((type function ,keysym)))
		(type ,(store-type type) ,sto)
		,@(when init-type
			`((type ,init-type ,init))))
       (very-quickly
	 (mod-dotimes (,idx (dimensions ,ten))
	   :with (linear-sums
		  (,of (strides ,ten)))
	   :do (setf ,init (,@(if (symbolp func)
				  `(,func)
				  `(funcall ,funcsym)) ,init ,(recursive-append
							       (when key
								 (if (symbolp key)
								     `(,key)
								     `(funcall ,keysym)))
							       `(t/store-ref ,type ,sto ,of))))))
       ,init))))

;;
(defmacro-clause (FOR xa SLICING x ALONG axis &optional FROM start BELOW oend TO cend DOWNTO dend)
  (let* ((xa (if (listp xa) xa (list xa))))
    (when (or (and oend cend) (and dend (or cend oend))) (error "Use only one of BELOW TO DOWNTO."))
    (unless (null xa)
      (with-gensyms (dim ssym alist asym xlist xsym slst tmp-axis tmp-dim tmp-x)
	(let* ((n (length xa))
	       (sa (mapcar #'(lambda (x) (gensym (string+ "stride-" (symbol-name x)))) xa)))
	  `(progn
	     (with ,dim = -1)
	     ,@(mapcan #'(lambda (x s) `((with ,x = nil)
					 (with ,s = 0)))
		       xa sa)
	     (initially
	      (let* ((,alist ,(with-gensyms (axis-eval)
			       `(let ((,axis-eval ,axis))
				  (if (listp ,axis-eval)
				      (assert (= (length ,axis-eval) ,n) nil 'invalid-arguments)
				      (setq ,axis-eval (make-list ,n :initial-element ,axis-eval)))
				  ,axis-eval)))
		     (,xlist ,(with-gensyms (x-eval)
				`(let ((,x-eval ,x))
				   (if (listp ,x-eval)
				       (assert (= (length ,x-eval) ,n) nil 'invalid-arguments)
				       (setq ,x-eval (setq ,x-eval (make-list ,n :initial-element ,x-eval))))
				   (iter (for ,xsym in ,x-eval)
					 (for ,asym on ,alist)
					 (let* ((,tmp-axis (modproj (car,asym) (order ,xsym))))
					   (let ((,tmp-x ,(if (or start oend cend dend)
							      `(let ((,tmp-dim (dimensions ,xsym ,tmp-axis))
								     (,slst (make-list (order ,xsym) :initial-element '(nil nil))))
								 (declare (ignorable ,tmp-dim))
								 (setf (nth ,tmp-axis ,slst) ,(cond
											       (dend `(list* ,start (- (modproj ,dend ,tmp-dim) ,tmp-dim 1) -1))
											       (oend `(list ,start ,oend))
											       (cend `(list ,start (1+ (modproj ,cend ,tmp-dim))))
											       (t `(list ,start nil))))
								 (subtensor~ ,xsym ,slst))
							      xsym)))
					     (when (or (< ,dim 0) (> ,dim (dimensions ,tmp-x ,tmp-axis)))
					       (setf ,dim (dimensions ,tmp-x ,tmp-axis)))
					     (setf (car ,asym) ,tmp-axis)
					     (collect ,tmp-x))))))))
		(setf ,@(mapcan #'(lambda (x s) `((values ,x ,s) (let* ((,tmp-axis (car ,alist))
									(,tmp-x (car ,xlist))
									(,xsym (slice~ ,tmp-x ,tmp-axis))
									(,ssym (strides ,tmp-x ,tmp-axis)))
								   (decf (slot-value ,xsym 'head) ,ssym)
								   (values ,xsym ,ssym))
						  ,xlist (cdr ,xlist)
						  ,alist (cdr ,alist)))
				xa sa))))
	     (repeat ,dim)
	     (progn
	       ,@(mapcar #'(lambda (x s) `(incf (slot-value ,x 'head) ,s)) xa sa))))))))
