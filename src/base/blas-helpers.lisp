(in-package #:matlisp)
   
(defun consecutive-storep (tensor)
  (declare (type standard-tensor tensor))
  (memoizing (tensor consecutive-storep)
    (letv* ((sort-std std-perm (very-quickly (sort-permute-base (copy-seq (the index-store-vector (strides tensor))) #'<)) :type index-store-vector pindex-store-vector)
	    (perm-dims (very-quickly (apply-action! (copy-seq (the index-store-vector (dimensions tensor))) std-perm)) :type index-store-vector))
      (very-quickly
	(loop
	   :for so-st :across sort-std
	   :for so-di :across perm-dims
	   :and accumulated-off := (aref sort-std 0) :then (the index-type (* accumulated-off so-di))
	   :unless (= so-st accumulated-off) :do (return (values nil perm-dims sort-std std-perm))
	   :finally (return (values (aref sort-std 0) perm-dims sort-std std-perm)))))))

(definline blas-func (name type)
  (string+
   (cond
     ((eq type 'single-float) "s")
     ((eq type 'double-float) "d")
     ((tree-equal type '(complex single-float)) "c")
     ((tree-equal type '(complex double-float)) "z")
     (t "error: unknown BLAS type."))
   name))

(definline blas-copyablep (ten-a ten-b)
  (declare (type standard-tensor ten-a ten-b))
  (when (= (order ten-a) (order ten-b))
    (letv* ((csto-a? pdims-a tmp perm-a (consecutive-storep ten-a) :type t index-store-vector nil pindex-store-vector)
	    (csto-b? pdims-b tmp perm-b (consecutive-storep ten-b) :type t index-store-vector nil pindex-store-vector))
     (when (and csto-a? csto-b? (very-quickly (lvec-eq perm-a perm-b)) (very-quickly (lvec-eq pdims-a pdims-b)))
       (list csto-a? csto-b?)))))

(definline fortran-nop (op)
  (ecase op (#\T #\N) (#\N #\T)))

(definline fortran-nuplo (op)
  (ecase op (#\U #\L) (#\L #\U)))

(definline split-job (job)
  (declare (type symbol job))
  (let-typed ((name (symbol-name job) :type string))
    (loop :for x :across name :collect (char-upcase x))))

(definline flip-major (job)
  (declare (type symbol job))
  (case job
    (:row-major :col-major)
    (:col-major :row-major)))

(definline blas-matrix-compatiblep (matrix op)
  (declare (type standard-tensor matrix)
	   (type character op))
  (assert (tensor-matrixp matrix) nil 'tensor-not-matrix)
  (let*-typed ((stds (strides matrix) :type index-store-vector)
	       (rs (aref stds 0) :type index-type)
	       (cs (aref stds 1) :type index-type))
    ;;Note that it is not required that (rs = nc * cs) or (cs = nr * rs)
    (cond
      ;;The ordering of these conditions is important to meet certain assumed conditions
      ;;in GEMM, when MATRIX has strides of the form #(1 1).
      ((and (= rs 1) (> cs 0)) (values cs op :col-major))
      ((and (char/= op #\C) (= cs 1) (> rs 0)) (values rs (fortran-nop op) :row-major)))))

(definline call-fortran? ( x lb)
  (declare (type standard-tensor x))
  (> (size x) lb))

(defmacro with-rowm (&rest body)
  `(let ((*default-stride-ordering* :row-major))
     ,@body))

(defmacro with-colm (&rest body)
  `(let ((*default-stride-ordering* :col-major))
     ,@body))

(defmacro with-columnification (((&rest input) (&rest output)) &rest body)
  (let ((input-syms (mapcar #'(lambda (x) (gensym (symbol-name (car x)))) input))
	(output-syms (mapcar #'(lambda (mat) (gensym (symbol-name mat))) output)))
    (with-gensyms (stack)
      `(let ((,stack nil))
	 (let (,@(mapcar #'(lambda (x sym) (destructuring-bind (mat job) x
					     `(,sym (if (blas-matrix-compatiblep ,mat ,job) ,mat (with-colm (copy ,mat))))))
			 input input-syms)
	       ,@(mapcar #'(lambda (mat sym) `(,sym (if (eql (nth-value 2 (blas-matrix-compatiblep ,mat #\N)) :col-major) (progn (push nil ,stack) ,mat)
							(with-colm (push t ,stack) (copy ,mat))))) output output-syms))
	 (symbol-macrolet (,@(mapcar #'(lambda (mat sym) `(,mat ,sym)) (append (mapcar #'car input) output) (append input-syms output-syms)))
	   ,@body)
	 ,@(mapcar #'(lambda (mat sym) `(when (pop ,stack) (copy! ,sym ,mat))) (reverse output) (reverse output-syms))
	 nil)))))

(definline pflip.f->l (uidiv)
  (declare (type (simple-array (signed-byte 32) (*)) uidiv))
  (let ((ret (make-array (length uidiv) :element-type 'pindex-type)))
    (declare (type pindex-store-vector ret))
    (very-quickly
      (loop :for i :from 0 :below (length uidiv)
	 :do (setf (aref ret i) (1- (aref uidiv i)))))
    ret))

(definline pflip.l->f (idiv)
  (declare (type pindex-store-vector idiv))
  (let ((ret (make-array (length idiv) :element-type '(signed-byte 32))))
    (declare (type (simple-array (signed-byte 32) (*)) ret))
    (very-quickly
      (loop :for i :from 0 :below (length idiv)
	 :do (setf (aref ret i) (1+ (aref idiv i)))))
    ret))

(defmacro with-lapack-query (class (work lwork) &rest code)
  `(let-typed ((,lwork -1 :type index-type))
     (with-field-element ,class (,work (t/fid+ ,(field-type class)) 1)
       (progn ,@code)
       (setq ,lwork (ceiling (t/frealpart ,(field-type class) (t/store-ref ,class ,work 0)))))
     (with-field-element ,class (,work (t/fid+ ,(field-type class)) ,lwork)
       ,@code)))
