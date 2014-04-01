(in-package #:matlisp)

(deft/generic (t/lapack-geev-func #'subfieldp) sym ())

(deft/method t/lapack-geev-func (sym real-tensor) ()
  'matlisp-lapack:dgeev)
;;Make API for real and complex versions similar.
(definline mzgeev (jobvl jobvr n a lda w rwork vl ldvl vr ldvr work lwork info &optional (head-a 0) (head-vl 0) (head-vr 0))
  (matlisp-lapack:zgeev jobvl jobvr n a lda w vl ldvl vr ldvr work lwork rwork info head-a head-vl head-vr))
(deft/method t/lapack-geev-func (sym complex-tensor) ()
  'mzgeev)
;;
(deft/generic (t/geev-output-fix #'subtypep) sym (wr wi))
(deft/method t/geev-output-fix (sym real-numeric-tensor) (wr wi)
  (let ((csym (complexified-type sym)))
    (using-gensyms (decl (wr wi))
      (with-gensyms (ret i)
	`(let* (,@decl
		(,ret (t/store-allocator ,csym (length ,wr))))
	   (declare (type ,(store-type sym) ,wr ,wi)
		    (type ,(store-type csym) ,ret))
	   (very-quickly
	     (loop :for ,i :from 0 :below (length ,wr)
		:do (t/store-set ,csym (complex (aref ,wr ,i) (aref ,wi ,i)) ,ret ,i)))
	   ,ret)))))

(deft/method t/geev-output-fix (sym complex-numeric-tensor) (wr wi)
  (using-gensyms (decl (wr))
    `(let (,@decl)
       (declare (type ,(store-type sym) ,wr))
       ,wr)))
;;
(deft/generic (t/lapack-geev! #'subtypep) sym (A lda vl ldvl vr ldvr wr wi work))

(deft/method t/lapack-geev! (sym blas-numeric-tensor) (A lda vl ldvl vr ldvr wr wi work)
  (using-gensyms (decl (A lda vl ldvl vr ldvr wr wi work))
    `(let (,@decl)
       (declare (type ,sym ,A)
		(type index-type ,lda)
		(type ,(store-type sym) ,wr ,wi ,work))
       (,(macroexpand-1 `(t/lapack-geev-func ,sym))
	 (if ,vl #\V #\N) (if ,vr #\V #\N)
	 (nrows ,A)
	 (the ,(store-type sym) (store ,A)) ,lda
	 ,wr ,wi
	 (if ,vl (the ,(store-type sym) (store ,vl)) (cffi:null-pointer)) (if ,vl ,ldvl 1)
	 (if ,vr (the ,(store-type sym) (store ,vr)) (cffi:null-pointer)) (if ,vr ,ldvr 1)
	 ,work (t/store-size ,sym ,work)
	 0
	 (the index-type (head ,A)) (if ,vl (the index-type (head ,vl)) 0) (if ,vr (the index-type (head ,vr)) 0)))))
;;

(deft/generic (t/lapack-geev-workspace-inquiry #'subtypep) sym (n jobvl jobvr))

#+nil
(deft/method t/lapack-geev-workspace-inquiry (sym blas-numeric-tensor) (n jobvl jobvr)
  (with-gensyms (n-sym)
    `(let ((,n-sym ,n))
       (* 10 ,n-sym))))

(deft/method t/lapack-geev-workspace-inquiry (sym blas-numeric-tensor) (n jobvl jobvr)
  (using-gensyms (decl (n jobvl jobvr))
    (with-gensyms (xxx)
     `(let (,@decl
	    (,xxx (t/store-allocator ,sym 1)))
	(declare (type index-type ,n)
		 (type character ,jobvl ,jobvr)
		 (type ,(store-type sym) ,xxx))
	(,(macroexpand-1 `(t/lapack-geev-func ,sym))
	  ,jobvl ,jobvr
	  ,n
	  ,xxx ,n
	  ,xxx ,xxx
	  ,xxx ,n
	  ,xxx ,n
	  ,xxx -1
	  0)
	(ceiling (t/frealpart ,(field-type sym) (t/store-ref ,sym ,xxx 0)))))))

#+nil
(t/lapack-geev-workspace-inquiry complex-tensor 2 #\V #\V)

;;
#+nil
(progn
(let ((*default-tensor-type* 'complex-tensor))
  (let ((a (copy! #2a((1 2) (3 4)) (zeros '(2 2)))))
    (t/lapack-geev! complex-tensor a 2 (zeros '(2 2)) 2 (zeros '(2 2)) 2 (t/store-allocator complex-tensor 2) (t/store-allocator complex-tensor 2) (t/store-allocator complex-tensor 100))))

  
(let ((a (copy! #2a((1 2) (3 4)) (zeros '(2 2)))))
  (t/lapack-geev! real-tensor a 2 nil 1 nil 1 (t/store-allocator real-tensor 2) (t/store-allocator real-tensor 2) (t/store-allocator real-tensor 100)))
)

(defgeneric geev! (a &optional vl vr)
  (:documentation "
 Syntax
 ======
 (GEEV a [job])

 Purpose:
 ========
 Computes the eigenvalues and left/right eigenvectors of A.

 For an NxN matrix A, its eigenvalues are denoted by:

              lambda(i),   j = 1 ,..., N
 
 The right eigenvectors of A are denoted by v(i) where:

                    A * v(i) = lambda(i) * v(i)

 The left eigenvectors of A are denoted by u(i) where:

                     H                      H
                 u(i) * A = lambda(i) * u(i)

 In matrix notation:
                             -1
                    A = V E V

           and
                          -1
                         H       H
                    A = U    E  U

 where lambda(i) is the ith diagonal of the diagonal matrix E,
 v(i) is the ith column of V and u(i) is the ith column of U.
 
 The computed eigenvectors are normalized to have Euclidean norm
 equal to 1 and largest component real.
 
 Return Values:
 ==============

     JOB            Return Values
 ------------------------------------------------------------------
     :NN (default)  [1] (DIAG E)       An Nx1 vector of eigenvalues
                    [2] INFO       

     :VN or T       [1] V          
                    [2] E                                             
                    [3] INFO       

     :NV            [1] E          
                    [2] U          
                    [3] INFO       

     :VV            [1] V          
                    [2] E
                    [3] U          
                    [3] INFO       
      
  where INFO is T if successful, NIL otherwise.
")
  (:method :before ((a standard-tensor) &optional vl vr)
	   (assert (tensor-squarep a) nil 'tensor-dimension-mismatch)
	   (when vl
	     (assert (and (tensor-squarep vl) (= (nrows vl) (nrows a)) (typep vl (type-of a)))  nil 'tensor-dimension-mismatch))
	   (when vr
	     (assert (and (tensor-squarep vr) (= (nrows vr) (nrows a)) (typep vr (type-of a)))  nil 'tensor-dimension-mismatch))))

(defmethod geev! ((a blas-numeric-tensor) &optional vl vr)
  (let ((cla (class-name (class-of A))))
    (assert (member cla *tensor-type-leaves*)
	    nil 'tensor-abstract-class :tensor-class (list cla))
        (compile-and-eval
	 `(defmethod geev! ((A ,cla) &optional vl vr)
	    (let* ((n (nrows A))
		   (jobvl (if vl #\V #\N))
		   (jobvr (if vr #\V #\N))
		   (work (t/store-allocator ,cla (t/lapack-geev-workspace-inquiry ,cla n jobvl jobvr)))
		   (wr (t/store-allocator ,cla n))
		   (wi (t/store-allocator ,cla n)))
	      (ecase jobvl
		,@(loop :for jvl :in '(#\N #\V)
		     :collect `(,jvl
				(ecase jobvr
				  ,@(loop :for jvr :in '(#\N #\V)
				       :collect `(,jvr
						  (with-columnification (() (A ,@(when (char= jvl #\V) `(vl)) ,@(when (char= jvr #\V) `(vr))))
						    (multiple-value-bind (osto owr owi ovl ovr owork info)
							(t/lapack-geev! ,cla
									A (or (blas-matrix-compatiblep A #\N) 0)
									,@(if (char= jvl #\N) `(nil 1) `(vl (or (blas-matrix-compatiblep vl #\N) 0)))
									,@(if (char= jvr #\N) `(nil 1) `(vr (or (blas-matrix-compatiblep vr #\N) 0)))
									wr wi work)
						      (declare (ignore osto owr owi ovl ovr owork))
						      (unless (= info 0)
							(error "geev returned ~a~%" info))))))))))
	      (values-list (remove-if #'null
				      (list
				       (let ((*check-after-initializing?* nil))
					 (make-instance ',(complexified-type cla)
							:dimensions (make-index-store (list (nrows A)))
							:strides (make-index-store (list 1))
							:head 0
							:store (t/geev-output-fix ,cla wr wi)))
				       vl vr)))))))
  (geev! A vl vr))
;;
(defgeneric eig (matrix &optional job)
  (:method :before ((matrix standard-tensor) &optional (job :nn))
	   (assert (tensor-matrixp matrix) nil 'tensor-dimension-mismatch)
	   (assert (member job '(:nn :nv :vn :vv)) nil 'invalid-arguments)))

(defmethod eig ((matrix complex-numeric-tensor) &optional (job :nn))
  (mlet* ((n (nrows matrix))
	  ((levec? revec?) (values-list (mapcar #'(lambda (x) (char= x #\V)) (split-job job))))
	  (vl (when levec? (zeros (list n n) (class-of matrix))))
	  (vr (when revec? (zeros (list n n) (class-of matrix)))))
    (geev! (copy matrix) vl vr)))

(defun geev-fix-up-eigvec (n eigval eigvec)
  (let* ((evec (copy! eigvec (zeros (list n n) (complexified-type (class-of eigvec)))))
	 (tmp (zeros n (complexified-type (class-of eigvec))))
	 (cviewa (col-slice~ evec 0))
	 (cviewb (col-slice~ evec 0))
	 (cst (aref (strides evec) 1)))
    (loop
       :with i := 0
       :do (if (< i n)
	       (if (zerop (imagpart (ref eigval i)))
		   (incf i)
		   (progn
		     (setf (slot-value cviewa 'head) (* i cst)
			   (slot-value cviewb 'head) (* (1+ i) cst))
		     (copy! cviewb tmp)
		     (copy! cviewa cviewb)
		     (axpy! #c(0 1) tmp cviewa)
		     (axpy! #c(0 -1) tmp cviewb)
		     (incf i 2)))
	       (return nil)))
    evec))

(defmethod eig ((matrix real-numeric-tensor) &optional (job :nn))
  (mlet* ((n (nrows matrix))
	  ((levec? revec?) (values-list (mapcar #'(lambda (x) (char= x #\V)) (split-job job))))
	  (ret (multiple-value-list
		(geev! (copy matrix)
		       (when levec? (zeros (list n n) (class-of matrix)))
		       (when revec? (zeros (list n n) (class-of matrix))))))
	  (eig (car ret)))
	 (if (loop :for i :from 0 :below n
		:do (unless (zerop (imagpart (ref eig i)))
		      (return nil))
		:finally (return t))
	     (values-list ret)
	     (values-list (cons eig (mapcar #'(lambda (mat) (geev-fix-up-eigvec n eig mat)) (cdr ret)))))))
