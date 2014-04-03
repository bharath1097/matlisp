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
(deft/generic (t/lapack-geev! #'subtypep) sym (A lda vl ldvl vr ldvr wr wi))
(deft/method t/lapack-geev! (sym blas-numeric-tensor) (A lda vl ldvl vr ldvr wr wi)
  (using-gensyms (decl (A lda vl ldvl vr ldvr wr wi) (lwork xxx))
    `(let (,@decl
	   (,lwork -1))
       (declare (type ,sym ,A)
		(type index-type ,lda ,lwork)
		(type ,(store-type sym) ,wr ,wi))
       (let-typed ((,xxx (t/store-allocator ,sym 1) :type ,(store-type sym)))
	 (,(macroexpand-1 `(t/lapack-geev-func ,sym))
	   (if ,vl #\V #\N) (if ,vr #\V #\N)
	   (nrows ,A)
	   ,xxx ,lda
	   ,xxx ,xxx
	   ,xxx (if ,vl ,ldvl 1)
	   ,xxx (if ,vr ,ldvr 1)
	   ,xxx -1
	   0)
	 (setq ,lwork (ceiling (t/frealpart ,(field-type sym) (t/store-ref ,sym ,xxx 0)))))
       (,(macroexpand-1 `(t/lapack-geev-func ,sym))
	 (if ,vl #\V #\N) (if ,vr #\V #\N)
	 (nrows ,A)
	 (the ,(store-type sym) (store ,A)) ,lda
	 ,wr ,wi
	 (if ,vl (the ,(store-type sym) (store ,vl)) (cffi:null-pointer)) (if ,vl ,ldvl 1)
	 (if ,vr (the ,(store-type sym) (store ,vr)) (cffi:null-pointer)) (if ,vr ,ldvr 1)
	 (t/store-allocator ,sym ,lwork) ,lwork
	 0
	 (the index-type (head ,A)) (if ,vl (the index-type (head ,vl)) 0) (if ,vr (the index-type (head ,vr)) 0)))))
;;
(deft/generic (t/lapack-heev-func #'subfieldp) sym ())
(deft/method t/lapack-heev-func (sym real-tensor) ()
  'matlisp-lapack:dsyev)
(deft/method t/lapack-heev-func (sym complex-tensor) ()
  'matlisp-lapack:zheev)

(deft/generic (t/lapack-heev! #'subtypep) sym (jobz uplo A lda w))
;;This will not work if you choose (simple-array (complex double-float) (*)) for complex-tensor.
(deft/method t/lapack-heev! (sym blas-numeric-tensor) (jobz uplo A lda w)
  (using-gensyms (decl (jobz A lda w uplo) (n lwork xxx))
    (let ((complex? (subtypep (field-type sym) 'complex)))
      `(let (,@decl
	     (,lwork -1))
       (declare (type ,sym ,A)
		(type character ,jobz ,uplo)
		(type index-type ,lda ,lwork)
		(type ,(store-type (realified-type sym)) ,w))
       (let-typed ((,n (nrows ,A) :type index-type))
	 (let-typed ((,xxx (t/store-allocator ,sym 1) :type ,(store-type sym)))
	   (,(macroexpand-1 `(t/lapack-heev-func ,sym))
	     ,jobz ,uplo
	     ,n
	     ,xxx ,lda
	     ,xxx
	     ,xxx -1
	     ,@(when complex? `(,xxx))
	     0)
	   (setq ,lwork (ceiling (t/frealpart ,(field-type sym) (t/store-ref ,sym ,xxx 0)))))
	 (,(macroexpand-1 `(t/lapack-heev-func ,sym))
	   ,jobz ,uplo
	   ,n
	   (the ,(store-type sym) (store ,A)) ,lda
	   ,w
	   (t/store-allocator ,sym ,lwork) ,lwork
	   ,@(when complex? `((t/store-allocator (t/realified-type ,sym) (* 3 ,n))))
	   0
	   (the index-type (head ,A))))))))

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
(defgeneric geev! (a &optional vl vr)
  (:documentation "
 Syntax
 ======
 (GEEV! a &optional vl vr)

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
 ")
  (:method :before ((a standard-tensor) &optional vl vr)
	   (assert (tensor-square-matrixp a) nil 'tensor-dimension-mismatch)
	   (when vl
	     (assert (and (tensor-square-matrixp vl) (= (nrows vl) (nrows a)) (typep vl (type-of a)))  nil 'tensor-dimension-mismatch))
	   (when vr
	     (assert (and (tensor-square-matrixp vr) (= (nrows vr) (nrows a)) (typep vr (type-of a)))  nil 'tensor-dimension-mismatch))))

(define-tensor-method geev! ((a blas-numeric-tensor :output) &optional vl vr)
  `(let* ((jobvl (if vl #\V #\N))
	  (jobvr (if vr #\V #\N))
	  (n (nrows A))
	  (wr (t/store-allocator ,(cl a) n))
	  (wi (t/store-allocator ,(cl a) n)))
     (ecase jobvl       
       ,@(loop :for jvl :in '(#\N #\V)
	    :collect `(,jvl
		       (ecase jobvr
			 ,@(loop :for jvr :in '(#\N #\V)
			      :collect `(,jvr
					 (with-columnification (() (A ,@(when (char= jvl #\V) `(vl)) ,@(when (char= jvr #\V) `(vr))))
					   (multiple-value-bind (osto owr owi ovl ovr owork info)
					       (t/lapack-geev! ,(cl a)
							       A (or (blas-matrix-compatiblep A #\N) 0)
							       ,@(if (char= jvl #\N) `(nil 1) `(vl (or (blas-matrix-compatiblep vl #\N) 0)))
							       ,@(if (char= jvr #\N) `(nil 1) `(vr (or (blas-matrix-compatiblep vr #\N) 0)))
							       wr wi)
					     (declare (ignore osto owr owi ovl ovr owork))
					     (unless (= info 0)
					       (if (< info 0)
						   (error "GEEV: Illegal value in the ~:r argument." (- info))
						   (error "GEEV: the QR algorithm failed to compute all the eigenvalues, and no eigenvectors have been computed;
elements ~a:~a of WR and WI contain eigenvalues which have converged." info n)))))))))))
     (let ((ret nil))
       (and vr (push vr ret))
       (and (or vr vl) (push vl ret))
       (values-list (cons (with-no-init-checks
			      (make-instance ',(complexified-type (cl a))
					     :dimensions (make-index-store (list (nrows A)))
					     :strides (make-index-store (list 1))
					     :head 0
					     :store (t/geev-output-fix ,(cl a) wr wi)))
			  ret)))))
;;
(defgeneric heev! (a &optional job uplo?)
  (:documentation "
 Syntax
 ======
 (HEEV! a &optional evec? )

 Purpose:
 ========
 Computes the eigenvalues / eigenvectors of a Hermitian (symmetric) A.
 ")
  (:method :before ((a standard-tensor) &optional (job :n) (uplo? *default-uplo*))
     (assert (tensor-square-matrixp a) nil 'tensor-dimension-mismatch)
     (assert (and (member job '(:v :n)) (member uplo? '(:u :l))) nil 'invalid-arguments)))

(define-tensor-method heev! ((a blas-numeric-tensor :output) &optional (job :n) (uplo? *default-uplo*))
  `(let ((evals (zeros (nrows a) ',(realified-type (cl a)))))
     (with-columnification (() (A))
       (let ((info (t/lapack-heev! ,(cl a)
				   (aref (symbol-name job) 0)
				   (aref (symbol-name uplo?) 0)
				   A (or (blas-matrix-compatiblep A #\N) 0)
				   (store evals))))
	 (unless (= info 0)
	   (if (< info 0)
	       (error "(SY/HE)EV: Illegal value in the ~:r argument." (- info))
	       (error "(SY/HE)EV: the algorithm failed to converge; ~a off-diagonal elements of an intermediate tridiagonal form did not converge to zero." info)))))
     (if (eql job :v)
	 (values evals A)
	 (values evals))))
;;
(defgeneric eig (matrix &optional job uplo)
  (:method :before ((matrix standard-tensor) &optional (job :nn) (uplo *default-uplo*))
     (declare (ignore job uplo))
     (assert (tensor-square-matrixp matrix) nil 'tensor-dimension-mismatch)))

(defmethod eig ((matrix complex-numeric-tensor) &optional (job :nn) (uplo *default-uplo*))
  (ecase job
    ((:nn :nv :vn :vv)
     (mlet* ((n (nrows matrix))
	     ((levec? revec?) (values-list (mapcar #'(lambda (x) (char= x #\V)) (split-job job)))))
	    (geev! (copy matrix) (when levec? (zeros (list n n) (class-of matrix))) (when revec? (zeros (list n n) (class-of matrix))))))
    ((:n :v)
     (heev! (copy matrix) job uplo))))

(defun geev-fix-up-eigvec (n eigval eigvec)
  (let* ((evec (copy! eigvec (zeros (list n n) (complexified-type (class-of eigvec)))))
	 (tmp (zeros n (complexified-type (class-of eigvec))))
	 (cviewa (col-slice~ evec 0)) (cviewb (col-slice~ evec 0))
	 (hd (head cviewa))
	 (cst (aref (strides evec) 1)))
    (loop :with i := 0
       :do (if (< i n)
	       (if (zerop (imagpart (ref eigval i)))
		   (incf i)
		   (progn
		     (setf (slot-value cviewa 'head) (+ hd (* i cst))
			   (slot-value cviewb 'head) (+ cst (slot-value cviewa 'head)))
		     (copy! cviewb tmp)
		     (copy! cviewa cviewb)
		     (axpy! #c(0 1) tmp cviewa)
		     (axpy! #c(0 -1) tmp cviewb)
		     (incf i 2)))
	       (return nil)))
    evec))

(defmethod eig ((matrix real-numeric-tensor) &optional (job :nn) (uplo *default-uplo*))
  (ecase job
    ((:nn :nv :vn :vv)    
     (mlet* ((n (nrows matrix))
	     ((levec? revec?) (values-list (mapcar #'(lambda (x) (char= x #\V)) (split-job job))))
	     (ret (multiple-value-list (geev! (copy matrix) (when levec? (zeros (list n n) (class-of matrix))) (when revec? (zeros (list n n) (class-of matrix)))))))
	    (let ((eval (car ret)))
	      (unless (dotimes (i n t) (unless (zerop (imagpart (ref eval i))) (return nil)))
		(when levec? (setf (second ret) (geev-fix-up-eigvec n eval (second ret))))
		(when revec? (setf (third ret) (geev-fix-up-eigvec n eval (third ret))))))
	    (values-list ret)))
    ((:n :v)
     (heev! (copy matrix) job uplo))))
