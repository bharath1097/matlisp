(in-package #:matlisp)

;;
(deft/generic (t/lapack-geev! #'subtypep) sym (A lda vl ldvl vr ldvr wr wi))
(deft/method t/lapack-geev! (sym blas-numeric-tensor) (A lda vl ldvl vr ldvr wr wi)
  (let ((ftype (field-type sym)))
    (using-gensyms (decl (A lda vl ldvl vr ldvr wr wi) (lwork xxx))
      `(let (,@decl)
	 (declare (type ,sym ,A)
		  (type index-type ,lda)
		  (type ,(store-type sym) ,wr ,wi))
	 (with-lapack-query ,sym (,xxx ,lwork)
	   (ffuncall ,(blas-func "geev" ftype) ,@(apply #'append (permute! (pair `(
	     (:& :character) (if ,vl #\V #\N) (:& :character) (if ,vr #\V #\N)
	     (:& :integer) (dimensions ,A 0)
	     (:* ,(lisp->ffc ftype) :+ (head ,A)) (the ,(store-type sym) (store ,A)) (:& :integer) ,lda
	     (:* ,(lisp->ffc ftype)) (the ,(store-type sym) ,wr) (:* ,(lisp->ffc ftype)) (the ,(store-type sym) ,wi)
	     (:* ,(lisp->ffc ftype) :+ (if ,vl (head ,vl) 0)) (if ,vl (the ,(store-type sym) (store ,vl)) (cffi:null-pointer)) (:& :integer) (if ,vl ,ldvl 1)
	     (:* ,(lisp->ffc ftype) :+ (if ,vr (head ,vr) 0)) (if ,vr (the ,(store-type sym) (store ,vr)) (cffi:null-pointer)) (:& :integer) (if ,vr ,ldvr 1)
	     (:* ,(lisp->ffc ftype)) ,xxx (:& :integer) ,lwork
	     (:& :integer :output) 0))
									   ;;Flip rwork to the end in the case of {z,c}geev.
									   (make-instance 'permutation-cycle
											  :store (when (subtypep ftype 'cl:complex)
												   (list (pidxv 12 11 10 9 8 7 6))))))))))))
;;
(deft/generic (t/lapack-heev! #'subtypep) sym (jobz uplo A lda w))
(deft/method t/lapack-heev! (sym blas-numeric-tensor) (jobz uplo A lda w)
  (using-gensyms (decl (jobz A lda w uplo) (lwork xxx xxr))
    (let ((complex? (subtypep (field-type sym) 'complex))
	  (ftype (field-type sym)))
      `(let (,@decl)
       (declare (type ,sym ,A)
		(type character ,jobz ,uplo)
		(type index-type ,lda)
		(type ,(store-type (realified-type sym)) ,w))
       (with-field-elements ,(realified-type sym) (,@(when complex? `((,xxr (t/fid+ (t/field-type (t/realified-type ,sym))) (* 3 (dimensions ,A 0))))))
	 (with-lapack-query ,sym (,xxx ,lwork)
	   (ffuncall ,(blas-func (if complex? "heev" "syev") ftype)
	     (:& :character) ,jobz (:& :character) ,uplo
	     (:& :integer) (dimensions ,A 0)
	     (:* ,(lisp->ffc ftype) :+ (head ,A)) (the ,(store-type sym) (store ,A)) (:& :integer) ,lda
	     (:* ,(lisp->ffc ftype)) ,w
	     (:* ,(lisp->ffc ftype)) ,xxx (:& :integer) ,lwork
	     ,@(when complex? `((:* ,(lisp->ffc ftype)) ,xxr))
	     (:& :integer :output) 0)))))))

;;
(deft/generic (t/geev-output-fix #'subtypep) sym (wr wi))
(deft/method t/geev-output-fix (sym real-numeric-tensor) (wr wi)
  (let ((csym (complexified-type sym)))
    (using-gensyms (decl (wr wi) (ret i))
      `(let* (,@decl
	      (,ret (t/store-allocator ,csym (length ,wr))))
	 (declare (type ,(store-type sym) ,wr ,wi)
		  (type ,(store-type csym) ,ret))
	 (very-quickly
	   (loop :for ,i :from 0 :below (length ,wr)
	      :do (t/store-set ,csym (complex (aref ,wr ,i) (aref ,wi ,i)) ,ret ,i)))
	 ,ret))))

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
       ,@(loop :for jvl :in '(#\N #\V) :collect
	    `(,jvl
	      (ecase jobvr
		,@(loop :for jvr :in '(#\N #\V) :collect
		     `(,jvr
		       (with-columnification (() (A ,@(when (char= jvl #\V) `(vl)) ,@(when (char= jvr #\V) `(vr))))
			 (let ((info (t/lapack-geev! ,(cl a)
						     A (or (blas-matrix-compatiblep A #\N) 0)
						     ,@(if (char= jvl #\N) `(nil 1) `(vl (or (blas-matrix-compatiblep vl #\N) 0)))
						     ,@(if (char= jvr #\N) `(nil 1) `(vr (or (blas-matrix-compatiblep vr #\N) 0)))
						     wr wi)))
			   (unless (= info 0)
			     (if (< info 0)
				 (error "GEEV: Illegal value in the ~:r argument." (- info))
				 (error "GEEV: the QR algorithm failed to compute all the eigenvalues, and no eigenvectors have been computed;
elements ~a:~a of WR and WI contain eigenvalues which have converged." info n)))))))))))
     (let ((ret nil))
       (when vr (push vr ret))
       (when vl (push vl ret))
       (values-list (list* (with-no-init-checks
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
     (values-n (if (eq job :v) 2 1) evals A)))
;;
(defgeneric eig (matrix &optional job uplo)
  (:method :before ((matrix standard-tensor) &optional (job :nn) (uplo *default-uplo*))
     (declare (ignore job uplo))
     (assert (tensor-square-matrixp matrix) nil 'tensor-dimension-mismatch)))

(defmethod eig ((matrix complex-numeric-tensor) &optional (job :nn) (uplo *default-uplo*))
  (ecase job
    ((:nn :nv :vn :vv)
     (letv* ((n (nrows matrix))
	     ((levec? revec?) (mapcar #'(lambda (x) (char= x #\V)) (split-job job))))
       (geev! (copy matrix) (when levec? (zeros (list n n) (class-of matrix))) (when revec? (zeros (list n n) (class-of matrix))))))
    ((:n :v)
     (heev! (copy matrix) job uplo))))

(defun geev-fix-up-eigvec (n eigval eigvec)
  (let* ((evec (copy! eigvec (zeros (list n n) (complexified-type (class-of eigvec)))))
	 (tmp (zeros n (complexified-type (class-of eigvec))))
	 (cviewa (col-slice~ evec 0)) (cviewb (col-slice~ evec 0))
	 (cst (aref (strides evec) 1)))
    (iter (with i = 0) (with hd = (head cviewa))
	  (cond
	    ((>= i n) (return nil))
	    ((zerop (imagpart (ref eigval i))) (incf i))
	    (t (setf (slot-value cviewa 'head) (+ hd (* i cst))
		     (slot-value cviewb 'head) (+ hd (* (1+ i) cst)))
	       (copy! cviewb tmp) (copy! cviewa cviewb)
	       (axpy! #c(0 1) tmp cviewa) (axpy! #c(0 -1) tmp cviewb)
	       (incf i 2))))
    evec))

(defmethod eig ((matrix real-numeric-tensor) &optional (job :nn) (uplo *default-uplo*))
  (ecase job
    ((:nn :nv :vn :vv)
     (letv* ((n (nrows matrix))
	     ((levec? revec?) (mapcar #'(lambda (x) (char= x #\V)) (split-job job)))
	     (ret (multiple-value-list (geev! (copy matrix) (when levec? (zeros (list n n) (class-of matrix))) (when revec? (zeros (list n n) (class-of matrix)))))))
       (let ((eval (first ret)))
	 (unless (dotimes (i n t) (unless (zerop (imagpart (ref eval i))) (return nil)))
	   (print "he")
	   (setq ret (list* eval (mapcar #'(lambda (x) (geev-fix-up-eigvec n eval x)) (cdr ret))))))
       (values-list ret)))
    ((:n :v)
     (heev! (copy matrix) job uplo))))

;; (let ((a  #i(randn([3, 3]) + 1i * randn ([3, 3]))))
;;   ;;(octave-send-tensor a "a")
;;   ;;(octave-send "[v, l] = eig(a);~%")
;;   (letv* ((s vl vr (eig a :vv)))
;;     (values #i(a - (/vl)' * diag(s, 2) * vl')
;; 	    #i(a - vr * diag(s, 2) * /vr))
    
;;     ;;(geev! a nil (zeros (dims a) (class-of a)))
   
;;     #+nil(norm #i(vr * diag(s, 2) * /vr - a))
;;     #+nil(norm (t- a (t* v (diag s 2) (inv v))))
;;     #+nil(values (norm (t- (diag~ (octave-read-tensor "l")) s))
;; 	    (norm (t- (octave-read-tensor "v") v))
;; 	    )))
