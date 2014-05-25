(in-package #:matlisp)

(deft/generic (t/lapack-gees-func #'subfieldp) sym ())
(deft/method t/lapack-gees-func (sym real-tensor) ()
  'matlisp-lapack:dgees)
;;Make API for real and complex versions similar.
(definline mzgees (jobvs sort select n a lda sdim w rwork vs ldvs work lwork bwork info &optional (head-a 0) (head-vs 0))
  (matlisp-lapack:zgees jobvs sort select n a lda sdim w vs ldvs work lwork rwork bwork info head-a head-vs ))
(deft/method t/lapack-gees-func (sym complex-tensor) ()
  'mzgees)
;;
(deft/generic (t/lapack-gees! #'subtypep) sym (A lda vs ldvs wr wi))
(deft/method t/lapack-gees! (sym blas-numeric-tensor) (A lda vs ldvs wr wi)
  (using-gensyms (decl (A lda vs ldvs wr wi) (lwork xxx))
    `(let (,@decl
	   (,lwork -1))
       (declare (type ,sym ,A)
		(type index-type ,lda ,lwork)
		;;This assumes that the (store-type .) and (store-type (realified-type .)) are the same
		;;(so does t/lapack-geev!).		
		(type ,(store-type sym) ,wr ,wi))
		(let-typed ((,xxx (t/store-allocator ,sym 1) :type ,(store-type sym)))
			   (,(macroexpand-1 `(t/lapack-gees-func ,sym))
			     (if ,vs #\V #\N)
			     #\N (cffi:null-pointer)
			     (nrows ,A)
			     ,xxx ,lda
			     0
			     ,xxx ,xxx
			     ,xxx (if ,vs ,ldvs 1)
			     ,xxx -1
			     (cffi:null-pointer)  0)
			   (setq ,lwork (ceiling (t/frealpart ,(field-type sym) (t/store-ref ,sym ,xxx 0)))))
		(,(macroexpand-1 `(t/lapack-gees-func ,sym))
		 (if ,vs #\V #\N)
		 #\N (cffi:null-pointer)
		 (nrows ,A)
		 (the ,(store-type sym) (store ,a)) ,lda
		 0
		 ,wr ,wi
		 (if ,vs (the ,(store-type sym) (store ,vs)) (cffi:null-pointer)) (if ,vs ,ldvs 1)
		 (t/store-allocator ,sym ,lwork) ,lwork
		 (cffi:null-pointer) 0
		 (the index-type (head ,a)) (if ,vs (the index-type (head ,vs)) 0)))))
;;
(defgeneric schur (A &optional job)
  (:documentation "
    Syntax
    ------
    (schur A &optional JOB)
    The function computes the schur decomposition of the square matrix A:
    A = V * T * (V**H) .
    The matrix V is {orthogonal/unitary}, the matrix T is {quasi-upper triangular/pper triangular}
    for {real/complex} matrices respectively.")
  (:method :before (A &optional (job :v))
	   (assert (and (tensor-square-matrixp A) (member job '(:v :n))) nil 'invalid-arguments :message "argument is not a square matrix.")))

(define-tensor-method schur ((A blas-numeric-tensor :input) &optional (job :v))
  `(let-typed ((tret (with-colm (copy A)))
	       (vs (when (eq job :v) (with-colm (zeros (dims A) ',(cl a)))))
	       (wr (t/store-allocator ,(cl a) (nrows a)) :type ,(store-type (cl a)))
	       (wi (t/store-allocator ,(cl a) (nrows a)) :type ,(store-type (cl a))))
      (let ((info (t/lapack-gees! ,(cl a)
				  tret (or (blas-matrix-compatiblep tret #\N) 0)
				  vs (when vs (or (blas-matrix-compatiblep vs #\N) 0))
				  wr wi)))
	(unless (= info 0)
	  (if (< info 0)
	      (error "GEES: Illegal value in the ~:r argument." (- info))
	      (error "GEES: (~a) the QR algorithm failed to compute all the eigenvalues." info))))
      (let ((ret nil))      
	(when vs (push vs ret))
	(push tret ret)
	(values-list (cons
		      (make-instance ',(complexified-type (cl a))
				     :dimensions (make-index-store (list (nrows A)))
				     :strides (make-index-store (list 1))
				     :head 0
				     :store (t/geev-output-fix ,(cl a) wr wi))
		      ret)))))
