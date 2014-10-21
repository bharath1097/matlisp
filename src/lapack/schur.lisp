(in-package #:matlisp)

;; (permute!  '(jobvs sort select n a lda sdim w rwork vs ldvs work lwork bwork info) )
;;
(deft/generic (t/lapack-gees! #'subtypep) sym (A lda vs ldvs wr wi))
(deft/method t/lapack-gees! (sym blas-numeric-tensor) (A lda vs ldvs wr wi)
  (let* ((ftype (field-type sym)))
    (using-gensyms (decl (A lda vs ldvs wr wi) (lwork xxx))
      `(let (,@decl)
	 (declare (type ,sym ,A)
		  (type index-type ,lda)
		  (type ,(store-type sym) ,wr ,wi))
	 (with-lapack-query ,sym (,xxx ,lwork)
	   (ffuncall ,(blas-func "gees" ftype) ,@(apply #'append (permute! (pair `(
             (:& :character) (if ,vs #\V #\N) (:& :character) #\N :* (cffi:null-pointer)
	     (:& :integer) (dimensions ,A 0)
	     (:* ,(lisp->ffc ftype) :+ (head ,A)) (the ,(store-type sym) (store ,A)) (:& :integer) ,lda
	     (:& :integer) 0
	     (:* ,(lisp->ffc ftype)) (the ,(store-type sym) ,wr) (:* ,(lisp->ffc ftype)) (the ,(store-type sym) ,wi)
	     (:* ,(lisp->ffc ftype) :+ (if ,vs (head ,vs) 0)) (if ,vs (the ,(store-type sym) (store ,vs)) (cffi:null-pointer)) (:& :integer) (if ,vs ,ldvs 1)
	     (:* ,(lisp->ffc ftype)) (the ,(store-type sym) ,xxx) (:& :integer) ,lwork
	     :* (cffi:null-pointer) (:& :integer :output) 0))
									   ;;Flip rwork to the end in the case of {z,c}geev.
									   (make-instance 'permutation-cycle
											  :store (when (subtypep ftype 'cl:complex)
												   (list (pidxv 12 11 10 9 8))))))))))))
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
	       (wr (t/store-allocator ,(cl a) (dimensions a 0)) :type ,(store-type (cl a)))
	       (wi (t/store-allocator ,(cl a) (dimensions a 0)) :type ,(store-type (cl a))))
      (let ((info (t/lapack-gees! ,(cl a)
				  tret (or (blas-matrix-compatiblep tret #\N) 0)
				  vs (when vs (or (blas-matrix-compatiblep vs #\N) 0))
				  wr wi)))
	(unless (= info 0)
	  (if (< info 0)
	      (error "GEES: Illegal value in the ~:r argument." (- info))
	      (error "GEES: (~a) the QR algorithm failed to compute all the eigenvalues." info))))
      (values-list (list*
		    (make-instance ',(complexified-type (cl a))
				   :dimensions (make-index-store (list (nrows A)))
				   :strides (make-index-store (list 1))
				   :head 0
				   :store (t/geev-output-fix ,(cl a) wr wi))
		    tret
		    (when vs (list vs))))))


;; (letv* ((a (randn '(10 10)))
;; 	(s u q (schur a :v)))
;;   (norm #i(a - q * u * q')))
