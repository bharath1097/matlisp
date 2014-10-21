(in-package #:matlisp)

;;
(deft/generic (t/blas-trsm! #'subtypep) sym (side uplo transA diagA alpha A lda B ldb))
(deft/method t/blas-trsm! (sym blas-numeric-tensor) (side uplo transA diagA alpha A lda B ldb)
  (let ((ftype (field-type sym)))
    (using-gensyms (decl (side uplo transA diagA alpha A lda B ldb))
      `(let* (,@decl)
	 (declare (type ,sym ,A ,B)
		  (type ,(field-type sym) ,alpha)
		  (type index-type ,lda ,ldb)
		  (type character ,transa ,diaga ,uplo ,side))
	 (ffuncall ,(blas-func "trsm" ftype)
		   (:& :character) ,side (:& :character) ,uplo (:& :character) ,transa (:& :character) ,diagA
		   (:& :integer) (dimensions ,B 0) (:& :integer) (dimensions ,B 1)
		   (:& ,(lisp->ffc ftype t)) ,alpha
		   (:* ,(lisp->ffc ftype) :+ (head ,A)) (the ,(store-type sym) (store ,A)) (:& :integer) ,lda
		   (:* ,(lisp->ffc ftype) :+ (head ,B)) (the ,(store-type sym) (store ,B)) (:& :integer) ,ldb)
	 ,B))))

(deft/generic (t/blas-trsv! #'subtypep) sym (uplo transA diagA A lda b st-b))
(deft/method t/blas-trsv! (sym blas-numeric-tensor) (uplo transA diagA A lda b st-b)
  (let ((ftype (field-type sym)))
    (using-gensyms (decl (uplo transA diagA A lda b st-b))
      `(let* (,@decl)
	 (declare (type ,sym ,A ,b)
		  (type index-type ,lda ,st-b)
		  (type character ,transa ,diaga ,uplo))
	 (ffuncall ,(blas-func "trsv" ftype)
	   (:& :character) ,uplo (:& :character) ,transa (:& :character) ,diagA
	   (:& :integer) (dimensions ,A 0)
	   (:* ,(lisp->ffc ftype) :+ (head ,A)) (the ,(store-type sym) (store ,A)) (:& :integer) ,lda
	   (:* ,(lisp->ffc ftype) :+ (head ,B)) (the ,(store-type sym) (store ,B)) (:& :integer) ,st-b)
	 ,b))))
;;

#+nil(defgeneric trsm! (A b &optional uplo diag side trans))
