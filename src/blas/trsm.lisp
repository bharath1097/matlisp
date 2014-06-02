(in-package #:matlisp)

(deft/generic (t/blas-trsm-func #'subfieldp) sym ())
(deft/method t/blas-trsm-func (sym real-tensor) () 'matlisp-blas:dtrsm)
(deft/method t/blas-trsm-func (sym complex-tensor) () 'matlisp-blas:ztrsm)

(deft/generic (t/blas-trsv-func #'subfieldp) sym ())
(deft/method t/blas-trsv-func (sym real-tensor) () 'matlisp-blas:dtrsv)
(deft/method t/blas-trsv-func (sym complex-tensor) () 'matlisp-blas:ztrsv)
;;
(deft/generic (t/blas-trsm! #'subtypep) sym (side uplo transA diagA alpha A lda B ldb))
(deft/method t/blas-trsm! (sym blas-numeric-tensor) (side uplo transA diagA alpha A lda B ldb)
  (using-gensyms (decl (side uplo transA diagA alpha A lda B ldb))
    `(let* (,@decl)
       (declare (type ,sym ,A ,B)
		(type ,(field-type sym) ,alpha)
		(type index-type ,lda ,ldb)
		(type character ,transa ,diaga ,uplo ,side))
       (,(macroexpand-1 `(t/blas-trsm-func ,sym))
	 ,side ,uplo ,transa ,diagA
	 (nrows ,B) (ncols ,B)
	 ,alpha
	 (the ,(store-type sym) (store ,A)) ,lda
	 (the ,(store-type sym) (store ,B)) ,ldb
	 (the index-type (head ,A)) (the index-type (head ,B))))))

(deft/generic (t/blas-trsv! #'subtypep) sym (uplo transA diagA A lda b st-b))
(deft/method t/blas-trsv! (sym blas-numeric-tensor) (uplo transA diagA A lda b st-b)
  (using-gensyms (decl (uplo transA diagA A lda b st-b))
    `(let* (,@decl)
       (declare (type ,sym ,A ,b)
		(type index-type ,lda ,st-b)
		(type character ,transa ,diaga ,uplo))
       (,(macroexpand-1 `(t/blas-trsv-func ,sym))
	 ,uplo ,transa ,diagA
	 (nrows ,A)
	 (the ,(store-type sym) (store ,A)) ,lda
	 (the ,(store-type sym) (store ,B)) ,st-b
	 (the index-type (head ,A)) (the index-type (head ,B))))))
;;

(defgeneric trsm! (A b &optional uplo diag side trans))
