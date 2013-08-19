(in-package #:matlisp)

(deft/generic (t/lapack-geev-func #'subtypep) sym ())

(deft/method t/lapack-geev-func (sym real-tensor) ()
  'dgeev)
;;Make API for real and complex versions similar.
(definline mzgeev (jobvl jobvr n a lda w rwork vl ldvl vr ldvr work lwork info &optional (head-a 0) (head-vl 0) (head-vr 0))
  (zgeev jobvl jobvr n a lda w vl ldvl vr ldvr work lwork rwork info head-a head-vl head-vr))
(deft/method t/lapack-geev-func (sym complex-tensor) ()
  'mzgeev)
;;
(deft/generic (t/geev-output-fix #'subtypep) sym (wr wi))
(deft/method t/geev-output-fix (sym real-numeric-tensor) (wr wi)
  (let ((csym (or (complexified-type sym) (error "No corresponding complex-tensor defined for type ~a." sym))))
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
	 ,work (length ,work)
	 0
	 (the index-type (head ,A)) (if ,vl (the index-type (head ,vl)) 0) (if ,vr (the index-type (head ,vr)) 0)))))
;;

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
"))


