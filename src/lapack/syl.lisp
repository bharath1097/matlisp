(in-package #:matlisp)
(in-readtable :infix-dispatch-table)

(deft/generic (t/lapack-trsyl-func #'subfieldp) sym ())
(deft/method t/lapack-trsyl-func (sym real-tensor) ()
  'matlisp-lapack:dtrsyl)
(deft/method t/lapack-trsyl-func (sym complex-tensor) ()
  'matlisp-lapack:ztrsyl)
;;
(deft/generic (t/lapack-trsyl! #'subtypep) sym (op.A op.B sgn A ld.a B ld.b C ld.c))
(deft/method t/lapack-trsyl! (sym blas-numeric-tensor) (op.A op.B sgn A ld.a B ld.b C ld.c)
  (using-gensyms (decl (op.A op.B sgn A ld.a B ld.b C ld.c))
    `(let (,@decl)
      (declare (type character ,op.A ,op.B ,sgn)
	       (type ,sym ,A ,B ,C)
	       (type index-type ,ld.a ,ld.b ,ld.c))
      (,(macroexpand-1 `(t/lapack-trsyl-func ,sym))
	,op.a ,op.b (if (char= ,sgn #\P) 1 -1)
	(nrows ,c) (ncols ,c)
	(the ,(store-type sym) (store ,A)) ,ld.a
	(the ,(store-type sym) (store ,B)) ,ld.b
	(the ,(store-type sym) (store ,C)) ,ld.c
	(t/fid* ,(field-type (realified-type sym))) 0
	(the index-type (head ,A)) (the index-type (head ,B)) (the index-type (head ,C))))))

(defgeneric trsyl! (A B C &optional job)
  (:documentation "
    Syntax
    ------
    (trsyl! A B C &optional job)
    Computes the solution to
	      op(A) X \pm X op(B) = C
    where A, B are (quasi) upper triangular matrices.

    The variable JOB can take on any of :{n, t/c}{n, t/c}{p, n}; the
    first two alphabets maps to op A, op B; whilst the third one controls
    the sign in the equation (oh dear! which year have we landed in.).
")
  (:method :before ((A base-tensor) (B base-tensor) (C base-tensor) &optional job)
	   (declare (ignore job))
	   (assert (and (tensor-square-matrixp A)
			(tensor-square-matrixp B)
			(= (nrows A) (nrows C))
			(= (ncols B) (ncols C)))
		   nil 'tensor-dimension-mismatch))
  (:method :before ((A real-numeric-tensor) (B real-numeric-tensor) (C real-numeric-tensor) &optional (job :nnp))
	   (declare (ignore A B C))
	   (destructuring-bind (job.a job.b sgn) (split-job job)
	     (assert (and (member job.a '(#\N #\T))
			  (member job.b '(#\N #\T))
			  (member sgn '(#\N #\P)))
		     nil 'invalid-arguments)))
  (:method :before ((A complex-numeric-tensor) (B complex-numeric-tensor) (C complex-numeric-tensor) &optional (job :nnp))
	   (declare (ignore A B C))
	   (destructuring-bind (job.a job.b sgn) (split-job job)
	     (assert (and (member job.a '(#\N #\C))
			  (member job.b '(#\N #\C))
			  (member sgn '(#\N #\P)))
		     nil 'invalid-arguments))))

(define-tensor-method trsyl! ((A blas-numeric-tensor :input) (B blas-numeric-tensor :input) (C blas-numeric-tensor :output) &optional (job :nnp))
  `(destructuring-bind (op.a op.b sgn) (split-job job)
     (with-columnification (((A #\C) (B #\C)) (C))
       (multiple-value-bind (scale info) (t/lapack-trsyl! ,(cl a) op.a op.b sgn
							  A (or (blas-matrix-compatiblep A #\N) 0)
							  B (or (blas-matrix-compatiblep B #\N) 0)
							  C (or (blas-matrix-compatiblep C #\N) 0))
	 (unless (= info 0)
	   (if (< info 0)
	       (error "TRSYL: Illegal value in the ~:r argument." (- info))
	       (error "TRSYL: A and B have common or very close eigenvalues; perturbed values were used to solve the equation (but the matrices A and B are unchanged).")))
	 (scal! scale C)))))

;;Should we use some fancy pattern matcher to do this ?
;;(solve #i(A * ?x - ?x * B = C))
(defun syl (A B C)
  "
    Syntax
    ------
    (syl A B C)
    Computes the solution to the Sylvester equation:
	    A X + X B = C
    using Schur decomposition."
  (mlet* (((l.a t.a u.a) (schur A) :type (nil real-tensor real-tensor))
	  ((l.b t.b u.b) (schur B) :type (nil real-tensor real-tensor))
	  (ucu #i(u.a' * c * u.b)))
    (trsyl! t.a t.b ucu)
    #i(u.a * ucu * u.b')))
