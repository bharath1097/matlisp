(in-package #:matlisp)
;;
(deft/generic (t/lapack-trsyl! #'subtypep) sym (op.A op.B sgn A ld.a B ld.b C ld.c))
(deft/method t/lapack-trsyl! (sym blas-numeric-tensor) (op.A op.B sgn A ld.a B ld.b C ld.c)
  (let ((ftype (field-type sym)))
    (using-gensyms (decl (op.A op.B sgn A ld.a B ld.b C ld.c))
      `(let (,@decl)
	 (declare (type character ,op.A ,op.B ,sgn)
		  (type ,sym ,A ,B ,C)
		  (type index-type ,ld.a ,ld.b ,ld.c))
	 (ffuncall ,(blas-func "trsyl" ftype)
	   (:& :character) ,op.a (:& :character) ,op.b (:& :integer) (if (char= ,sgn #\P) 1 -1)
	   (:& :integer) (dimensions ,C 0) (:& :integer) (dimensions ,C 1)
	   (:* ,(lisp->ffc ftype) :+ (head ,A)) (the ,(store-type sym) (store ,A)) (:& :integer) ,ld.a
	   (:* ,(lisp->ffc ftype) :+ (head ,B)) (the ,(store-type sym) (store ,B)) (:& :integer) ,ld.b
	   (:* ,(lisp->ffc ftype) :+ (head ,C)) (the ,(store-type sym) (store ,C)) (:& :integer) ,ld.c
	   (:& ,(lisp->ffc (field-type (realified-type sym))) :output) (t/fid* ,(field-type (realified-type sym)))
	   (:& :integer :output) 0)))))

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
       (letv* ((scale info (t/lapack-trsyl! ,(cl a) op.a op.b sgn
					    A (or (blas-matrix-compatiblep A #\N) 0)
					    B (or (blas-matrix-compatiblep B #\N) 0)
					    C (or (blas-matrix-compatiblep C #\N) 0))))
	 (unless (= info 0)
	   (if (< info 0)
	       (error "TRSYL: Illegal value in the ~:r argument." (- info))
	       (error "TRSYL: A and B have common or very close eigenvalues; perturbed values were used to solve the equation (but the matrices A and B are unchanged).")))
	 (scal! scale C)))))

;;Should we use some fancy pattern matcher to do this ?
;;(solve #i(A' * ?x + ?x * B = C))
;;#i(a * b -> b * a, forall a in F, forall b in L(V, V))
;;#i(a * b \neq b * a, a, b are matrices)
;;#i(a + b -> b + a)
(defun syl (A B C)
  "
    Syntax
    ------
    (syl A B C)
    Computes the solution to the Sylvester equation:
	    A X + X B = C
    using Schur decomposition."
  (letv* ((l.a t.a u.a (schur A) :type nil real-tensor real-tensor)
	  (l.b t.b u.b (schur B) :type nil real-tensor real-tensor)
	  ;;We can't use infix-dispatch-table just yet :(
	  (ucu (gemm 1 u.a (gemm 1 c u.b nil nil :nn) nil nil :cn)))
    (trsyl! t.a t.b ucu)
    (gemm 1 u.a (gemm 1 ucu u.b nil nil :nc) nil nil)))

;; (letv* ((a (randn '(10 10)))
;; 	(b (randn '(10 10)))
;; 	(x (randn '(10 10)))
;; 	(c #i(a * x + x * b)))
;;   (norm (t- (syl a b c) x)))
