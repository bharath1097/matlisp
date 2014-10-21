(in-package #:matlisp)

;;
(deft/generic (t/lapack-geqp! #'subtypep) sym (A lda jpvt tau))
(deft/method t/lapack-geqp! (sym blas-numeric-tensor) (A lda jpvt tau)
  (let* ((ftype (field-type sym)) (complex? (subtypep ftype 'cl:complex)))
    (using-gensyms (decl (A lda jpvt tau) (xxx xxr lwork))
      `(let (,@decl)
	 (declare (type ,sym ,A)
		  (type index-type ,lda)
		  (type (simple-array ,(matlisp-ffi::%ffc->lisp :integer) (*)) ,jpvt)
		  (type ,(store-type sym) ,tau))
	 (with-field-elements ,sym (,@(when complex? `((,xxr (t/fid+ ,ftype) (dimensions ,A 1)))))
	   (with-lapack-query ,sym (,xxx ,lwork)
	     (ffuncall ,(blas-func "geqp3" ftype)
	       (:& :integer) (dimensions ,A 0) (:& :integer) (dimensions ,A 1)	     
	       (:* ,(lisp->ffc ftype) :+ (head ,A)) (the ,(store-type sym) (store ,A)) (:& :integer) ,lda
	       (:* :integer) (the (simple-array ,(matlisp-ffi::%ffc->lisp :integer) (*)) ,jpvt)
	       (:* ,(lisp->ffc ftype)) (the ,(store-type sym) ,tau)
	       (:* ,(lisp->ffc ftype)) (the ,(store-type sym) ,xxx) (:& :integer) ,lwork
	       ,@(when complex? `((:* ,(lisp->ffc ftype)) (the ,(store-type sym) ,xxr)))
	       (:& :integer :output) 0)))))))

(deft/generic (t/lapack-geqr! #'subtypep) sym (A lda tau))
(deft/method t/lapack-geqr! (sym blas-numeric-tensor) (A lda tau)
  (let* ((ftype (field-type sym)))
    (using-gensyms (decl (A lda tau) (xxx lwork))
      `(let (,@decl)
	 (declare (type ,sym ,A)
		  (type index-type ,lda)
		  (type ,(store-type sym) ,tau))
	 (with-lapack-query ,sym (,xxx ,lwork)
	   (ffuncall ,(blas-func "geqrf" ftype)
	     (:& :integer) (dimensions ,A 0) (:& :integer) (dimensions ,A 1)
	     (:* ,(lisp->ffc ftype) :+ (head ,A)) (the ,(store-type sym) (store ,A)) (:& :integer) ,lda
	     (:* ,(lisp->ffc ftype)) (the ,(store-type sym) ,tau)
	     (:* ,(lisp->ffc ftype)) (the ,(store-type sym) ,xxx) (:& :integer) ,lwork
	     (:& :integer :output) 0))))))

(deft/generic (t/lapack-orgqr! #'subtypep) sym (rank A lda tau))
(deft/method t/lapack-orgqr! (sym blas-numeric-tensor) (rank A lda tau)
  (let* ((ftype (field-type sym)) (complex? (subtypep ftype 'cl:complex)))
    (using-gensyms (decl (A lda tau) (xxx lwork))
      `(let (,@decl)
	 (declare (type ,sym ,A)
		  (type index-type ,lda ,rank)
		  (type ,(store-type sym) ,tau))
	 (with-lapack-query ,sym (,xxx ,lwork)
	   (ffuncall ,(blas-func (if complex? "ungqr" "orgqr") ftype)
	     (:& :integer) (dimensions ,A 0) (:& :integer) (dimensions ,A 1) (:& :integer) ,rank
	     (:* ,(lisp->ffc ftype) :+ (head ,A)) (the ,(store-type sym) (store ,A)) (:& :integer) ,lda
	     (:* ,(lisp->ffc ftype)) (the ,(store-type sym) ,tau)
	     (:* ,(lisp->ffc ftype)) (the ,(store-type sym) ,xxx) (:& :integer) ,lwork
	     (:& :integer :output) 0))))))

(deft/generic (t/lapack-ormqr! #'subtypep) sym (side trans rank A lda tau c ldc))
(deft/method t/lapack-ormqr! (sym blas-numeric-tensor) (side trans rank A lda tau c ldc)
  (let* ((ftype (field-type sym)) (complex? (subtypep ftype 'cl:complex)))
    (using-gensyms (decl (side trans A lda tau c ldc) (xxx lwork))
      `(let (,@decl)
	 (declare (type ,sym ,A)
		  (type index-type ,lda ,ldc ,rank)
		  (type ,(store-type sym) ,tau)
		  (type character ,side ,trans))
	 (with-lapack-query ,sym (,xxx ,lwork)
	   (ffuncall ,(blas-func (if complex? "unmqr" "ormqr") ftype)
	     (:& :character) ,side (:& :character) ,trans
	     (:& :integer) (dimensions ,A 0) (:& :integer) (dimensions ,A 1) (:& :integer) ,rank
	     (:* ,(lisp->ffc ftype) :+ (head ,A)) (the ,(store-type sym) (store ,A)) (:& :integer) ,lda
	     (:* ,(lisp->ffc ftype)) (the ,(store-type sym) ,tau)
	     (:* ,(lisp->ffc ftype) :+ (head ,C)) (the ,(store-type sym) (store ,C)) (:& :integer) ,ldc
	     (:* ,(lisp->ffc ftype)) (the ,(store-type sym) ,xxx) (:& :integer) ,lwork
	     (:& :integer :output) 0))))))
;;
(defgeneric geqp! (a)
  (:documentation
   "
  SYNTAX
  ======
  (GEQP! A)

  INPUT
  -----
  A    A Matlisp M x N matrix

  OUTPUT (VALUES Q R PVT-VEC)
  ------
  Q, R     Matlisp matricies representing the QR composition of A*P
  JPVT     A lisp sequence of integers representing the column pivoting.
	   This is the variable JPVT in LAPACK's [DZ]GEGP3.F:

	   JPVT    (input/output) INTEGER array, dimension (N)
		   On entry, if JPVT(J).ne.0, the J-th column of A is permuted
		   to the front of A*P (a leading column); if JPVT(J)=0,
		   the J-th column of A is a free column.
		   On exit, if JPVT(J)=K, then the J-th column of A*P was the
		   the K-th column of A.

	  *** THE EXCEPTION TAKEN HERE IS THAT \"JPVT - 1\" IS RETURNED TO COMPLY ***
	  *** WITH THE ZERO BASED INDEXING OF LISP. ***

  PURPOSE
  =======

  Use QR or QR! for access to this routine.

  Computes the QR factorization of an M-by-N matrix A using column pivoting.
  I.e., A*P = Q*R is computed where P is a column pivoting matrix.
"))

(define-tensor-method geqp! ((a blas-numeric-tensor :output))
  `(let-typed ((jpvt (make-array (dimensions a 1) :element-type ',(matlisp-ffi::%ffc->lisp :integer) :initial-element 0) :type (simple-array ,(matlisp-ffi::%ffc->lisp :integer) (*)))
	       (tau (t/store-allocator ,(cl a) (lvec-min (dimensions a))) :type ,(store-type (cl a))))
     (with-columnification (() (a))
       (let ((info (t/lapack-geqp! ,(cl a) a (or (blas-matrix-compatiblep a #\N) 0) jpvt tau)))
	 (unless (= info 0) (error "GEQP3: the ~a'th argument had an illegal value." (- info)))))
     (values A jpvt tau)))

(defmethod geqp! ((a real-matrix))

  ;; Set up the work spaces need by DGEQP3
  (let* ((m (nrows a))
	 (n (ncols a))
	 (min-nm (min m n))
	 (lda (nrows a))
	 (jpvt (allocate-integer4-store n 0))
	 (tau (allocate-real-store min-nm))
	 (lwork (dgeqp3-workspace-inquiry m n))
	 (work (allocate-real-store lwork)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Okay...off to work...do the basic decomposition
    (multiple-value-bind (store-a jpvt tau work info)
	(lapack:dgeqp3 m n (store a) lda jpvt tau work lwork 0)


      ;; Check for error
      (unless (zerop info)
	(error "QR Decomp faile: Argument ~d in call to DGEQP3 is bad" info))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; If we get here there is no error, construct the R matrix
      (let ((r (make-real-matrix min-nm n)))
	(dotimes (row min-nm)
	  (loop for col from row below n do
		(setf (matrix-ref r row col)
		      (aref store-a (fortran-matrix-indexing row col lda)))))

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; and we need to construct the Q portion.
	(multiple-value-bind (store-a work info)
	    (lapack:dorgqr m min-nm min-nm store-a m tau work lwork 0)

	  (declare (ignore work))

	  ;; Was there any error?
	  (unless (zerop info)
	    (warn "Error in DORGQR in argument ~d.  Returning nil." (- info))
	    (values nil nil nil))

	  ;; No error, so construct the new Q matrix
	  (let ((q (make-real-matrix m min-nm)))

	    (dotimes (row m)
	      (dotimes (col min-nm)
		(setf (matrix-ref q row col)
		      (aref store-a (fortran-matrix-indexing row col lda)))))

	    ;; and return Q, R, and JPVT...
	    (values q r (map (type-of jpvt) #'1- jpvt))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((xx (allocate-real-store 1))
      (xxint (allocate-integer4-store 1))
      (work (allocate-real-store 1)))

  (defun zgeqp3-workspace-inquiry (m n)
    (multiple-value-bind (a jpvt tau work rwork info)
	(lapack:zgeqp3 m n xx m xxint xx work -1 xx 0)

	(declare (ignore a jpvt tau rwork))

	(unless (zerop info)
	  (error "Error in computing required work space dimensions"))

	(values (ceiling (aref work 0))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod geqp! ((a complex-matrix))

  ;; Set up the work spaces need by DGEQP3
  (let* ((m (nrows a))
	 (n (ncols a))
	 (min-nm (min m n))
	 (lda (nrows a))
	 (jpvt (allocate-integer4-store n 0))
	 (tau (allocate-complex-store min-nm))
	 (lwork (dgeqp3-workspace-inquiry m n))
	 (work (allocate-complex-store lwork))
	 (rwork (allocate-real-store (* 2 n))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Okay...off to work...do the basic decomposition
    (multiple-value-bind (store-a jpvt tau work rwork info)
	(lapack:zgeqp3 m n (store a) lda jpvt tau work lwork rwork 0)

      (declare (ignore rwork))

      ;; Check for error
      (unless (zerop info)
	(error "QR Decomp faile: Argument ~d in call to ZGEQP3 is bad" info))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; If we get here there is no error, construct the R matrix
      (let ((r (make-complex-matrix min-nm n))
	    (idx-fortran 0))

	(dotimes (row min-nm)
	  (loop for col from row below n do
		(setf idx-fortran (fortran-complex-matrix-indexing row col lda)
		      (matrix-ref r row col) (complex (aref store-a idx-fortran)
						      (aref store-a (1+ idx-fortran))))))

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; and we need to construct the Q portion.
	(multiple-value-bind (store-a work info)
	    (lapack:zungqr m min-nm min-nm store-a m tau work lwork 0)

	  (declare (ignore work))

	  ;; Was there any error?
	  (unless (zerop info)
	    (warn "Error in DORGQR in argument ~d.  Returning nil." (- info))
	    (values nil nil nil))

	  ;; No error, so construct the new Q matrix
	  (let ((q (make-complex-matrix m min-nm)))

	    (dotimes (row m)
	      (dotimes (col min-nm)
		(setf idx-fortran (fortran-complex-matrix-indexing row col lda)
		      (matrix-ref q row col) (complex (aref store-a idx-fortran)
						      (aref store-a (1+ idx-fortran))))))

	    ;; and return Q, R, and JPVT...
	    (values q r (map (type-of jpvt) #'1- jpvt))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun qr (a &optional (econ t) (pivot nil))
  "
  SYNTAX
  ======
  (QR A [ECON PIVOT])

  INPUT
  -----
  A       A Matlisp matrix of size M x N
  ECON    Produce the economy size QR decomposition which means return Q1 and R1 only.
	  T or NIL. Default is T.
  PIVOT   T or NIL.  Default is NIL.  When PIVOT is true column pivoting is used.

  OUTPUT: (VALUES Q R PVT)
  ------
  Q  A Matlisp matrix of size M x M
  R  A Matlisp matrix of size M x N
  PVT A Lisp sequence of (SIGNED-BYTE 32)

  PURPOSE
  =======
  Compute the QR decompostion of A.  A = Q*R.  When M > N, the QR decomposition
  can be written as
  A*P = [Q1 Q2] * [ R1 ]
		  [ -- ]
		  [ 0  ]

  When ECON == T only Q1 and R1 is return. Otherwise Q and R are returned.
  Note that when ECON == NULL the value of Q2 is taken from the SVD of A;
  this matches the results of OCTAVE and MATLAB.

  When PIVOT == NIL, no column pivoting is used and P == Identity-Matrix

  When PIVOT == T, column pivoting is used to improve accuracy.  In this
  case P represents the rearrangement of columns.  The sequence PVT indicate
  this column pivoting.  Specifically, the I-th column of A*P is the
  \"(ELT PVT I)-th\" column of A.
"

  (cond
   (econ
    (cond
     (pivot  (geqp! (copy a)))
     (t      (geqr! (copy a)))))

   (t
    ;; Okay ... A [Q1 Q2] form was requested, but this only makes sense
    ;; when ROWS > COLS.  When ROWS <= COLS, GEQR! is sufficient
    (if (<= (number-of-rows a) (number-of-cols a))
	(cond
	 (pivot (geqp! (copy a)))
	 (t     (geqr! (copy a))))

      ;; Else we do a QR and an SVD to pick up the null-space of A
      (cond
       (pivot (multiple-value-bind (q1 r1 pvt1)
		  (geqp! (copy a))
		(values
		 ;; Extend the Q part
		 (join q1
		       (matrix-ref (svd (matrix-ref a (seq 0 (1- (number-of-rows a))) (coerce pvt1 'list)) :a)	; the Q2 part
				   (seq 0 (1- (number-of-rows a)))
				   (seq (number-of-cols a) (1- (number-of-rows a))))
		       :horizontal)

		 ;; Extend the R part
		 (join r1 (zeros (- (number-of-rows a) (number-of-cols a))
				 (number-of-cols a)) :vertical)

		 ;; The PVT part does not need extending
		 pvt1)))

       (t     (multiple-value-bind (q1 r1)
		  (geqr! (copy a))
		(values

		 ;;
		 (join q1
		       (matrix-ref (svd a :a)	; the Q2 part
				   (seq 0 (1- (number-of-rows a)))
				   (seq (number-of-cols a) (1- (number-of-rows a))))
		       :horizontal)

		 (join r1 (zeros (- (number-of-rows a) (number-of-cols a))
				 (number-of-cols a)) :vertical)

		 ;; The PVT part is nil since no pivoting is done here
		 nil))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun qr! (a &optional (econ t) (pivot nil))
  "
  SYNTAX
  ======
  (QR! A [ECON])

  PURPOSE
  =======
  Compute the QR decomposition of A.  See QR.

  NOTE:  THIS IS A DESTRUCTIVE VERSION.  THE MATRIX A IS OVERWRITTEN WITH Q.
	 USE THIS ROUTINE ONLY IF A IS VERY LARGE AND YOU DON'T CARE ABOUT
	 IT AFTER THE CALL."

  (cond
   (econ
    (cond
     (pivot  (geqp!  a))
     (t      (geqr!  a))))

   (t
    ;; Okay ... A [Q1 Q2] form was requested, but this only makes sense
    ;; when ROWS > COLS.  When ROWS <= COLS, GEQR! is sufficient
    (if (<= (number-of-rows a) (number-of-cols a))
	(cond
	 (pivot (geqp!  a))
	 (t     (geqr!  a)))

      ;; Else we do a QR and an SVD to pick up the null-space of A
      (cond
       (pivot (multiple-value-bind (q1 r1 pvt1)
		  (geqp!  a)
		(values
		 ;; Extend the Q part
		 (join q1
		       (matrix-ref (svd (matrix-ref a (seq 0 (1- (number-of-rows a))) (coerce pvt1 'list)) :a)	; the Q2 part
				   (seq 0 (1- (number-of-rows a)))
				   (seq (number-of-cols a) (1- (number-of-rows a))))
		       :horizontal)

		 ;; Extend the R part
		 (join r1 (zeros (- (number-of-rows a) (number-of-cols a))
				 (number-of-cols a)) :vertical)

		 ;; The PVT part does not need extending
		 pvt1)))

       (t     (multiple-value-bind (q1 r1)
		  (geqr!  a)
		(values

		 (join q1
		       (matrix-ref (svd a :a)	; the Q2 part
				   (seq 0 (1- (number-of-rows a)))
				   (seq (number-of-cols a) (1- (number-of-rows a))))
		       :horizontal)

		 (join r1 (zeros (- (number-of-rows a) (number-of-cols a))
				 (number-of-cols a)) :vertical)

		 ;; The PVT part is nil since no pivoting is done here
		 nil))))))))
