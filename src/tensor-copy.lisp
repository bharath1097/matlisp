(in-package :matlisp)

(defparameter *lisp-copy-upper-bound* 1000
  "When (< (store-size te) *LISP-COPY-UPPER-BOUND*) the method defined in Lisp
is used, else the fortran routine is called instead.
")

(defun column-major-p (offsets dims)
  (loop
     for off across offsets
     and dim across dims
     and accumulated-off = 1 then (* accumulated-off dim)
     unless (= off accumulated-off) do (return nil)
     finally (return t)))

(defun row-major-p (offsets dims)
  (very-quickly
    (loop
       for idx of-type index-type from (1- (length dims)) downto 0
       for dim of-type index-type = (aref dims idx)
       for off of-type index-type = (aref offsets idx)
       and accumulated-off of-type index-type = 1 then (* accumulated-off dim)
       unless (= off accumulated-off) do (return nil)
       finally (return t))))

(defmacro mod-loop ((idx dims) &body body)
  (check-type idx symbol)
  (let ((tensor-table (make-hash-table)))
    (labels ((get-tensors (decl)
	       (if (null decl) t
		   (let ((cdecl (car decl)))
		     (when (and (eq (first cdecl) 'type)
				(get-tensor-class-optimization (second cdecl)))
		       (dolist (sym (cddr cdecl))
			 (let ((hsh (list
				     :class (second cdecl)
				     :stride-sym (gensym (string+ (symbol-name sym) "-stride"))
				     :store-sym (gensym (string+ (symbol-name sym) "-store"))
				     :offset-sym (gensym (string+ (symbol-name sym) "-offset")))))
			   (setf (gethash sym tensor-table) hsh))))
		     (get-tensors (cdr decl)))))
	     (ttrans-p (code)
	       (and (eq (first code) 'tensor-ref)
		    (gethash (second code) tensor-table)
		    (eq (third code) idx)))
	     (transform-setf-tensor-ref (snippet ret)
	       (if (null snippet) ret
		   (transform-setf-tensor-ref
		    (cddr snippet)
		    (append ret
			    (destructuring-bind (to from &rest rest) snippet
			      (declare (ignore rest))
			      (let ((to-t? (ttrans-p to))
				    (fr-t? (ttrans-p from)))
				(cond
				  ((and to-t? fr-t?)
				   (let ((to-opt (gethash (second to) tensor-table))
					 (fr-opt (gethash (second from) tensor-table)))
				     ;;Add type checking here!
				     (cdr (funcall (getf (get-tensor-class-optimization (getf to-opt :class)) :reader-writer)
						   (getf fr-opt :store-sym) (getf fr-opt :offset-sym) (getf to-opt :store-sym) (getf to-opt :offset-sym)))))
				  (to-t?
				   (let ((to-opt (gethash (second to) tensor-table)))
				     ;;Add type checking here!
				     (cdr (funcall (getf (get-tensor-class-optimization (getf to-opt :class)) :value-writer)
						   from (getf to-opt :store-sym) (getf to-opt :offset-sym)))))
				  (fr-t?
				   (let ((fr-opt (gethash (second from) tensor-table)))
				     (cons to (funcall (getf (get-tensor-class-optimization (getf fr-opt :class)) :reader)
						       (getf fr-opt :store-sym) (getf fr-opt :offset-sym)))))
				  (t
				   (list to from)))))))))
	     (transform-tensor-ref (snippet)
	       (if (eq (first snippet) 'setf)
		   (cons 'setf (transform-setf-tensor-ref (cdr snippet) nil))
		   (destructuring-bind (tref ten index) snippet
		     (assert (eq tref 'tensor-ref))
		     (let ((topt (gethash ten tensor-table)))
		       (if (not (and (eq index idx) topt)) snippet
			   (funcall (getf (get-tensor-class-optimization (getf topt :class)) :reader) (getf topt :store-sym) (getf topt :offset-sym)))))))
	     (find-tensor-refs (code ret)
	       (if (null code) (reverse ret)
		   (cond
		     ((consp code)
		      (if (member (first code) '(tensor-ref setf))
			  (transform-tensor-ref code)
			  (find-tensor-refs (cdr code) (cons (find-tensor-refs (car code) nil) ret))))
		     (t code)))))
      (when (eq (caar body) 'declare)
	(get-tensors (cdar body)))
      (with-gensyms (dims-sym rank-sym count-sym)
	`(let* ((,dims-sym ,dims)
		(,rank-sym (length ,dims-sym))
		(,idx (allocate-index-store ,rank-sym))
		,@(loop for key being the hash-keys of tensor-table
		     collect (let ((hsh (gethash key tensor-table)))
			       `(,(getf hsh :stride-sym) (strides ,key))))
		,@(loop for key being the hash-keys of tensor-table
		     collect (let ((hsh (gethash key tensor-table)))
			       `(,(getf hsh :store-sym) (store ,key)))))
	   (declare (type (index-array *) ,idx ,@(loop for key being the hash-keys of tensor-table
						    collect (getf (gethash key tensor-table) :stride-sym)))
		    ,@(loop for key being the hash-keys of tensor-table
			   collect (let* ((hsh (gethash key tensor-table))
					  (opt (get-tensor-class-optimization (getf hsh :class))))
				     `(type ,(linear-array-type (getf opt :store-type)) ,(getf hsh :store-sym)))))
	   (loop
	      ,@(loop for key being the hash-keys of tensor-table
		   append (let ((hsh (gethash key tensor-table)))
			    `(with ,(getf hsh :offset-sym) of-type index-type = (head ,key))))
	      do (locally
		   ,@(find-tensor-refs body nil))
	      while (dotimes (,count-sym ,rank-sym nil)
		      (declare (type index-type ,count-sym))
		      (if (= (aref ,idx ,count-sym) (1- (aref ,dims-sym ,count-sym)))
			  (progn
			    (setf (aref ,idx ,count-sym) 0)
			    ,@(loop for key being the hash-keys of tensor-table
				 collect (let ((hsh (gethash key tensor-table)))
					   `(decf ,(getf hsh :offset-sym) (* (aref ,(getf hsh :stride-sym) ,count-sym) (1- (aref ,dims-sym ,count-sym)))))))
			  (progn
			    (incf (aref ,idx ,count-sym))
			    ,@(loop for key being the hash-keys of tensor-table
				 collect (let ((hsh (gethash key tensor-table)))
					   `(incf ,(getf hsh :offset-sym) (aref ,(getf hsh :stride-sym) ,count-sym))))
			    (return t))))))))))
       
(defun tensor-copy (from to)
  (declare (optimize (speed 3) (safety 0))
	   (type real-tensor to from))
  (let ((dims (dimensions from)))
    (mod-loop (idx dims)
      (declare (type real-tensor to from))
      (setf (tensor-ref to idx) (tensor-ref from idx)))))


(let ((x (make-real-tensor-dims 100 100 100))
      (y (make-real-tensor-dims 100 100 100)))
  (mod-loop (idx #(100 100 100))
    (declare (type real-tensor x y))
    (setf (tensor-ref x idx) (random 1d0)))
  (time (tensor-copy x y)))

(defmacro generate-typed-copy!-func (func store-type matrix-type blas-func)
  ;;Be very careful when using functions generated by this macro.
  ;;Indexes can be tricky and this has no safety net
  ;;Use only after checking the arguments for compatibility.
  `(defun ,func (mat-a mat-b)
     (declare (type ,matrix-type mat-a mat-b)
	      (optimize (safety 0) (speed 3)))
     (mlet* (((cp-a inc-a sz-a) (blas-copyable-p mat-a) :type (boolean fixnum nil))
	     ((cp-b inc-b sz-b) (blas-copyable-p mat-b) :type (boolean fixnum nil))
	     ((hd-a st-a sz) (slot-values mat-a '(head store number-of-elements)) :type (fixnum (,store-type *) fixnum))
	     ((hd-b st-b) (slot-values mat-b '(head store)) :type (fixnum (,store-type *))))
	    (if (and cp-a cp-b)
		(,blas-func sz st-a inc-a st-b inc-b :head-x hd-a :head-y hd-b)
		(mlet* (((nr-a nc-a rs-a cs-a) (slot-values mat-a '(number-of-rows number-of-cols row-stride col-stride))
			 :type (fixnum fixnum fixnum fixnum))
			((rs-b cs-b) (slot-values mat-b '(row-stride col-stride))
			 :type (fixnum fixnum)))
		       ;;Choose the smaller of the loops
		       (when (> (nrows mat-a) (ncols mat-a))
			 (rotatef nr-a nc-a)
			 (rotatef rs-a cs-a)
			 (rotatef rs-b cs-b))
		       (loop for i from 0 below nr-a
			  do (,blas-func nc-a st-a cs-a st-b cs-b :head-x (+ hd-a (* i rs-a)) :head-y (+ hd-b (* i rs-b)))))))
     mat-b))


(defun real-typed-copy!-func (ten-a ten-b)
  


(defun find-longest-chain (stds dims))
  
;; (defun tensor-copy (to from)
;;   (declare (optimize (speed 3) (safety 0))
;; 	   (type real-tensor to from))
;;   (let* ((rank (rank to))
;; 	 (dims (dimensions to))
;; 	 (t-strides (strides to))
;; 	 (f-strides (strides from))
;; 	 (t-store (store to))
;; 	 (f-store (store from))
;; 	 (idx (allocate-index-store rank)))
;;     (declare (type (index-array *) dims t-strides f-strides idx)
;; 	     (type (real-array *) t-store f-store))
;;     (loop
;;        with of-t of-type index-type = (head to)
;;        with of-f of-type index-type = (head from)
;;        do (setf (aref t-store of-f) (aref f-store of-f))
;;        while (dotimes (i rank nil)
;; 	       (incf (aref idx i))
;; 	       (incf of-t (aref t-strides i))
;; 	       (incf of-f (aref f-strides i))
;; 	       (when (< (aref idx i) (aref dims i)) (return t))
;; 	       (setf (aref idx i) 0)
;; 	       (decf of-t (* (aref t-strides i) (aref dims i)))
;; 	       (decf of-f (* (aref f-strides i) (aref dims i)))))))

;; (cffi:define-foreign-library strided-copy
;;   (t (:default "/home/neptune/devel/matlisp/csrc/libtcopy")))

;; (cffi:use-foreign-library strided-copy)

;; (cffi:defcfun ("tcopy_" fortran-tcopy) :void
;;   (rank :pointer :int64) (dims :pointer :int64)
;;   (head-t :pointer :int64) (strides-t :pointer :int64) (data-t :pointer :double)
;;   (head-f :pointer :int64) (strides-f :pointer :int64) (data-f :pointer :double)
;;   (idx-work :pointer :int64))

;; (defun tcopy (rank dims head-t strides-t data-t head-f strides-f data-f idx-work)
;;   (with-foreign-objects-stacked ((r :int64 :initial-element rank)
;; 				 (ht :int64 :initial-element head-t)
;; 				 (hf :int64 :initial-element head-f))
;;     (fortran-tcopy r (sb-sys:vector-sap dims)
;; 		   ht (sb-sys:vector-sap strides-t) (sb-sys:vector-sap data-t)
;; 		   hf (sb-sys:vector-sap strides-f) (sb-sys:vector-sap data-f)
;; 		   (sb-sys:vector-sap idx-work))))

;; (cffi:defcfun ("strided_copy" strided-copy) :void
;;   (rank :int64) (dims :pointer :int64)
;;   (head-t :int64) (strides-t :pointer :int64) (data-t :pointer :double)
;;   (head-f :int64) (strides-f :pointer :int64) (data-f :pointer :double)
;;   (idx-work :pointer :int64))

;; (let* ((idx (allocate-index-store (rank x))))
;;   (time (strided-copy (rank x) (sb-sys:vector-sap (dimensions x))
;; 		(head x) (sb-sys:vector-sap (strides x)) (vector-data-address (store x))
;; 		(head y) (sb-sys:vector-sap (strides y)) (vector-data-address (store y))
;; 		(sb-sys:vector-sap idx))))



;;
#+nil
(defun test-tensor-1k-dot ()
  (declare (optimize (speed 3) (safety 0)))
  (let ((t-a (make-real-tensor 1000 1000))
	(t-b (make-real-tensor 1000 1000))
	(t-c (make-real-tensor 1000 1000)))
    (declare (type real-tensor t-a t-b t-c))
    (let ((s-a (store t-a))
	  (s-b (store t-b))
	  (s-c (store t-c)))
      (declare (type (real-array 1000000) s-a s-b s-c))
      (dotimes (i (* 1000 1000))
	(setf (aref s-a i) (random 1d0))
	(setf (aref s-b i) (random 1d0)))
      (time
       (loop for n from 0 below (the index-type (* 1000 1000)) do
	    (multiple-value-bind (i j) (floor n 1000)
	      (declare (type index-type i j))
	      (setf (aref s-c (+ (* i 1000) j))
		    (ddot 1000 (vector-data-address s-a) 1 (vector-data-address s-b) 1000 :head-x (* i 1000) :head-y j))))))))

