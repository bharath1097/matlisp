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
				     :offset-sym (gensym (string+ (symbol-name sym) "-offset"))
				     :ref-count 0)))
			   (setf (gethash sym tensor-table) hsh))))
		     (get-tensors (cdr decl)))))
	     (ttrans-p (code)
	       (and (consp code) (eq (first code) 'tensor-ref)
		    (gethash (second code) tensor-table)
		    (eq (third code) idx)))
	     (incref (ten)
	       (incf (getf (gethash ten tensor-table) :ref-count)))
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
				     (if (eq (second (multiple-value-list (get-tensor-class-optimization (getf to-opt :class))))
					     (second (multiple-value-list (get-tensor-class-optimization (getf fr-opt :class)))))
					 (progn
					   (incref (second to)) (incref (second from))
					   (cdr (funcall (getf (get-tensor-class-optimization (getf to-opt :class)) :reader-writer)
							 (getf fr-opt :store-sym) (getf fr-opt :offset-sym) (getf to-opt :store-sym) (getf to-opt :offset-sym))))
					 (list to (find-tensor-refs from nil)))))
				  (to-t?
				   (incref (second to))
				   (let ((to-opt (gethash (second to) tensor-table)))
				     ;;Add type checking here!
				     (cdr (funcall (getf (get-tensor-class-optimization (getf to-opt :class)) :value-writer)
						   from (getf to-opt :store-sym) (getf to-opt :offset-sym)))))
				  (fr-t?
				   (incref (second from))
				   (let ((fr-opt (gethash (second from) tensor-table)))
				     (cons to (funcall (getf (get-tensor-class-optimization (getf fr-opt :class)) :reader)
						       (getf fr-opt :store-sym) (getf fr-opt :offset-sym)))))
				  (t
				   (list to (find-tensor-refs from nil))))))))))
	     (transform-tensor-ref (snippet)
	       (if (eq (first snippet) 'setf)
		   (cons 'setf (transform-setf-tensor-ref (cdr snippet) nil))
		   (destructuring-bind (tref ten index) snippet
		     (assert (eq tref 'tensor-ref))
		     (let ((topt (gethash ten tensor-table)))
		       (if (not (and (eq index idx) topt)) snippet
			   (progn
			     (incref ten)
			     (funcall (getf (get-tensor-class-optimization (getf topt :class)) :reader) (getf topt :store-sym) (getf topt :offset-sym))))))))
	     (find-tensor-refs (code ret)
	       (if (null code) (reverse ret)
		   (cond
		     ((consp code)
		      (if (member (first code) '(tensor-ref setf))
			  (transform-tensor-ref code)
			  (find-tensor-refs (cdr code) (cons (find-tensor-refs (car code) nil) ret))))
		     (t code)))))
      ;;
      (when (eq (caar body) 'declare)
	(get-tensors (cdar body)))
      (let ((tr-body (find-tensor-refs body nil)))
	(with-gensyms (dims-sym rank-sym count-sym)
	  `(let* ((,dims-sym ,dims)
		  (,rank-sym (length ,dims-sym))
		  (,idx (allocate-index-store ,rank-sym))
		  ,@(loop for key being the hash-keys of tensor-table
		       when (> (getf (gethash key tensor-table) :ref-count) 0)
		       collect (let ((hsh (gethash key tensor-table)))
				 `(,(getf hsh :stride-sym) (strides ,key))))
		  ,@(loop for key being the hash-keys of tensor-table
		       when (> (getf (gethash key tensor-table) :ref-count) 0)
		       collect (let ((hsh (gethash key tensor-table)))
				 `(,(getf hsh :store-sym) (store ,key)))))
	     (declare (type (index-array *) ,idx ,@(loop for key being the hash-keys of tensor-table
						      when (> (getf (gethash key tensor-table) :ref-count) 0)
						      collect (getf (gethash key tensor-table) :stride-sym)))
		      ,@(loop for key being the hash-keys of tensor-table
			   when (> (getf (gethash key tensor-table) :ref-count) 0)
			   collect (let* ((hsh (gethash key tensor-table))
					  (opt (get-tensor-class-optimization (getf hsh :class))))
				     `(type ,(linear-array-type (getf opt :store-type)) ,(getf hsh :store-sym)))))
	     (loop
		,@(loop for key being the hash-keys of tensor-table
		     when (> (getf (gethash key tensor-table) :ref-count) 0)
		     append (let ((hsh (gethash key tensor-table)))
			      `(with ,(getf hsh :offset-sym) of-type index-type = (head ,key))))
		do (locally
		       ,@tr-body)
		;;Optimized for row-order
		while (loop for ,count-sym of-type index-type from (1- ,rank-sym) downto 0
			 do (if (= (aref ,idx ,count-sym) (1- (aref ,dims-sym ,count-sym)))
				(progn
				  (setf (aref ,idx ,count-sym) 0)
				  ,@(loop for key being the hash-keys of tensor-table
				       when (> (getf (gethash key tensor-table) :ref-count) 0)
				       collect (let ((hsh (gethash key tensor-table)))
						 `(decf ,(getf hsh :offset-sym) (* (aref ,(getf hsh :stride-sym) ,count-sym) (1- (aref ,dims-sym ,count-sym)))))))
				(progn
				  (incf (aref ,idx ,count-sym))
				  ,@(loop for key being the hash-keys of tensor-table
				       when (> (getf (gethash key tensor-table) :ref-count) 0)
				       collect (let ((hsh (gethash key tensor-table)))
						 `(incf ,(getf hsh :offset-sym) (aref ,(getf hsh :stride-sym) ,count-sym))))
				  (return t)))
			 finally (return nil)))))))))
