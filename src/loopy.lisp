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

(defmacro mod-dotimes ((idx dims) &body body)
  (check-type idx symbol)
  (labels ((parse-code (body ret)
	     (cond
	       ((null body)
		(values nil ret))
	       ((eq (car body) 'with)
		(multiple-value-bind (indic decl) (parse-with (cadr body))
		  (setf (getf ret indic) decl))
		(parse-code (cddr body) ret))
	       ((eq (car body) 'do)
		(values (cadr body) ret))
	       (t (error 'unknown-token :token (car body) :message "Error in macro: mod-dotimes -> parse-code.~%"))))
	   (parse-with (code)
	     (cond
	       ((eq (car code) 'linear-sums)
		(values :linear-sums
			(loop for decl in (cdr code)
			   collect (destructuring-bind (offst strds &optional (init 0)) decl
				     (list :offset-sym offst
					   :offset-init init
					   :stride-sym (gensym (string+ (symbol-name offst) "-stride"))
					   :stride-expr strds)))))
	       ((and (eq (car code) 'loop-order)
		     (member (cadr code) '(:row-major :col-major)))
		(values :loop-order (second code)))
	       ((eq (car code) 'variables)
		(values :variables
			(loop for decl in (cdr code)
			   collect (destructuring-bind (sym init &key type) decl
				     (list :variable sym
					   :init init
					   :type type)))))
	       (t (error 'unknown-token :token (car code) :message "Error in macro: mod-dotimes -> parse-with.~%")))))
    (multiple-value-bind (code sdecl) (parse-code body nil)
      (with-gensyms (dims-sym rank-sym count-sym)
	`(let* ((,dims-sym ,dims)
		(,rank-sym (length ,dims-sym))
		(,idx (allocate-index-store ,rank-sym))
		,@(mapcar #'(lambda (x) `(,(getf x :stride-sym) ,(getf x :stride-expr))) (getf sdecl :linear-sums))
		,@(mapcar #'(lambda (x) `(,(getf x :variable) ,(getf x :init))) (getf sdecl :variables)))
	   ,@(let ((decl `(,@(when (getf sdecl :linear-sums)
				   `((type (index-array *) ,@(mapcar #'(lambda (x) (getf x :stride-sym)) (getf sdecl :linear-sums)))))
			   ,@(loop for x in (getf sdecl :variables)
				unless (null (getf x :type))
				collect `(type ,(getf x :type) ,(getf x :variable))))))
		  (unless (null decl)
		    `((declare ,@decl))))
	   (loop ,@(loop for decl in (getf sdecl :linear-sums)
		      append `(with ,(getf decl :offset-sym) of-type index-type = ,(getf decl :offset-init)))
	      ,@(unless (null code)
		 `(do (,@code)))
	      while (very-quickly
		      ,(append
			(if (member (getf sdecl :loop-order) '(nil :row-major))
			    `(loop for ,count-sym of-type index-type from (1- ,rank-sym) downto 0)
			    `(loop for ,count-sym of-type index-type from 0 below ,rank-sym))
			`(do
			  (if (= (aref ,idx ,count-sym) (1- (aref ,dims-sym ,count-sym)))
			      (progn
				(setf (aref ,idx ,count-sym) 0)
				,@(loop
				     for decl in (getf sdecl :linear-sums)
				     collect (let ((cstrd (gensym (string+ "cur-" (symbol-name (getf decl :stride-sym))))))
					       `(let ((,cstrd (aref ,(getf decl :stride-sym) ,count-sym)))
						  (declare (type index-type ,cstrd))
						  (unless (= ,cstrd 0)
						    (decf ,(getf decl :offset-sym) (* ,cstrd (1- (aref ,dims-sym ,count-sym)))))))))
			      (progn
				(incf (aref ,idx ,count-sym))
				,@(loop
				     for decl in (getf sdecl :linear-sums)
				     collect (let ((cstrd (gensym (string+ "cur-" (symbol-name (getf decl :stride-sym))))))
					       `(let ((,cstrd (aref ,(getf decl :stride-sym) ,count-sym)))
						  (declare (type index-type ,cstrd))
						  (unless (= ,cstrd 0)
						    (incf ,(getf decl :offset-sym) ,cstrd)))))
				(return t)))
			    finally (return nil))))))))))



(defun modidx (n i len dims)
  (declare (optimize (speed 3) (safety 0))
	   (type index-type n)
	   (type (index-array *) dims))
  (multiple-value-bind (div rem) (floor n (aref dims i))
    (declare (ignore div))
    (if (= i len) t
	(modidx rem (1+ i) len dims))))



(very-quickly
  (let ((idx (allocate-index-store 3))
	(cdims (idxv 10000 100 1))
	(rem 0))
    (declare (type (index-array *) idx cdims)
	     (type index-type rem))
    (time   
     (loop for i of-type index-type from 0 below (* 100 100 100)
	do (progn
	     (setf rem i)
	     (loop
		for j of-type index-type from 0 below 3
		do (setf (values (aref idx j) rem) (floor rem (aref cdims j)))))))))

(time 
 (let ((idx (allocate-index-store 3)))
   (loop for i of-type index-type from 0 below (* 1000 1000 1000)
      do (multiple-value-bind (div rem) (floor i 1000000)
	   (setf (aref idx 0) div)
	   (multiple-value-bind (div-1 rem-1) (floor rem 1000)
	     (setf (aref idx 1) div-1)
	     (setf (aref idx 2) rem-1))))))

;;Very ugly inflexible code; get rid of this in some time or make use of mod-dotimes.
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
						   (find-tensor-refs from nil) (getf to-opt :store-sym) (getf to-opt :offset-sym)))))
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
