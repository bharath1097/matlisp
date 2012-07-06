(in-package #:matlisp)

(defmacro mod-dotimes ((idx dims) &body body)
"
  (mod-dotimes (idx {seq}) compound-form*)

  (mod-dotimes (var {seq})
    {with (loop-order {:row-major :col-major})}
    {with (linear-sums
	      {(offsets {stride-seq})}*)}
    {do ({code}*)})

  Examples:
  > (mod-dotimes (idx (idxv 2 2))
      with (linear-sums (of (idxv 2 1)))
      do (format t \"~a ~a~%\" idx of))
  #(0 0) 0
  #(0 1) 1
  #(1 0) 2
  #(1 1) 3

  > (mod-dotimes (idx (idxv 2 2))
      with (loop-order :col-major)
      with (linear-sums (of (idxv 2 1)))
      do (format t \"~a ~a~%\" idx of))
  #(0 0) 0
  #(1 0) 2
  #(0 1) 1
  #(1 1) 3

  Make sure that \"do\" is specified at the end. Parser stops
  at the first 'do it finds.
"
  (check-type idx symbol)
  (labels ((parse-code (body ret)
	     (cond
	       ((null body)
		(values nil ret))
	       ((eq (car body) 'with)
		(multiple-value-bind (indic decl) (parse-with (cadr body))
		  (setf (getf ret indic) decl))
		(parse-code (cddr body) ret))
	       ;;Let's not do too much
	       #+nil
	       ((eq (car body) 'finally)
		(setf (getf ret :finally) (second body))
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
	       ;;Useless without a finally clause
	       #+nil
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
	`(let ((,dims-sym ,dims))
	   (declare (type (index-array *) ,dims-sym))
	   (let ((,rank-sym (length ,dims-sym)))
	     (declare (type index-type ,rank-sym))
	     (let ((,idx (allocate-index-store ,rank-sym))
		   ,@(mapcar #'(lambda (x) `(,(getf x :stride-sym) ,(getf x :stride-expr))) (getf sdecl :linear-sums))
		   ,@(mapcar #'(lambda (x) `(,(getf x :variable) ,(getf x :init))) (getf sdecl :variables)))
	       (declare (type (index-array *) ,idx)
			,@(when (getf sdecl :linear-sums)
				`((type (index-array *) ,@(mapcar #'(lambda (x) (getf x :stride-sym)) (getf sdecl :linear-sums)))))
			,@(loop for x in (getf sdecl :variables)
				  unless (null (getf x :type))
			     collect `(type ,(getf x :type) ,(getf x :variable))))
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
						    (decf ,(getf decl :offset-sym) (the index-type (* ,cstrd (1- (aref ,dims-sym ,count-sym))))))))))
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
			  finally (return nil))))
	      ,@(unless (null (getf sdecl :finally))
			`(finally (,@(getf sdecl :finally))))))))))))

(defmacro list-loop ((idx ele lst) &rest body)
  "
  (list-loop (idx ele {list}) compound-form*)

  Examples:
  > (list-loop (idx ele '((1 2) (4 5)))
      with (linear-sums (of (idxv 2 1)))
      do (format t \"~a ~a ~a~%\" idx of ele))
  #(0 0) 0 1
  #(0 1) 1 2
  #(1 0) 2 4
  #(1 1) 3 5
"
  (check-type idx symbol)
  (check-type ele symbol)
  (labels ((parse-code (body ret)
	     (cond
	       ((null body)
		(values nil ret))
	       ((eq (car body) 'with)
		(multiple-value-bind (indic decl) (parse-with (cadr body))
		  (setf (getf ret indic) decl))
		(parse-code (cddr body) ret))
	       ;;Let's not do too much.
	       #+nil
	       ((eq (car body) 'finally)
		(setf (getf ret :finally) (second body))
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
	       ;;Traversing the list the other way is far too inefficient and/or too hard to do.
	       #+nil
	       ((and (eq (car code) 'loop-order)
		     (member (cadr code) '(:row-major :col-major)))
		(values :loop-order (second code)))
	       ;;Useless without a finally clause.
	       #+nil
	       ((eq (car code) 'variables)
		(values :variables
			(loop for decl in (cdr code)
			   collect (destructuring-bind (sym init &key type) decl
				     (list :variable sym
					   :init init
					   :type type)))))
	       (t (error 'unknown-token :token (car code) :message "Error in macro: mod-dotimes -> parse-with.~%")))))
    (multiple-value-bind (code sdecl) (parse-code body nil)
      (with-gensyms (lst-sym dims-sym rank-sym lst-rec-sym lst-rec-count-sym lst-rec-lst-sym)
	`(let ((,lst-sym ,lst))
	   (declare (type list ,lst-sym))
	   (let ((,dims-sym (make-index-store (list-dimensions ,lst-sym))))
	     (declare (type (index-array *) ,dims-sym))
	     (let ((,rank-sym (array-dimension ,dims-sym 0)))
	       (declare (type index-type ,rank-sym))
	       (let ((,idx (allocate-index-store ,rank-sym))
		     ,@(mapcar #'(lambda (x) `(,(getf x :offset-sym) ,(getf x :offset-init))) (getf sdecl :linear-sums))
		     ,@(mapcar #'(lambda (x) `(,(getf x :stride-sym) ,(getf x :stride-expr))) (getf sdecl :linear-sums))
		     ,@(mapcar #'(lambda (x) `(,(getf x :variable) ,(getf x :init))) (getf sdecl :variables)))
		 (declare (type (index-array *) ,idx)
			  ,@(when (getf sdecl :linear-sums)
				  `((type (index-array *) ,@(mapcar #'(lambda (x) (getf x :stride-sym)) (getf sdecl :linear-sums)))
				    (type index-type ,@(mapcar #'(lambda (x) (getf x :offset-sym)) (getf sdecl :linear-sums)))))
			  ,@(loop for x in (getf sdecl :variables)
			       unless (null (getf x :type))
			       collect `(type ,(getf x :type) ,(getf x :variable))))
		 (labels ((,lst-rec-sym (,lst-rec-count-sym ,lst-rec-lst-sym)
			    (if (null ,lst-rec-lst-sym)
				(progn
				  (unless (= (aref ,idx ,lst-rec-count-sym) (aref ,dims-sym ,lst-rec-count-sym))
				    (error 'non-uniform-bounds-error :assumed (aref ,dims-sym ,lst-rec-count-sym) :found ,lst-rec-count-sym
					   :message "Error in list-loop, given list is not uniform in dimensions."))
				  (setf (aref ,idx ,lst-rec-count-sym) 0)
				  ,@(loop
				       for decl in (getf sdecl :linear-sums)
				       collect `(decf ,(getf decl :offset-sym) (* (aref ,(getf decl :stride-sym) ,lst-rec-count-sym) (aref ,dims-sym ,lst-rec-count-sym))))
				  ,@(if (null (getf sdecl :finally))`(nil)
					`((when (= ,lst-rec-count-sym 0)
					    ,(getf sdecl :finally)))))
				(progn
				  ;;list-dimensions does not parse the entire list, just goes through caaa..r's to find out the
				  ;;dimensions if it is uniform.
				  (unless (< -1 (aref ,idx ,lst-rec-count-sym) (aref ,dims-sym ,lst-rec-count-sym))
				    (error 'out-of-bounds-error :requested (aref ,idx ,lst-rec-count-sym) :bound (aref ,dims-sym ,lst-rec-count-sym)
					   :message "Error in list-loop, given list is not uniform in dimensions."))
				  (if (consp (car ,lst-rec-lst-sym))
				      (,lst-rec-sym (1+ ,lst-rec-count-sym) (car ,lst-rec-lst-sym))
				      (let ((,ele (car ,lst-rec-lst-sym)))
					,code))
				  (incf (aref ,idx ,lst-rec-count-sym))
				  ,@(loop
				       for decl in (getf sdecl :linear-sums)
				       collect `(incf ,(getf decl :offset-sym) (aref ,(getf decl :stride-sym) ,lst-rec-count-sym)))
				  (,lst-rec-sym ,lst-rec-count-sym (cdr ,lst-rec-lst-sym))))))
		   (,lst-rec-sym 0 ,lst-sym))))))))))
