(in-package :matlisp)

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
"
  (mod-dotimes (idx {seq}) compound-form*)

  (mod-dotimes (var {seq})
    {with (loop-order {:row-major :col-major})}
    {with (linear-sums
	      {(offsets {stride-seq})}*)}
    {with (variables
	      {(vars init &key type)}*)}
    {do ({code}*)})

  Examples:
  > (mod-dotimes (idx (vidx 2 2))
      with (linear-sums (of (vidx 2 1)))
      do (format t \"~a ~a~%\" idx of))
  #(0 0) 0
  #(0 1) 1
  #(1 0) 2
  #(1 1) 3

  > (mod-dotimes (idx (vidx 2 2))
      with (loop-order :col-major)
      with (linear-sums (of (vidx 2 1)))
      do (format t \"~a ~a~%\" idx of))
  #(0 0) 0
  #(1 0) 2
  #(0 1) 1
  #(1 1) 3

  > (mod-dotimes (idx (vidx 2 2))
      with (variables (tmp 1d0 :type double-float))
      with (linear-sums (of (vidx 2 1)))
      do (progn
	   (format t \"~a ~a ~a~%\" idx of tmp)
	   (incf tmp)))
  #(0 0) 0 0d0
  #(0 1) 1 1d0
  #(1 0) 2 2d0
  #(1 1) 3 3d0
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
