(in-package #:matlisp)

(defmacro mod-dotimes ((idx dims &key (loop-order *default-stride-ordering* loop-ordering-p) (uplo? :ul)) &body body)
"
  (mod-dotimes (idx {seq} &key loop-order uplo?) compound-form*)

  The argument LOOP-ORDER can either take the keywords {:ROW-MAJOR, :COL-MAJOR},
  or the pindex-store-vector corresponding to a permutation action. In the latter
  case an array of the form [0,...,n] is permuted using APPLY-ACTION!, and parsed
  left-to-right order.

  The argument UPLO? can take one of the keywords {:UL, :U, :L}. If :UL is used,
  the loop run around every point in the cube; if :U is specified only the indices
  defined by upper simplex, including the diagonal, is generated; similarly for :L,
  the lower simplex is generated. Using either {:U, :L} with UPLO?, automatically
  sets the loop-ordering, for expected results. If an argument to LOOP-ORDER is also
  specified along with UPLO?, then you'll in general see things which may have weird
  effects on the control flow.

  Make sure that \"do\" is specified at the end: the parser stops at the first 'do
  it finds.

  (mod-dotimes (var {seq})
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

  > (mod-dotimes (idx (idxv 2 2) :loop-order :col-major)
      with (linear-sums (of (idxv 2 1)))
      do (format t \"~a ~a~%\" idx of))
  #(0 0) 0
  #(1 0) 2
  #(0 1) 1
  #(1 1) 3
"
  (check-type idx symbol)
  (unless loop-ordering-p
    (ecase uplo? (:ul nil) (:u (setq loop-order :col-major)) (:l (setq loop-order :row-major))))
  (labels ((parse-code (body ret)
	     (cond
	       ((null body)
		(values nil ret))
	       ((member (car body) '(with :with))
		(multiple-value-bind (indic decl) (parse-with (cadr body))
		  (setf (getf ret indic) (append (getf ret indic) decl)))
		(parse-code (cddr body) ret))
	       ((member (car body) '(do :do))
		(values (cadr body) ret))
	       (t (error 'unknown-token :token (car body) :message "Error in macro: mod-dotimes -> parse-code.~%"))))
	   (parse-with (code)
	     (cond
	       ((member (car code) '(linear-sums :linear-sums))
		(values :linear-sums
			(loop :for decl :in (cdr code)
			   :collect (destructuring-bind (offst strds &optional (init 0)) decl
				      (list :offset-sym offst
					    :offset-init init
					    :stride-sym (gensym (string+ (symbol-name offst) "-stride"))
					    :stride-expr strds)))))
	       (t (error 'unknown-token :token (car code) :message "Error in macro: mod-dotimes -> parse-with.~%")))))
    (multiple-value-bind (code sdecl) (parse-code body nil)
      (let ((loop-perm (unless (member loop-order '(:row-major :col-major))
			 ;;Assumed to be a permutation action store
			 (prog1 loop-order
			   (setq loop-order nil)))))
	(with-gensyms (perm-sym loopi-sym dims-sym rank-sym count-sym)
	  `(let ((,dims-sym ,dims))
	     (declare (type index-store-vector ,dims-sym))
	     (let ((,rank-sym (length ,dims-sym))
		   ,@(when loop-perm
			   `((,perm-sym ,loop-perm))))
	       (declare (type index-type ,rank-sym)
			,@(when loop-perm
				`((type pindex-store-vector ,perm-sym))))
	       ,@(when loop-perm
		       `((assert (<= (length ,perm-sym) ,rank-sym) nil 'permutation-permute-error)))
	       (let ((,idx (allocate-index-store ,rank-sym))
		     ,@(when loop-perm `((,loopi-sym (allocate-index-store ,rank-sym))))
		     ,@(mapcar #'(lambda (x) `(,(getf x :stride-sym) ,(getf x :stride-expr))) (getf sdecl :linear-sums))
		     ,@(mapcar #'(lambda (x) `(,(getf x :variable) ,(getf x :init))) (getf sdecl :variables)))
		 (declare (type index-store-vector ,idx ,@(when loop-perm `(,loopi-sym)))
			  ,@(when (getf sdecl :linear-sums)
				  `((type index-store-vector ,@(mapcar #'(lambda (x) (getf x :stride-sym)) (getf sdecl :linear-sums)))))
			  ,@(loop :for x :in (getf sdecl :variables)
			       :unless (null (getf x :type))
			       :collect `(type ,(getf x :type) ,(getf x :variable))))
		 ,@(when loop-perm
			 `((very-quickly
			     (loop :for i :of-type index-type :from 0 :below ,rank-sym :do (setf (aref ,loopi-sym i) i))
			     (apply-action! ,loopi-sym ,perm-sym))))
		 (loop ,@(loop :for decl :in (getf sdecl :linear-sums)
			    :append `(:with ,(getf decl :offset-sym) :of-type index-type := ,(getf decl :offset-init)))
		    ,@(unless (null code)
			      `(:do (,@code)))
		    :while (very-quickly
			     ,(append
			       (if loop-perm
				   `(loop :for ,count-sym :of-type index-type :across ,loopi-sym)
				   (ecase loop-order
				     (:row-major `(loop :for ,count-sym :of-type index-type :from (1- ,rank-sym) :downto 0))
				     (:col-major `(loop :for ,count-sym :of-type index-type :from 0 :below ,rank-sym))))
			       `(:do
				 (if ,(recursive-append (ecase uplo?
							  (:ul nil)
							  (:l `(or (and (> ,count-sym 0) (= (aref ,idx ,count-sym) (aref ,idx (1- ,count-sym))))))
							  (:u `(or (and (< ,count-sym (1- ,rank-sym)) (= (aref ,idx ,count-sym) (aref ,idx (1+ ,count-sym)))))))
							`(= (aref ,idx ,count-sym) (1- (aref ,dims-sym ,count-sym))))
				     (progn
				       ,@(loop
					    :for decl :in (getf sdecl :linear-sums)
					    :collect (let ((cstrd (gensym (string+ "cur-" (symbol-name (getf decl :stride-sym))))))
						       `(let ((,cstrd (aref ,(getf decl :stride-sym) ,count-sym)))
							  (declare (type index-type ,cstrd))
							  (unless (= ,cstrd 0)
							    (decf ,(getf decl :offset-sym) (the index-type (* ,cstrd (aref ,idx ,count-sym))))))))
				       (setf (aref ,idx ,count-sym) 0))
				     (progn
				       (incf (aref ,idx ,count-sym))
				       ,@(loop
					    :for decl :in (getf sdecl :linear-sums)
					    :collect (let ((cstrd (gensym (string+ "cur-" (symbol-name (getf decl :stride-sym))))))
						       `(let ((,cstrd (aref ,(getf decl :stride-sym) ,count-sym)))
							  (declare (type index-type ,cstrd))
							  (unless (= ,cstrd 0)
							    (incf ,(getf decl :offset-sym) ,cstrd)))))
				       (return t)))
				 :finally (return nil)))))))))))))

(defmacro dorefs ((idx dims &key (loop-order *default-stride-ordering* loop-ordering-p) (uplo? :ul)) (&rest ref-decls) &rest body)
  (let* ((tsyms (zipsym (mapcar #'second ref-decls)))
	 (rsyms (mapcar #'car ref-decls))
	 (types (mapcar #'(lambda (x) (destructuring-bind (ref ten &key type) x
				       (declare (ignore ref ten))
				       type))
			ref-decls))
	 (ssyms (mapcar #'(lambda (x y) (when y `(,(gensym) (store ,(car x))))) tsyms types))
	 (osyms (mapcar #'(lambda (y) (when y (gensym))) types)))
    `(let-typed (,@(mapcar #'(lambda (x y) (if y (append x `(:type ,y)) x)) tsyms types))
       (let-typed (,@(remove-if #'null (mapcar #'(lambda (x y) (when y (append x `(:type ,(store-type y))))) ssyms types)))
	 (mod-dotimes (,idx ,dims ,@(when loop-ordering-p `(:loop-order ,loop-order)) :uplo? ,uplo?)
	   :with (linear-sums
		  ,@(remove-if #'null (mapcar #'(lambda (of ten typ) (when typ `(,of (strides ,(car ten)) (head ,(car ten)))))
					      osyms tsyms types)))
	   :do (symbol-macrolet (,@(mapcar #'(lambda (ref sto ten of typ) (if typ
									      (list ref `(the ,(field-type typ) (t/store-ref ,typ ,(car sto) ,of)))
									      (list ref `(ref ,(car ten) ,idx))))
						     rsyms ssyms tsyms osyms types))
		 ,@body))))))

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
	     (declare (type index-store-vector ,dims-sym))
	     (let ((,rank-sym (array-dimension ,dims-sym 0)))
	       (declare (type index-type ,rank-sym))
	       (let ((,idx (allocate-index-store ,rank-sym))
		     ,@(mapcar #'(lambda (x) `(,(getf x :offset-sym) ,(getf x :offset-init))) (getf sdecl :linear-sums))
		     ,@(mapcar #'(lambda (x) `(,(getf x :stride-sym) ,(getf x :stride-expr))) (getf sdecl :linear-sums))
		     ,@(mapcar #'(lambda (x) `(,(getf x :variable) ,(getf x :init))) (getf sdecl :variables)))
		 (declare (type index-store-vector ,idx)
			  ,@(when (getf sdecl :linear-sums)
				  `((type index-store-vector ,@(mapcar #'(lambda (x) (getf x :stride-sym)) (getf sdecl :linear-sums)))
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
				       collect `(decf ,(getf decl :offset-sym) (the index-type (* (aref ,(getf decl :stride-sym) ,lst-rec-count-sym) (aref ,dims-sym ,lst-rec-count-sym)))))
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
				       collect `(incf ,(getf decl :offset-sym) (the index-type (aref ,(getf decl :stride-sym) ,lst-rec-count-sym))))
				  (,lst-rec-sym ,lst-rec-count-sym (cdr ,lst-rec-lst-sym))))))
		   (,lst-rec-sym 0 ,lst-sym))))))))))
