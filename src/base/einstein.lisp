(in-package :matlisp)

(defun has-sym (lst sym)
  (if (atom lst) (eql lst sym)
      (or (has-sym (car lst) sym) (has-sym (cdr lst) sym))))

;;Only works for distinct objects
(defun generate-permutations (lst)
  (if (null (cdr lst)) (list lst)
      (apply #'append (mapcar #'(lambda (x)
				  (let ((pop-x (setrem lst x)))
				    (mapcar #'(lambda (y) (cons x y)) (generate-permutations pop-x))))
			      lst))))

(defun parse-loopx (type place clause)
  (let* ((refs (let ((tmp (getcons (list place clause) 'ref))
		     (ret nil))
		 (loop :for ele :in tmp
		    :do (setf ret (setadd ret ele #'equal)))
		 ret))
	 (tens (let ((ret nil))
		 (loop :for ele :in refs
		    :do (setf ret (setadd ret (if (symbolp (second ele))
						  (second ele)
						  (error "error: tensor argument is not a symbol.")))))
		 ret))
	 (tlist (mapcar #'(lambda (sym)
			    (let* ((gsym sym)
				   (hsym (gensym (string+ "head-" (symbol-name sym)))))
			      `(:tensor (,gsym ,sym :type ,type)
				:head (,hsym (head ,gsym) :type index-type)
				:store (,(gensym (string+ "store-" (symbol-name sym))) (store ,gsym) :type ,(store-type type))
				:strides (,(gensym (string+ "strides-" (symbol-name sym))) (strides ,gsym) :type index-store-vector)
				:dimensions (,(gensym (string+ "dimensions-" (symbol-name sym))) (dimensions ,gsym) :type index-store-vector))))
			tens))
	 (indices (let ((tmp nil)
			(idx-pos (apply #'append (mapcar #'(lambda (x) (loop :for ele :in (cddr x)
									  :counting t :into i
									  :collect `(,ele (,(cadr x) ,(1- i))))) refs))))
		    (loop :for ipos :in idx-pos
		       :do (let ((cdim (find (car ipos) tmp :key #'car)))
			     (if cdim
				 (rplacd (last cdim) (cdr ipos))
				 (push ipos tmp))))
		    tmp)))
    (values refs tlist indices)))

;;Add options (allow function to compile the clause ?) for more compiler options.
(defun loop-generator-base (type index-order place clause &key (testp t) (tight-iloop nil))
  (multiple-value-bind (refs tlist indices) (parse-loopx type place clause)
    (let* ((tens (mapcar #'(lambda (x) (second (getf x :tensor))) tlist))
	   (indices (progn
		      (loop :for idx :in indices
			 :do (assert (member (car idx) index-order) nil  "Error index ~a not found in the index-order." (car idx)))
		      (loop :for idx :in index-order
			 :collect (let ((cdim (find idx indices :key #'car)))
				    (assert (not (null cdim)) nil "Error index ~a not found in the expression." idx)
				    cdim))))
	   (idx-d (let ((refrem (maptree '(ref) #'(lambda (x) (declare (ignore x)) (values t #'mapcar)) clause)))
		    (remove-if #'null (mapcar #'(lambda (x) (when (has-sym refrem x) x)) (mapcar #'car indices))))))
      (labels ((get-prop (x &optional prop)
		 (let ((plst (find x tlist :key #'(lambda (x) (cadr (getf x :tensor))))))
		   (if prop
		       (getf plst prop)
		       plst)))
	       (get-offset (x)
		 (caar (second (find (cdr x) (get-prop (car x) :offsets) :key #'car :test #'list-eq)))))
	;;Populate offsets
	(loop :for ref :in refs
	   :do (let* ((plist (get-prop (second ref)))
		      (ofsym (gensym (string+ "offset-" (symbol-name (second (getf plist :tensor))))))
		      (ret `((,ofsym ,(car (getf plist :head)) :type index-type)
			     (,(gensym (string+ "ref-" (symbol-name (second (getf plist :tensor))))) (t/store-ref ,type ,(car (getf plist :store)) ,ofsym) :type ,(field-type type)))))
		 (if (getf plist :offsets)
		     (setf (getf plist :offsets) (append (getf plist :offsets) (list (list (cddr ref) ret))))
		     (rplacd (last plist) (list :offsets (list (list (cddr ref) ret)))))))
	;;Compute offset increments
	(let ((rev (reverse indices)))
	  (labels ((get-incs (idxs acc decl incs ten ofst)
		     (if (null idxs) (values decl incs)
			 (let* ((clst (car idxs))
				(cidx (car clst))
				(idx-rem (mapcar #'car idxs))
				(tloop (and tight-iloop (eql cidx (car (last index-order))))))
			   (cond
			     ((loop :for ele :in (car ofst)
				 :do (when (member ele idx-rem)
				       (return nil))
				 :finally (return t))
			      (values (append (make-list (length idxs)) decl)
				      (append (make-list (1- (length idxs))) (cons `(setf ,(caar (cadr ofst)) ,(car (get-prop ten :head))) incs))))
			     (t
			      (let* ((plst (get-prop ten))
				     (dsym (gensym (string+ "d-stp-" (symbol-name cidx) "-" (symbol-name ten))))
				     (memp (member cidx (car ofst)))
				     (stp (when memp `(aref ,(car (getf plst :strides)) ,(position cidx (car ofst))))))
				(get-incs (cdr idxs) (if memp (list `(the index-type (* ,(if tloop 1 stp) (aref ,(car (getf plst :dimensions)) ,(position cidx (car ofst)))))) nil)
					  (if (or tloop (and (null acc) (not memp))) (cons nil decl)
					      (cons
					       (if memp
						   `(,dsym ,(if (null acc) stp `(the index-type (- ,stp ,@acc))) :type index-type)
						   `(,dsym (the index-type (- ,@acc)) :type index-type))
					       decl))
					  (if (and (null acc) (not memp)) (cons nil incs)
					      (cons `(incf ,(caar (cadr ofst)) ,@(unless tloop `(,dsym))) incs))
					  ten ofst))))))))
	    (mapcar #'(lambda (ten)
			(loop :for ofst :in (get-prop ten :offsets)
			   :do (rplacd (last ofst) (multiple-value-list (get-incs rev nil nil nil ten ofst)))))
		    tens)))
	;;
	(labels ((testgen ()
		   `((assert (and ,@(mapcar #'(lambda (idx)
						`(= ,@(mapcar #'(lambda (x) `(aref ,(car (get-prop (car x) :dimensions)) ,(cadr x))) (cdr idx)))) indices)
				  ,@(mapcar #'(lambda (r) `(= (length ,(car (get-prop (second r) :dimensions))) ,(length (cddr r)))) refs))
			     nil "error: arguments are not of appropriate sizes.")
		     ,@(when tight-iloop
			     `((assert (= 1 ,@(mapcar #'(lambda (x) `(aref ,(car (get-prop (car x) :strides)) ,(cadr x))) (cdar (last indices)))) nil "error: Inner loop strides are not 1.")))))
		 ;;
		 (t/compile (place clause)
		   (let* ((cclause (maptree '(ref) #'(lambda (x)
						       (values (let* ((plst (get-prop (cadr x)))
								      (ofset-sym (get-offset (cdr x))))
								 `(t/store-ref ,type ,(car (getf plst :store)) ,ofset-sym))
							       #'mapcar))
					    clause))
			  (ftype (field-type type)))
		     (setf cclause
			   (maptree '(* + - /) #'(lambda (x)
						   (let ((op (car x)))
						     (values `(,(case op (* 't/f*) (+ 't/f+) (- 't/f-) (/ 't/f/))
								,ftype
								,@(cdr x))
							     #'mapcar)))
				    cclause))
		     (let ((plst (get-prop (cadr place)))
			   (valsym (gensym "value")))
		       `((let-typed ((,valsym (t/f+ ,ftype (t/store-ref ,type ,(car (getf plst :store)) ,(get-offset (cdr place)))
						    ,cclause ) :type ,ftype))
				    (t/store-set ,type ,valsym ,(car (getf plst :store)) ,(get-offset (cdr place))))))))
		 ;;
		 (loopgen (idxs place clause)
		    (if (null idxs) (t/compile place clause)
			(let ((cidx (caar idxs))
			      (clst (car idxs)))
			  (let ((tdecl (let ((ilist (mapcar #'car idxs)))
					 (remove-if #'null
						    (apply #'append
							   (mapcar #'(lambda (ten)
								       (mapcar #'(lambda (ofs)
										   (when (loop :for idx :in (car ofs)
											    :do (when (member idx ilist)
												  (return nil))
											    :finally (return t))
										     (let ((decl (cadr (cadr ofs))))
										       (setf clause (maptree '(ref) #'(lambda (x)
															(values
															 (if (and (eql (cadr x) ten) (equal (cddr x) (car ofs)))
															     (car decl)
															     x)
															 #'mapcar))
													     clause))
										       decl)))
									       (get-prop ten :offsets)))
								   (setrem tens (cadr place))))))))
			    (list
			     (recursive-append
			      (unless (null tdecl)
				`(let-typed (,@tdecl)))
			      `(loop ,@(let ((repl `(aref ,(car (get-prop (car (cadr clst)) :dimensions)) ,(cadr (cadr clst)))))
					    (if (member cidx idx-d)
						`(:for ,cidx :of-type index-type :from 0 :below ,repl)
						`(:repeat ,repl)))
				  :do (progn
					,@(loopgen (cdr idxs) place clause)
					,@(remove-if #'null (apply #'append
								   (mapcar #'(lambda (ten) (mapcar #'(lambda (x) (elt (fourth x) (position cidx index-order))) (get-prop ten :offsets))) tens))))))))))))
	  ;;
	  `(locally
	       (declare (type ,type ,@tens))
	     (let-typed (,@(apply #'append (mapcar #'(lambda (ten) (mapcar #'(lambda (prop) (get-prop ten prop)) '(:head :store :strides :dimensions))) tens)))
			(let-typed (,@(apply #'append (mapcar #'(lambda (ten) (mapcar #'(lambda (x) (car (second x))) (get-prop ten :offsets))) tens))
				      ,@(remove-if #'null (apply #'append (mapcar #'(lambda (ten) (apply #'append (mapcar #'third (get-prop ten :offsets)))) tens))))
				   ,@(when testp (testgen))
				   (very-quickly
				     ,@(loopgen indices place clause))))
	     ,(cadr place)))))))

(defmacro einstein-sum-base (type idx-order place clause &optional (testp t) (tight-iloop nil))
  (loop-generator-base type idx-order place clause :testp testp :tight-iloop tight-iloop))

;;Push this code into loop-generator-base ?
(defun loop-generator (type index-order place clause &key (testp t))
  (multiple-value-bind (refs tlist indices) (parse-loopx type place clause)
    (declare (ignore refs tlist))
    (let ((in-idx (find (car (last index-order)) indices :key #'car)))
      `(if (= 1 ,@(mapcar #'(lambda (x) `(aref (the index-store-vector (strides ,(car x))) ,(cadr x))) (cdr in-idx)))
	   ,(loop-generator-base type index-order place clause :testp testp :tight-iloop t)
	   ,(loop-generator-base type index-order place clause :testp testp :tight-iloop nil)))))

(defmacro einstein-sum (type idx-order place clause &optional (testp t))
  (loop-generator type idx-order place clause :testp testp))

;;Yes this has an overhead, but if you're *really* worried about efficiency then roll your custom loops
;;with einstein-sum-base. This is a super-adaptive on-the-fly-loop-generating-function generating
;;macro. You have the power now, without any of the tedium :)
(defmacro lambda-einstein (args (type place clause &optional (testp t)))
  (multiple-value-bind (refs tlist indices) (parse-loopx type place clause)
    (declare (ignore refs))
    (let ((tens (mapcar #'(lambda (x) (second (getf x :tensor))) tlist)))
      (assert (subsetp tens args) nil "Error args and the list of tensor do not match.")
      (with-gensyms (functable)
	`(let ((,functable (make-hash-table :test 'equal)))
	   (lambda (,@args)
	     (declare (type ,type ,@tens))
	     (let* ((idx-ord (mapcar #'car (very-quickly (sort (list ,@(mapcar #'(lambda (idx) `(list ',(car idx) (+ ,@(mapcar #'(lambda (x) `(aref (the index-store-vector (strides ,(car x))) ,(cadr x))) (cdr idx))))) indices)) #'(lambda (a b) (declare (type index-type a b)) (> a b)) :key #'cadr))))
		    (func (or (gethash idx-ord ,functable)
			      (let* ((code (loop-generator ',type idx-ord ',place ',clause :testp ,testp))
				     (funcnew (compile-and-eval
					       (list 'lambda '(,@args) code))))
				(format t "Compiling code for index-order : ~a~%" idx-ord)
				(setf (gethash idx-ord ,functable) funcnew)
				funcnew))))
	       (apply func (list ,@args)))))))))
