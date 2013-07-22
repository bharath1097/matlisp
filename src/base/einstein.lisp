(in-package :matlisp)

(defun get-cons (lst sym)
  (if (atom lst) nil
      (if (eq (car lst) sym)
	  (list lst)
	  (append (get-cons (car lst) sym) (get-cons (cdr lst) sym)))))

(defun has-sym (lst sym)
  (if (atom lst) (eql lst sym)
      (or (has-sym (car lst) sym) (has-sym (cdr lst) sym))))

(defun mapcons (func lst keys)
  (if (atom lst) lst
      (let ((tlst (if (member (car lst) keys)
			(funcall func lst)
			lst)))
	(if (atom tlst) tlst
	    (mapcar #'(lambda (x) (mapcons func x keys)) tlst)))))

(defun loop-generator (type index-order place clause &key (testp t) (tight-iloop nil))
  (let* ((refs (let ((tmp (get-cons (list place clause) 'ref))
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
		    (loop :for idx :in tmp 
		       :do (assert (member (car idx) index-order) nil  "Error index ~a not found in the index-order." (car idx)))
		    (loop :for idx :in index-order
		       :collect (let ((cdim (find idx tmp :key #'car)))
				  (assert (not (null cdim)) nil "Error index ~a not found in the expression." idx)
				  cdim))))
	 (idx-d (let ((refrem (mapcons #'(lambda (x) (declare (ignore x)) t)
				       clause '(ref))))
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
			    (get-incs nil acc (cons nil decl)
				      (cons `(setf ,(caar (cadr ofst)) ,(car (get-prop ten :head))) incs)
				      ten ofst))
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
					      `(= ,@(mapcar #'(lambda (x) `(aref ,(car (get-prop (car x) :dimensions)) ,(cadr x))) (cdr idx)))) indices))
			   nil "error: arguments are not of appropriate sizes.")
		   ,@(when tight-iloop
			   `((assert (= 1 ,@(mapcar #'(lambda (x) `(aref ,(car (get-prop (car x) :strides)) ,(cadr x))) (cdar (last indices)))) nil "error: Inner loop strides are not 1.")))))
	       (t/compile (place clause)
		 (let* ((cclause (mapcons #'(lambda (x)	     
					      (let* ((plst (get-prop (cadr x)))
						     (ofset-sym (get-offset (cdr x))))
						`(t/store-ref ,type ,(car (getf plst :store)) ,ofset-sym)))
					  clause '(ref)))
			(ftype (field-type type)))
		   (setf cclause
			 (mapcons #'(lambda (x)
				      (let ((op (car x)))
					`(,(case op (* 't/f*) (+ 't/f+) (- 't/f-) (/ 't/f/))
					   ,ftype
					   ,@(cdr x))))
				  cclause '(* + - /)))
		   (let ((plst (get-prop (cadr place)))
			 (valsym (gensym "value")))
		     `((let-typed ((,valsym (t/f+ ,ftype (t/store-ref ,type ,(car (getf plst :store)) ,(get-offset (cdr place)))
						  ,cclause ) :type ,ftype))
				  (t/store-set ,type ,valsym ,(car (getf plst :store)) ,(get-offset (cdr place))))))))
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
										     (setf clause (mapcons #'(lambda (x)
													       (if (and (eql (cadr x) ten) (equal (cddr x) (car ofs)))
														   (car decl)
														   x))
													   clause '(ref)))
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
	 ,(cadr place))))))

(defmacro einstein-sum (type idx-order place clause &optional (tightp nil))
  (loop-generator type idx-order place clause :tight-iloop tightp))
