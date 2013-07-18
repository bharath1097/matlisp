(in-package :matlisp)

(defparameter *contract-ops* '(sum))

;;(defparameter *tgemv* '(contract (ref y i) (+ (* alpha (sum (k) (ref A i k) (ref x k))) (* beta (ref y i)))))

(defparameter *tclause* '(einstein-sum (ref C i j) (* (ref A i k) (ref B k j))))

(defun get-cons (lst sym)
  (if (atom lst) nil
      (if (eq (car lst) sym)
	  (list lst)
	  (append (get-cons (car lst) sym) (get-cons (cdr lst) sym)))))

(defun get-repeats (lst)
  (do ((tmp lst (cdr tmp))
       (ret nil (if (and (not (member (car tmp) ret)) (member (car tmp) (cdr tmp)))
		    (cons (car tmp) ret)
		    ret)))
      ((null tmp) ret)))

(defun gensym-list (n)
  (loop :repeat n :collect (gensym)))

#+nil
(defun loop-gen (idx ret einx)
  (if (null idx) code
      (destructuring-bind (var repeat) (car idx)
	`(loop :for ,var :of-type index-type :from ,start :below ,end
	    :do ,(loop-gen (cdr idx) code)))))

(tensor-args (get-cons (cddr clause) 'ref))
	 (code-idx (get-repeats (apply #'append (mapcar #'cddr tensor-args))))
	 (arg-idx (let ((ret nil))
		    (mapcar #'(lambda (x) (if (symbolp x) (setf ret (setadd ret x)))) (cddr arg))
		    ret))
	 (idxs (append arg-idx code-idx))
	 (dims (apply #'append (mapcar #'(lambda (x) (loop :for idx :in (cddr x)
							:counting t :into i
							:when (member idx idxs)
							:collect `(,idx (aref (dimensions ,(cadr x)) ,(1- i))))) tensor-args)))

	 (osyms (zipsym (mapcar #'(lambda (x) `(head ,(car x))) tsyms)))
	 (stosyms (zipsym (mapcar #'(lambda (x) `(store ,(car x))) tsyms)))
    	 (stdsyms (zipsym (mapcar #'(lambda (x) `(strides ,(car x))) tsyms)))
	 (dimsyms (zipsym (mapcar #'(lambda (x) `(dimensions ,(car x))) tsyms))))


(defun mapcons (func lst keys)
  (cond
    ((atom lst) lst)
    ((member (car lst) keys)
     (funcall func lst))
    (t
     (mapcar #'(lambda (x) (mapcons func x keys)) lst))))

(mapcons #'(lambda (x) `(aref (store ,(cadr x)) ,@(cddr x)))
	 *tclause* '(ref))

	       
	     (loopgen (idxs cclause place &optional (start? t))		
		`(loop
		    :with ... :of-type index-type := ...
		    :with ... :of-type index-type := ...
		    :for (car idxs) :of-type index-type :from 0 :below 
		
		))


(defun loop-generator (type clause &optional (testp t))
  (let* ((ten-syms (mapcar #'(lambda (x)
			    (let* ((sym (second x))
				   (gsym (gensym (symbol-name sym))))
			      `((,gsym ,sym)
				(,(gensym (string+ "head-" (symbol-name sym))) (head ,gsym))
				(,(gensym (string+ "store-" (symbol-name sym))) (store ,gsym))
				(,(gensym (string+ "strides-" (symbol-name sym))) (strides ,gsym))
				(,(gensym (string+ "dimensions-" (symbol-name sym))) (dimensions ,gsym)))))
			   (get-cons (cdr clause) 'ref)))
	 (offsets nil)
	 (ranges nil))
    (labels ((get-plst (x)
	       (find x ten-syms :key #'cadar :test #'eql))
	     (get-offset (x)
	       (let ((ofst (find x offsets :key #'cadr :test #'equal)))
		 (if ofst
		     (car ofst)
		     (let ((ofsym (gensym (string+ "offset-" (symbol-name (car x))))))
		       (push (list ofsym x) offsets)
		       ofsym))))
	     (testgen ()
	       (let ((dims (apply #'append (mapcar #'(lambda (x) (loop :for ele :in (cdr (cadr x))
								    :counting t :into i
								    :collect (let ((plst (get-plst (car (cadr x)))))
									       `(,ele (aref ,(car (elt plst 4)) ,(1- i)))))) offsets))))
		 (loop :for ele :in dims
		    :do (let ((cdim (find (car ele) ranges :key #'car :test #'eql)))
			  (if cdim
			      (rplacd (last cdim) (cdr ele))
			      (push ele ranges))))
		 (when testp
		   `((assert (and ,@(mapcar #'(lambda (x) `(= ,@(cdr x))) ranges)) nil "error: arguments are not of appropriate sizes.")))))
	     (loopgen (idxs place clause &optional (startp t))
		(let ((cidx (caar idxs)))
		  `((let*-typed (,@(remove-if #'null
					      (mapcar #'(lambda (x)
							  (if (or (member cidx (cdr (cadr x))) startp)
							      (let ((offset (gensym (string+ "of-" (symbol-name cidx) "-" (symbol-name (car (cadr x)))))))
								`(:with ,offset :of-type index-type := ...
									:for ,(car x) :of-type index-type := ,(if startp
														  (let ((plst (get-plst (car (cadr x)))))
														    (car (elt plst 1)))
														  (car x))
									:then (the index-type (+ ,offset ,(car x))))
								nil))
							  offsets)))

		    (loop
		       :for ,cidx :of-type index-type :from 0 :below ,(cadr (car idxs))
		       ,@(apply #'append (remove-if #'null (mapcar #'(lambda (x)								       
								       (if (or (member cidx (cdr (cadr x))) startp)
									   (let ((offset (gensym (string+ "of-" (symbol-name cidx) "-" (symbol-name (car (cadr x)))))))
									     `(:with ,offset :of-type index-type := ...
									       :for ,(car x) :of-type index-type := ,(if startp
															 (let ((plst (get-plst (car (cadr x)))))
															   (car (elt plst 1)))
															 (car x))
										    :then (the index-type (+ ,offset ,(car x))))
									   nil))
								   offsets)))
				)))))
      (let* ((cclause (mapcons #'(lambda (x)
				   (let* ((plst (get-plst (cadr x)))
					  (ofset-sym (get-offset (cdr x))))
				     `(t/store-ref ,type ,(caaddr plst) ,ofset-sym)))
			       clause '(ref))))
	`(let (,@(mapcar #'car ten-syms))
	   (declare (type ,type ,@(mapcar #'caar ten-syms)))
	   (let (,@(apply #'append (mapcar #'cdr ten-syms)))
	     (declare (type index-type ,@(mapcar #'caadr ten-syms))
		      (type ,(store-type type) ,@(mapcar #'caaddr ten-syms))
		      (type index-store-vector ,@(mapcar #'car (apply #'append (mapcar #'cdddr ten-syms)))))
	     ,@(testgen)
	     ,@(loopgen ranges (cadr cclause) (caddr cclause) t)))))))
	

(loop-generator 'real-tensor *tclause*)
