(in-package #:matlisp)

(defun id-action-repr (n)
  (declare (type fixnum n))
  (let ((ret (allocate-index-store n)))
    (declare (type (index-array *) ret))
    (very-quickly
      (loop
	 for i of-type index-type from 0 below n
	 do (setf (aref ret i) i)))
    ret))

(definline idx-max (seq)
  (declare (type (index-array *) seq))
  (reduce #'max seq))

(definline idx-min (seq)
  (declare (type (index-array *) seq))
  (reduce #'min seq))

(defun idx= (a b)
  (declare (type (index-array *) a b))
  (when (= (length a) (length b))
    (very-quickly
      (loop
	 for ele-a across a
	 for ele-b across b
	 unless (= ele-a ele-b)
	 do (return nil)
	 finally (return t)))))

(definline idx->list (a)
  (declare (type (index-array *) a))
  (loop for ele across a
       collect ele))

;;Write a uniform randomiser
(defun seqrnd (seq)
  "Randomize the elements of a sequence. Destructive on SEQ."
  (sort seq #'> :key #'(lambda (x) (declare (ignore x))
			       (random 1.0))))

;;Class definitions----------------------------------------------;;
(defclass permutation ()
  ((representation :accessor repr
		   :initarg :repr)
   (group-rank :accessor group-rank
	       :type index-type)))

(defparameter +permutation-identity+
  (let ((ret (make-instance 'permutation :repr :id)))
    (setf (group-rank ret) 0)
    ret))

(defmethod print-object ((per permutation) stream)
  (print-unreadable-object (per stream :type t)
    (format stream "S_~a~%~a~%" (group-rank per) (repr per))))
;;
(defclass permutation-cycle (permutation)
  ((representation :type cons)))

(defun cycle-repr-p (perm)
  "
  Does a sorting operation to check for duplicate elements in
  the cycle representation of a permutation.
"
  (if (not (typep perm '(index-array *))) nil
      (locally
      	  (declare (type (index-array *) perm))
	(let ((len (length perm)))
	  (declare (type index-type len))
	  (if (<= len 1) nil
	      (let ((sort (very-quickly (sort (copy-seq perm) #'<))))
		(declare (type (index-array *) sort))
		(very-quickly
		  (loop for i of-type index-type from 1 below len
		     when (= (aref sort i) (aref sort (1- i)))
		     do (return nil)
		     finally (return t)))))))))

(defmethod initialize-instance :after ((per permutation-cycle) &rest initargs)
  (declare (ignore initargs))
  (very-quickly
    (loop
       for cyc of-type (index-array *) in (repr per)
       unless (cycle-repr-p cyc)
       do (error 'permutation-invalid-error)
       maximizing (idx-max cyc) into g-rnk of-type index-type
       finally (setf (group-rank per) (the index-type (1+ g-rnk))))))

(definline make-pcycle (&rest args)
  (make-instance 'permutation-cycle :repr args))

;;
(defclass permutation-action (permutation)
  ((representation :type (index-array *))))

(defun action-repr-p (act)
  "
  Checks if ARR is a possible permutation vector.  A permutation pi
  is characterized by a vector containing the indices from 0,...,
  @function{length}(@arg{perm})-1 in some order.
"
  (if (not (typep act '(index-array *))) nil
      (locally
	  (declare (type (index-array *) act))
	(let* ((len (length act))
	       (sort (very-quickly (sort (copy-seq act) #'<))))
	  (declare (type (index-array *) sort)
		   (type index-type len))
	  (very-quickly
	    (loop for i of-type index-type from 0 below len
	       unless (= (aref sort i) i)
	       do (return nil)
	       finally (return t)))))))

(defmethod initialize-instance :after ((per permutation-action) &rest initargs)
  (declare (ignore initargs))
  (let ((act (repr per)))
    (declare (type (index-array *) act))
    (unless (action-repr-p act)
      (error 'permutation-invalid-error))
    (setf (group-rank per) (1+ (idx-max act)))))

(definline make-paction (pact)
  (make-instance 'permutation-action :repr pact))

;;Conversions and validation-------------------------------------;;
(defun action->cycle (act)
  "
  (action->cycle act)

  This function obtains the canonical cycle representation
  of a permutation. The first argument \"act\" is the action of the
  permutation on the array #(0 1 2 3 ..): an object of the class
  permutation-action.
   
  \"Canonical\" may be a bit of an overstatement; this is the way
  S_n was presented in Van der Waerden's book.
"
  (declare (type permutation-action act))
  (mlet*
   ((arr (repr act) :type (index-array *)))
   (labels ((find-cycle (x0)
	      ;; This function obtains the cycle starting from x_0.
	      (declare (type index-type x0))
	      (if (= (aref arr x0) x0) (values 0 nil)
		  (very-quickly
		    (loop
		       for x of-type index-type = (aref arr x0) then (aref arr x)
		       and ret of-type cons = (list x0) then (cons x ret)
		       counting t into i of-type index-type
		       when (= x x0)
		       do (return (values i ret))))))
	    (cycle-walk (cyc ignore)
	      ;; Finds all cycles
	      (let ((x0 (find-if-not #'(lambda (x) (member x ignore)) arr)))
		(if (null x0)
		    cyc
		    (multiple-value-bind (clen clst) (find-cycle x0)
		      (declare (type index-type clen)
			       (type list clst))
		      (cycle-walk
		       (if (= clen 0) cyc
			   (cons (make-array clen :element-type 'index-type :initial-contents clst) cyc))
		       (nconc ignore (if (= clen 0) (list x0) clst))))))))
     (let ((cyc-lst (cycle-walk nil nil)))
       (if (null cyc-lst)
	   +permutation-identity+
	   (make-instance 'permutation-cycle
			  :repr cyc-lst))))))

(defun cycle->action (cyc)
  "
   (cycle->action cyc)

   This function obtains the action representation of a permutation
   from the cyclic one. The first argument \"cyc\" is the cyclic
   representation of the permutation: an object of the class
   permutation-cycle.
"
  (declare (type permutation-cycle cyc))
  (let ((act-repr (id-action-repr (group-rank cyc)))
	(cycs-repr (repr cyc)))
    (declare (type (index-array *) act-repr))
    (dolist (cyc cycs-repr)
      (declare (type (index-array *) cyc))
      (let ((xl (aref act-repr (aref cyc (1- (length cyc))))))
	(very-quickly
	  (loop
	     for i of-type index-type downfrom (1- (length cyc)) to 0
	     do (setf (aref act-repr (aref cyc i))
		      (if (= i 0) xl
			  (aref act-repr (aref cyc (1- i)))))))))
    (make-instance 'permutation-action :repr act-repr)))

;;
(defgeneric permute! (seq perm)
  (:documentation "
  (permute! seq perm)

  Applies the permutation on the sequence.
")
  (:method ((seq sequence) (perm (eql +permutation-identity+)))
    seq))

(defmethod permute! ((seq sequence) (perm permutation-cycle))
  (labels ((apply-cycle! (seq pcyc)
	     (declare (type (index-array *) pcyc))
	     (very-quickly
	       (let ((xl (aref seq (aref pcyc (1- (length pcyc))))))
		 (loop for i of-type index-type downfrom (1- (length pcyc)) to 0
		    do (setf (aref seq (aref pcyc i))
			     (if (= i 0) xl
				 (aref seq (aref pcyc (1- i))))))))))
    (let ((len (length seq))
	  (glen (group-rank perm))
	  (cycs-lst (repr perm)))
      (declare (type index-type len glen))
      (if (< len glen) (error 'permutation-permute-error :seq-len len :group-rank glen)
	  (etypecase seq
	    (vector
	     (dolist (cyc cycs-lst seq)
	       (declare (type (index-array *) cyc))
	       (apply-cycle! seq cyc)))
	    (cons
	     (let ((cseq (make-array len :initial-contents seq)))
	       (declare (type (simple-vector *) cseq))
	       (dolist (cyc cycs-lst)
		 (declare (type (index-array *) cyc))
		 (apply-cycle! cseq cyc))
	       (mapl
		(let ((i 0))
		  (declare (type fixnum i))
		  (lambda (x)
		    (when (< i glen)
		      (rplaca x (aref cseq i))
		      (incf i))))
		seq))))))))

(defmethod permute! ((seq sequence) (perm permutation-action))
  (let ((len (length seq))
	(glen (group-rank perm)))
    (declare (type index-type len glen))
    (if (< len glen) (error 'permutation-permute-error :seq-len len :group-rank glen)
	(let ((cseq (make-array len :initial-contents seq))
	      (act (repr perm)))
	  (declare (type (simple-vector *) cseq)
		   (type (index-array *) act))
	  (etypecase seq
	    (vector
	     (very-quickly
	       (loop
		  for i of-type index-type from 0 below glen
		  do (unless (= i (aref act i))
		       (setf (aref seq i) (aref cseq (aref act i))))
		  finally (return seq))))
	    (cons
	     (mapl
	      (let ((i 0))
		(declare (type fixnum i))
		(lambda (x)
		  (when (< i glen)
		    (rplaca x (aref cseq (aref act i)))
		    (incf i))))
	      seq)))))))

(defun permute (seq perm)
  (declare (type sequence seq)
	   (type permutation perm))
  (let ((cseq (copy-seq seq)))
    (permute! cseq perm)))
;;
#+nil
(defun permute-argument (func-symbol perm)
  (declare (type symbol func-symbol)
	   (type permutation perm))
  (let* ((glen (group-rank perm))
	 (args (loop for i from 0 below glen
		  collect (gensym))))
    (eval `(lambda (,@args &rest rest)
	     (apply ',func-symbol (append (list ,@(permute! args perm)) rest))))))

(defun argument-permute (func perm)
  (declare (type function func)
	   (type permutation perm))
  (lambda (&rest args)
    (apply func (permute! args perm))))

(defun curry (func perm &rest curried-args)
  (declare (type function func)
	   (type permutation perm))
  (lambda (&rest args)
    (apply func (permute! (append curried-args args) perm))))

(defun compose (func-a func-b perm)
  (declare (type function func-a func-b)
	   (type permutation perm))
  (lambda (&rest args)
    (apply func-a (permute! (multiple-value-list (funcall func-b args)) perm))))
;;

(defun idx-sort-permute (seq predicate)
  "
  (sort-permute seq predicate)

  Sorts a index-array and also returns
  the permutation-action required to move
  from the given sequence to the sorted form.

  Takes about 10x the running time which can be
  achieved with cl:sort.
  "
  (declare (type (index-array *) seq)
	   (type function predicate))
  (let* ((len (length seq))
	 (perm (id-action-repr len)))
    (declare (type index-type len)
	     (type (index-array *) perm))
    (labels ((qsort-bounds (todo)
	       (declare (type list todo))
	       (if (null todo) t
		   (destructuring-bind (lb ub) (pop todo)
		     (declare (type index-type lb ub))
		     #+nil(format t "~a lb:~a ub:~a ~%" seq lb ub)
		     (if (= ub (1+ lb)) t
			 (let* ((ele (aref seq lb))
				(ele-idx (very-quickly
					   (loop
					      for i of-type index-type from (1+ lb) below ub
					      with ele-idx of-type index-type = lb
					      do (unless (funcall predicate ele (aref seq i))
						   (when (> i (1+ ele-idx))
						     (rotatef (aref seq ele-idx) (aref seq (1+ ele-idx)))
						     (rotatef (aref perm ele-idx) (aref perm (1+ ele-idx))))
						   (rotatef (aref seq ele-idx) (aref seq i))
						   (rotatef (aref perm ele-idx) (aref perm i))
						   (incf ele-idx)
						   #+nil(format t "       ~a ~%" seq))
					      finally (return ele-idx)))))
			   ;;The things we do for tail recursion!
			   (when (> (- ub ele-idx) 2)
			     (push (list (1+ ele-idx) ub) todo))
			   (when (> (- ele-idx lb) 1)
			     (push (list lb ele-idx) todo))
			   (qsort-bounds todo)))))))
      (qsort-bounds `((0 ,len)))
      (values seq (action->cycle (make-paction perm))))))
