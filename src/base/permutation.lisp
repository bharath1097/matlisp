(in-package #:matlisp)

;;This must match the type used in LAPACK
(deftype perrepr-type ()
  '(unsigned-byte 32))

(deftype perrepr-vector (&optional (size '*))
  `(simple-array perrepr-type (,size)))

(make-array-allocator allocate-perrepr-store 'perrepr-type 0
  "
  Syntax
  ======
  (ALLOCATE-PERREPR-STORE SIZE [INITIAL-ELEMENT 0])

  Purpose
  =======
  Allocates integer4 (32-bits) storage.")
;;

(defun perrepr-id-action (n)
  (declare (type fixnum n))
  (let ((ret (allocate-perrepr-store n)))
    (declare (type perrepr-vector ret))
    (very-quickly
      (loop
	 for i of-type perrepr-type from 0 below n
	 do (setf (aref ret i) i)))
    ret))

(definline perrepr-max (seq)
  (declare (type perrepr-vector seq))
  (very-quickly (reduce #'max seq)))

(definline perrepr-min (seq)
  (declare (type perrepr-vector seq))
  (very-quickly (reduce #'min seq)))

(definline perv (&rest contents)
  (make-array (length contents) :element-type 'perrepr-type :initial-contents contents))

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

(defmethod print-object ((per permutation) stream)
  (print-unreadable-object (per stream :type t)
    (format stream "S_~a~%" (group-rank per))
    (if (zerop (group-rank per))
	(format stream "ID~%")
	(format stream "~a~%" (repr per)))))
;;

(defclass permutation-action (permutation)
  ((representation :type perrepr-vector)))


(defmethod initialize-instance :after ((per permutation-action) &rest initargs)
  (declare (ignore initargs))
  (labels ((action-repr-p (act)
	     "
  Checks if ARR is a possible permutation vector.  A permutation pi
  is characterized by a vector containing the indices from 0,...,
  @function{length}(@arg{perm})-1 in some order.
"
	     (if (not (typep act 'perrepr-vector)) nil
		 (locally
		     (declare (type perrepr-vector act))
		   (let* ((len (length act))
			  (sort (very-quickly (sort (copy-seq act) #'<))))
		     (declare (type perrepr-vector sort)
			      (type index-type len))
		     (very-quickly
		       (loop for i of-type index-type from 0 below len
			  unless (= (aref sort i) i)
			  do (return nil)
			  finally (return t))))))))
    (assert (action-repr-p (repr per)) nil 'permutation-invalid-error)
    (setf (group-rank per) (length (repr per)))))
;;

(defclass permutation-cycle (permutation)
  ((representation :type list)))

(defmethod initialize-instance :after ((per permutation-cycle) &rest initargs)
  (declare (ignore initargs))
  (labels ((cycle-repr-p (perm)
	     "
  Does a sorting operation to check for duplicate elements in
  the cycle representation of a permutation.
"
	     (if (not (typep perm 'perrepr-vector)) nil
		 (locally
		     (declare (type perrepr-vector perm))
		   (let ((len (length perm)))
		     (declare (type index-type len))
		     (if (<= len 1) nil
			 (let ((sort (very-quickly (sort (copy-seq perm) #'<))))
			   (declare (type perrepr-vector sort))
			   (very-quickly
			     (loop for i of-type index-type from 1 below len
				when (= (aref sort i) (aref sort (1- i)))
				do (return nil)
				finally (return t))))))))))
    (if (null (repr per)) (setf (group-rank per) 0)
	(very-quickly
	  (loop
	     for cyc of-type perrepr-vector in (repr per)
	     unless (cycle-repr-p cyc)
	     do (error 'permutation-invalid-error)
	     maximizing (perrepr-max cyc) into g-rnk of-type index-type
	     finally (setf (group-rank per) (the index-type (1+ g-rnk))))))))

;;

(defclass permutation-pivot-flip (permutation)
  ((representation :type perrepr-vector)))

(defmethod initialize-instance :after ((per permutation-pivot-flip) &rest initargs)
  (declare (ignore initargs))
  (labels ((pivot-flip-p (idiv)
	     "
  Checks if ARR is a possible permutation pivot-flip. A pivot
  flip is more algorithmic in its representation. If a sequence
  is given, apply a pivot-flip on it is equivalent to running from
  the left to the right of the permutation by flipping (pi(i), i)
  sequentially.
"
	     (if (not (typep idiv 'perrepr-vector)) nil
		 (let ((len (length idiv)))
		   (declare (type perrepr-vector idiv)
			    (type index-type len))
		   (very-quickly
		     (loop for i of-type index-type from 0 below len
			do (unless (< -1 (aref idiv i) len)
			     (return nil))
			finally (return t)))))))
    (assert (pivot-flip-p (repr per)) nil 'permutation-invalid-error)
    (setf (group-rank per) (length (repr per)))))

;;
(definline make-pcycle (&rest args)
  (make-instance 'permutation-cycle :repr args))

(definline make-paction (pact)
  (make-instance 'permutation-action :repr pact))

(definline make-pidx (pact)
  (make-instance 'permutation-pivot-flip :repr pact))

;;Generic permute! method.
(defgeneric permute! (seq perm)
  (:documentation "
  (permute! seq perm)

  Applies the permutation on the sequence.
")
  (:method :before ((seq sequence) (perm permutation))
	   (let ((len (length seq)))
	     (assert (>= len (group-rank perm)) nil
		     'permutation-permute-error :seq-len len :group-rank (group-rank perm)))))

(definline permute (seq perm)
  (permute! (copy-seq seq) perm))

;;Action
(defmethod permute! ((seq cons) (perm permutation-action))
  (let ((cseq (make-array (length seq) :initial-contents seq))
	(act (repr perm))
	(glen (group-rank perm)))
    (mapl
     (let ((i 0))
       (declare (type fixnum i))
       (lambda (x)
	 (when (< i glen)
	   (rplaca x (aref cseq (aref act i)))
	   (incf i)))) seq)))
    
(defmethod permute! ((seq vector) (perm permutation-action))
  (let ((cseq (make-array (length seq) :initial-contents seq))
	(act (repr perm)))
      (loop
	 for i from 0 below (group-rank perm)
	 do (unless (= i (aref act i))
	      (setf (aref seq i) (aref cseq (aref act i))))
	 finally (return seq))))

;;Cycle
;;Might be useful ?
(defun apply-cycle! (seq pcyc)
  (declare (type perrepr-vector pcyc)
	   (type vector seq))
  (let ((xl (aref seq (aref pcyc (1- (length pcyc))))))
    (loop for i of-type index-type downfrom (1- (length pcyc)) to 0
       do (setf (aref seq (aref pcyc i))
		(if (= i 0) xl
		    (aref seq (aref pcyc (1- i))))))))

(defmethod permute! ((seq cons) (perm permutation-cycle))
  (let ((cseq (make-array (length seq) :initial-contents seq))
	(glen (group-rank perm)))
    (dolist (cyc (repr perm))
      (declare (type perrepr-vector cyc))
      (apply-cycle! cseq cyc))
    (mapl
     (let ((i 0))	 
       (lambda (x)
	 (when (< i glen)
	   (rplaca x (aref cseq i))
	   (incf i)))) seq)))

(defmethod permute! ((seq vector) (perm permutation-cycle))
  (dolist (cyc (repr perm) seq)
    (declare (type perrepr-vector cyc))
    (apply-cycle! seq cyc)))

;;Pivot idx
(defmethod permute! ((seq vector) (perm permutation-pivot-flip))
  (let-typed ((pidx (repr perm) :type perrepr-vector))
     (loop for i of-type index-type from 0 below (group-rank perm)
	unless (= i (aref pidx i))
	do (rotatef (aref seq i) (aref seq (aref pidx i)))
	finally (return seq))))
  
(defmethod permute! ((seq cons) (perm permutation-pivot-flip))
  (let ((cseq (make-array (length seq) :initial-contents seq))
	(glen (group-rank perm)))
    (permute! cseq perm)
    (mapl
     (let ((i 0))
       (lambda (x)
	 (when (< i glen)
	   (rplaca x (aref cseq i))
	   (incf i)))) seq)))

;;Conversions----------------------------------------------------;;
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
   ((arr (repr act) :type perrepr-vector))
   (labels ((find-cycle (x0)
	      ;; This function obtains the cycle starting from x_0.
	      (declare (type perrepr-type x0))
	      (if (= (aref arr x0) x0) (values 0 nil)
		  (very-quickly
		    (loop
		       for x of-type perrepr-type = (aref arr x0) then (aref arr x)
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
			   (cons (make-array clen :element-type 'perrepr-type :initial-contents clst) cyc))
		       (nconc ignore (if (= clen 0) (list x0) clst))))))))
     (let ((cyc-lst (cycle-walk nil nil)))
       (make-instance 'permutation-cycle
		      :repr cyc-lst)))))

(defun cycle->action (cyc)
  "
   (cycle->action cyc)

   This function obtains the action representation of a permutation
   from the cyclic one. The first argument \"cyc\" is the cyclic
   representation of the permutation: an object of the class
   permutation-cycle.
"
  (declare (type permutation-cycle cyc))
  (let ((act-repr (perrepr-id-action (group-rank cyc)))
	(cycs-repr (repr cyc)))
    (declare (type perrepr-vector act-repr))
    (dolist (cyc cycs-repr)
      (declare (type perrepr-vector cyc))
      (let ((xl (aref act-repr (aref cyc (1- (length cyc))))))
	(very-quickly
	  (loop
	     for i of-type index-type downfrom (1- (length cyc)) to 0
	     do (setf (aref act-repr (aref cyc i))
		      (if (= i 0) xl
			  (aref act-repr (aref cyc (1- i)))))))))
    (make-instance 'permutation-action :repr act-repr)))

(defun pivot-flip->action (pflip)
  (declare (type permutation-pivot-flip pflip))
  (let* ((idiv (repr pflip))
	 (len (length idiv)))
    (declare (type perrepr-vector idiv)
	     (type index-type len))
    (let ((act (perrepr-id-action len)))
      (declare (type perrepr-vector act))
      (very-quickly
	(loop for i from 0 below len
	   do (let ((val (aref idiv i)))
		(unless (= val i)
		  (rotatef (aref act i) (aref act val))))))
      (make-instance 'permutation-action :repr act))))

;; (defun cycle->pivot-flip (cycs)
;;   (declare (type permutation-cycle cycs)))
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

;;Optimize: pick different pivot.
(defgeneric sort-permute (seq predicate)
  (:documentation "
  (sort-permute seq predicate)

  Sorts the given sequence and return
  the permutation required to move
  from the given sequence to the sorted form.
  "))

(defun idx-sort-permute (seq predicate)
  "
  (sort-permute seq predicate)

  Sorts a index-array and also returns
  the permutation-action required to move
  from the given sequence to the sorted form.

  Takes about 10x the running time which can be
  achieved with cl:sort.
  "
  (declare (type index-store-vector seq)
	   (type function predicate))
  (let* ((len (length seq))
	 (perm (perrepr-id-action len)))
    (declare (type index-type len)
	     (type perrepr-vector perm))
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

;;Add a general sorter, this is a very useful thing to have.
;;Add a function to apply permutations to a matrices, tensors.
