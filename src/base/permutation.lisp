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
	 :for i :of-type perrepr-type :from 0 :below n
	 :do (setf (aref ret i) i)))
    ret))

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
		       (loop :for i :of-type index-type :from 0 :below len
			  :unless (= (aref sort i) i)
			  :do (return nil)
			  :finally (return t))))))))
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
			     (loop :for i :of-type index-type :from 1 :below len
				:when (= (aref sort i) (aref sort (1- i)))
				:do (return nil)
				:finally (return t))))))))))
    (if (null (repr per)) (setf (group-rank per) 0)
	(very-quickly
	  (loop
	     :for cyc :of-type perrepr-vector :in (repr per)
	     :unless (cycle-repr-p cyc)
	     :do (error 'permutation-invalid-error)
	     :maximizing (lvec-max cyc) into g-rnk of-type index-type
	     :finally (setf (group-rank per) (the index-type (1+ g-rnk))))))))

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
  sequentially. This is the representation used in LAPACK.
"
	     (if (not (typep idiv 'perrepr-vector)) nil
		 (let ((len (length idiv)))
		   (declare (type perrepr-vector idiv)
			    (type index-type len))
		   (very-quickly
		     (loop :for i :of-type index-type :from 0 :below len
			:do (unless (< -1 (aref idiv i) len)
			      (return nil))
			:finally (return t)))))))
    (assert (pivot-flip-p (repr per)) nil 'permutation-invalid-error)
    (setf (group-rank per) (length (repr per)))))

;;
;; (defclass permutation-matrix (permutation)
;;   ((representation :type perrepr-vector)))

;; (defmethod initialize-instance :after ((per permutation-pivot-flip) &rest initargs)
;;   (declare (ignore initargs))
;;   (labels ((pivot-flip-p (idiv)
;;	     "
;;   Checks if ARR is a possible permutation pivot-flip. A pivot
;;   flip is more algorithmic in its representation. If a sequence
;;   is given, apply a pivot-flip on it is equivalent to running from
;;   the left to the right of the permutation by flipping (pi(i), i)
;;   sequentially. This is the representation used in LAPACK.
;; "
;;	     (if (not (typep idiv 'perrepr-vector)) nil
;;		 (let ((len (length idiv)))
;;		   (declare (type perrepr-vector idiv)
;;			    (type index-type len))
;;		   (very-quickly
;;		     (loop :for i :of-type index-type :from 0 :below len
;;			:do (unless (< -1 (aref idiv i) len)
;;			      (return nil))
;;			:finally (return t)))))))
;;     (assert (pivot-flip-p (repr per)) nil 'permutation-invalid-error)
;;     (setf (group-rank per) (length (repr per)))))


;;
(definline make-pcycle (&rest args)
  (make-instance 'permutation-cycle :repr args))

(definline make-paction (pact)
  (make-instance 'permutation-action :repr pact))

(definline make-pidx (pact)
  (make-instance 'permutation-pivot-flip :repr pact))

;;Generic permute! method.
(defgeneric permute! (thing permutation &optional argument)
  (:documentation "
  (permute! thing permutation [argument 0])

  Permutes the ARGUMENT index of the the array-like object THING, by
  applying PERMUTATION on it.")
  (:method :before ((seq sequence) (perm permutation) &optional (arg 0))
	   (declare (ignore arg))
	   (let ((len (length seq)))
	     (assert (>= len (group-rank perm)) nil
		     'permutation-permute-error :seq-len len :group-rank (group-rank perm))))
  (:method :before ((ten standard-tensor) (perm permutation) &optional (arg 0))
	   (let ((len (aref (dimensions ten) arg)))
	     (assert (>= len (group-rank perm)) nil
		     'permutation-permute-error :seq-len len :group-rank (group-rank perm)))))

(definline permute (thing perm &optional (arg 0))
  (permute! (copy thing) perm arg))

;;Action
(defmethod permute! ((seq cons) (perm permutation-action) &optional arg)
  (declare (ignore arg))
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

(defmethod permute! ((seq vector) (perm permutation-action) &optional arg)
  (declare (ignore arg))
  (let ((cseq (make-array (length seq) :initial-contents seq))
	(act (repr perm)))
    (loop
       :for i :from 0 :below (group-rank perm)
       :do (unless (= i (aref act i))
	     (setf (aref seq i) (aref cseq (aref act i))))
       :finally (return seq))))

(defmethod permute! ((ten standard-tensor) (perm permutation-action) &optional (arg 0))
  (let ((cyc (action->cycle perm)))
    (permute! ten cyc arg)))

;;Cycle
;;Might be useful ?
(defun apply-cycle! (seq pcyc)
  (declare (type perrepr-vector pcyc)
	   (type vector seq))
  (let ((xl (aref seq (aref pcyc (1- (length pcyc))))))
    (loop :for i :of-type index-type :downfrom (1- (length pcyc)) :to 0
       :do (setf (aref seq (aref pcyc i))
		 (if (= i 0) xl
		     (aref seq (aref pcyc (1- i))))))))

(defmethod permute! ((seq cons) (perm permutation-cycle) &optional arg)
  (declare (ignore arg))
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

(defmethod permute! ((seq vector) (perm permutation-cycle) &optional arg)
  (declare (ignore arg))
  (dolist (cyc (repr perm) seq)
    (declare (type perrepr-vector cyc))
    (apply-cycle! seq cyc)))

(defmethod permute! ((A standard-tensor) (perm permutation-cycle) &optional (arg 0))
  (multiple-value-bind (tone ttwo) (let ((slst (make-list (rank A) :initial-element '\:)))
				     (rplaca (nthcdr arg slst) 0)
				     (values (sub-tensor~ A slst) (sub-tensor~ A slst)))
    (let-typed ((cyclst (repr perm) :type cons)
		(cp-ten (make-instance (class-of tone)
				       :dimensions (copy-seq (dimensions tone))))
		(std-arg (aref (strides A) arg) :type index-type)
		(hd-sl (head ttwo) :type index-type))
	       (dolist (cyc cyclst)
		 (declare (type perrepr-vector cyc))
		 (setf (head tone) (+ hd-sl (* std-arg (aref cyc (1- (length cyc))))))
		 (copy! tone cp-ten)
		 (loop :for i :of-type index-type :downfrom (1- (length cyc)) :to 0
		    :do (progn
			  (setf (head tone) (+ hd-sl (* std-arg (aref cyc i))))
			  (copy!
			   (if (= i 0) cp-ten
			       (progn
				 (setf (head ttwo) (+ hd-sl (* std-arg (aref cyc (1- i)))))
				 ttwo))
			   tone))))))
  A)

;;Pivot idx
(defmethod permute! ((seq vector) (perm permutation-pivot-flip) &optional arg)
  (declare (ignore arg))
  (let-typed ((pidx (repr perm) :type perrepr-vector))
	     (loop :for i :of-type index-type :from 0 :below (group-rank perm)
		:unless (= i (aref pidx i))
		:do (rotatef (aref seq i) (aref seq (aref pidx i)))
		:finally (return seq))))

(defmethod permute! ((seq cons) (perm permutation-pivot-flip) &optional arg)
  (declare (ignore arg))
  (let ((cseq (make-array (length seq) :initial-contents seq))
	(glen (group-rank perm)))
    (permute! cseq perm)
    (mapl
     (let ((i 0))
       (lambda (x)
	 (when (< i glen)
	   (rplaca x (aref cseq i))
	   (incf i)))) seq)))

(defmethod permute! ((A standard-tensor) (perm permutation-pivot-flip) &optional (arg 0))
  (let ((idiv (repr perm)))
    (multiple-value-bind (tone ttwo) (let ((slst (make-list (rank A) :initial-element '\:)))
				       (rplaca (nthcdr arg slst) 0)
				       (values (sub-tensor~ A slst nil) (sub-tensor~ A slst nil)))
      (let ((argstd (aref (strides A) arg))
	    (hd-sl (head ttwo)))
	(loop :for i :from 0 :below (length idiv)
	   :do (progn
		 (unless (= i (aref idiv i))
		   (setf (head ttwo) (+ hd-sl (* (aref idiv i) argstd)))
		   (swap! tone ttwo))
		 (incf (head tone) argstd))))))
  A)

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
		       :for x :of-type perrepr-type := (aref arr x0) :then (aref arr x)
		       :and ret :of-type cons := (list x0) :then (cons x ret)
		       :counting t :into i :of-type index-type
		       :when (= x x0)
		       :do (return (values i ret))))))
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
	     :for i :of-type index-type :downfrom (1- (length cyc)) :to 0
	     :do (setf (aref act-repr (aref cyc i))
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
	(loop :for i :from 0 :below len
	   :do (let ((val (aref idiv i)))
		 (unless (= val i)
		   (rotatef (aref act i) (aref act val))))))
      (make-instance 'permutation-action :repr act))))

(defun mod-max (seq lidx uidx)
  (declare (type perrepr-vector seq))
  (let ((len (length seq)))
    (very-quickly
      (loop :for idx :of-type index-type :downfrom uidx :above lidx
	 :with max :of-type perrepr-type := (aref seq uidx)
	 :with max-idx :of-type index-type := uidx
	 :do (let ((ele (aref seq (mod idx len))))
	       (when (> ele max)
		 (setf max ele
		       max-idx idx)))
	 :finally (return (values max max-idx))))))

#|

(defun cycle->pivot-flip (cyc)
  (let ((cp-cyc (copy-seq cyc)))
    (let
	(labels ((mod-max (seq lidx uidx)
		   (declare (type perrepr-vector seq))
		   (let ((len (length cyc)))
		     (very-quickly
		       (loop :for idx :of-type index-type :downfrom uidx :above lidx
			  :with max :of-type perrepr-type := (aref seq uidx)
			  :with max-idx :of-type index-type := uidx
			  :do (let ((ele (aref seq (mod idx len))))
				(when (> ele max)
				  (setf max ele
					max-idx idx)))
			  :finally (return (values max max-idx))))))
		 (get-flip (lidx uidx)
		   (multiple-value-bind (max max-idx) (mod-max cyc lidx uidx)
		     (let ((ele-0 (aref cyc (mod max-idx len)))
			   (ele-1 (aref cyc (mod (1- max-idx) len))))
		       (setf (aref pidx (max ele-0 ele-1))
			     (min ele-0 ele-1))
		       |#

#+nil
(defun permute-argument (func-symbol perm)
  (declare (type symbol func-symbol)
	   (type permutation perm))
  (let* ((glen (group-rank perm))
	 (args (loop :for i :from 0 :below glen
		  :collect (gensym))))
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

(definline sort-permute (seq predicate)
  "
  Sorts a lisp-vector in-place, by using the function @arg{predicate} as the
  order. Also computes the permutation which would sort the original sequence
  @arg{seq}.
  "
  (declare (type vector seq))
  ;;This function is ugly of-course, but is also very very quick!
  (let*-typed ((len (length seq) :type fixnum)
	       (perm (perrepr-id-action len) :type perrepr-vector)
	       (jobs (list `(0 ,len))))
	      (loop ;;:repeat 10
		 :for bounds := (pop jobs) :then (pop jobs)
		 :until (null bounds)
		 :do (let*-typed ((below-idx (first bounds) :type fixnum)
				  (above-idx (second bounds) :type fixnum)
				  (piv (+ below-idx (floor (- above-idx below-idx) 2)) :type fixnum))
				 (loop ;;:repeat 10
				    :with ele :=  (aref seq piv)
				    :with lbound :of-type fixnum := below-idx
				    :with ubound :of-type fixnum := (1- above-idx)
				    :until (progn
					     ;;(format t "~%~a ~%" (list lbound piv ubound))
					     (loop :for i :of-type fixnum :from lbound :to piv
						:until (or (= i piv) (funcall predicate ele (aref seq i)))
						:finally (setq lbound i))
					     (loop :for i :of-type fixnum :downfrom ubound :to piv
						:until (or (= i piv) (funcall predicate (aref seq i) ele))
						:finally (setq ubound i))
					     ;;(format t "~a ~%" (list lbound piv ubound))
					     (cond
					       ((= ubound lbound piv)
						(when (> (- piv below-idx) 1)
						  (push `(,below-idx ,piv) jobs))
						(when (> (- above-idx (1+ piv)) 1)
						  (push `(,(1+ piv) ,above-idx) jobs))
						;;(format t "~a~%" jobs)
						t)
					       ((< lbound piv ubound)
						(rotatef (aref seq lbound) (aref seq ubound))
						(rotatef (aref perm lbound) (aref perm ubound))
						(incf lbound)
						(decf ubound)
						nil)
					       ((= lbound piv)
						(rotatef (aref seq piv) (aref seq (1+ piv)))
						(rotatef (aref perm piv) (aref perm (1+ piv)))
						(unless (= ubound (1+ piv))
						  (rotatef (aref seq piv) (aref seq ubound))
						  (rotatef (aref perm piv) (aref perm ubound)))
						(incf piv)
						(incf lbound)
						nil)
					       ((= ubound piv)
						(rotatef (aref seq (1- piv)) (aref seq piv))
						(rotatef (aref perm (1- piv)) (aref perm piv))
						(unless (= lbound (1- piv))
						  (rotatef (aref seq lbound) (aref seq piv))
						  (rotatef (aref perm lbound) (aref perm piv)))
						(decf piv)
						(decf ubound)
						nil)))))
		 :finally (return (values seq (make-instance 'permutation-action :repr perm))))))
