(in-package #:matlisp)

;;This must match the type used in LAPACK
(deftype pindex-type ()
  '(unsigned-byte 32))

(deftype pindex-store-vector (&optional (size '*))
  `(simple-array pindex-type (,size)))

(make-array-allocator allocate-pindex-store 'pindex-type 0
  "
  Syntax
  ======
  (ALLOCATE-PINDEX-STORE SIZE [INITIAL-ELEMENT 0])

  Purpose
  =======
  Allocates integer4 (32-bits) storage.")
;;

(definline pindex-id (n)
  (declare (type fixnum n))
  (let-typed ((ret (allocate-pindex-store n) :type pindex-store-vector))
	     (very-quickly
	       (loop :for i :of-type pindex-type :from 0 :below n
		  :do (setf (aref ret i) i)))
	     ret))

(definline pidxv (&rest contents)
  (make-array (length contents) :element-type 'pindex-type :initial-contents contents))

;;Write a uniform randomiser
(defun seqrnd (seq)
  "Randomize the elements of a sequence. Destructive on SEQ."
  (sort seq #'> :key #'(lambda (x) (declare (ignore x))
			       (random 1.0))))

;;Class definitions----------------------------------------------;;
(defclass permutation ()
  ((store :accessor store :initarg :store)
   (permutation-size :accessor permutation-size :type index-type)))

(defmethod print-object ((per permutation) stream)
  (print-unreadable-object (per stream :type t)
    (format stream "S_~a~%" (permutation-size per))
    (if (= (permutation-size per) 1)
	(format stream "ID~%")
	(format stream "~a~%" (store per)))))

;;
(defclass permutation-action (permutation)
  ((store :type pindex-store-vector)))

(defmethod initialize-instance :after ((perm permutation-action) &rest initargs)
  (declare (ignore initargs))
  (when *check-after-initializing?*
    (let-typed ((repr (store perm) :type pindex-store-vector))
	       (very-quickly
		 (loop :for i :of-type index-type :from 0 :below (length repr)
		    :with srepr :of-type pindex-store-vector := (sort (copy-seq repr) #'<)
		    :do (assert (= (aref srepr i) i) nil 'permutation-invalid-error)))
	       (setf (permutation-size perm) (length repr)))))

;;
(defclass permutation-cycle (permutation)
  ((store :type list)))

(defmethod initialize-instance :after ((per permutation-cycle) &rest initargs)
  (declare (ignore initargs))
  (when *check-after-initializing?*
    (if (null (store per))
	(setf (permutation-size per) 1)
	(loop
	   :for cyc :of-type pindex-store-vector :in (store per)
	   :with ss :of-type pindex-type := 0
	   :do (very-quickly
		 (loop
		    :for i :of-type index-type :from 1 :below (length cyc)
		    :with scyc :of-type pindex-store-vector := (sort (copy-seq cyc) #'<)
		    :do (assert (/= (aref scyc (1- i)) (aref scyc i)) nil 'permutation-invalid-error)
		    :finally (setf ss (max ss (aref scyc (1- (length scyc)))))))
	   :finally (setf (permutation-size per) (1+ ss))))))

;;
(defclass permutation-pivot-flip (permutation)
  ((store :type pindex-store-vector)))

(defmethod initialize-instance :after ((per permutation-pivot-flip) &rest initargs)
  (declare (ignore initargs))
  (when *check-after-initializing?*
    (let*-typed ((repr (store per) :type pindex-store-vector)
		 (len (length repr) :type index-type))
	       (very-quickly
		 (loop :for i :of-type index-type :from 0 :below len
		    :do (assert (< -1 (aref repr i) len) nil 'permutation-invalid-error)))
	       (setf (permutation-size per) len))))

;;Generic permute! method.
(defgeneric permute! (thing permutation &optional argument)
  (:documentation "
  (permute! thing permutation [argument 0])

  Permutes the ARGUMENT index of the the array-like object THING, by
  applying PERMUTATION on it.")
  (:method :before ((seq sequence) (perm permutation) &optional (arg 0))
	   (declare (ignore arg))
	   (let ((len (length seq)))
	     (assert (>= len (permutation-size perm)) nil
		     'permutation-permute-error :seq-len len :per-size (permutation-size perm))))
  (:method :before ((ten standard-tensor) (perm permutation) &optional (arg 0))
	   (assert (>= (aref (dimensions ten) arg) (permutation-size perm)) nil
		   'permutation-permute-error :seq-len (aref (dimensions ten) arg) :permutation-size (permutation-size perm))))

(definline permute (thing perm &optional (arg 0))
  (permute! (copy thing) perm arg))

;;Action
(definline apply-action! (seq perm)
  (declare (type vector seq)
	   (type pindex-store-vector perm))
  (let* ((size (length perm))
	 (cseq (vectorify seq size)))
    (loop :for i :from 0 :below size
       :do (setf (aref seq i) (aref cseq (aref perm i)))
       :finally (return seq))))

(defmethod permute! ((seq cons) (perm permutation-action) &optional arg)
  (declare (ignore arg))
  (let* ((size (permutation-size perm))
	 (cseq (vectorify seq size))
	 (act (store perm)))
    (loop :for i :from 0 :below size
       :for lst := seq :then (cdr lst)
       :do (setf (car lst) (aref cseq (aref act i)))
       :finally (return seq))))

(defmethod permute! ((seq vector) (perm permutation-action) &optional arg)
  (declare (ignore arg))
  (apply-action! seq (the pindex-store-vector (store perm))))

(defmethod permute! ((ten standard-tensor) (perm permutation-action) &optional (arg 0))
  (permute! ten (action->pivot-flip perm) arg))

;;Cycle
(definline apply-cycle! (seq pcyc)
  (declare (type pindex-store-vector pcyc)
	   (type vector seq))
  (loop :for i :of-type index-type :downfrom (1- (length pcyc)) :to 1
     :with xl := (aref seq (aref pcyc (1- (length pcyc))))
     :do (setf (aref seq (aref pcyc i)) (aref seq (aref pcyc (1- i))))
     :finally (progn
		(setf (aref seq (aref pcyc 0)) xl)
		(return seq))))

(defmethod permute! ((seq cons) (perm permutation-cycle) &optional arg)
  (declare (ignore arg))
  (unless (= (permutation-size perm) 1)
    (let* ((size (permutation-size perm))
	   (cseq (vectorify seq size)))
      (loop :for cyc :of-type pindex-store-vector :in (store perm)
	 :do (apply-cycle! cseq cyc))
      (copy-n cseq seq size)))
  seq)

(defmethod permute! ((seq vector) (perm permutation-cycle) &optional arg)
  (declare (ignore arg))
  (unless (= (permutation-size perm) 1)
    (loop :for cyc :of-type pindex-store-vector :in (store perm)
       :do (apply-cycle! seq cyc)))
  seq)

(defmethod permute! ((A standard-tensor) (perm permutation-cycle) &optional (arg 0))
  (permute! A (action->pivot-flip (cycle->action perm)) arg))

;Pivot idx
(definline apply-flips! (seq pflip)
  (declare (type pindex-store-vector pflip)
	   (type vector seq))
  (loop :for i :of-type index-type :from 0 :below (length pflip)
     :unless (= i (aref pflip i))
     :do (rotatef (aref seq i) (aref seq (aref pflip i)))
     :finally (return seq)))

(defmethod permute! ((seq vector) (perm permutation-pivot-flip) &optional arg)
  (declare (ignore arg))
  (apply-flips! seq (store perm)))

(defmethod permute! ((seq cons) (perm permutation-pivot-flip) &optional arg)
  (declare (ignore arg))
  (let* ((size (permutation-size perm))
	 (cseq (vectorify seq size)))
    (apply-flips! cseq (store perm))
    (copy-n cseq seq size))
  seq)

(defmethod permute! ((A standard-tensor) (perm permutation-pivot-flip) &optional (arg 0))
  (multiple-value-bind (t1 t2) (let ((slst (make-list (rank A) :initial-element '(* * *))))
				 (rplaca (nthcdr arg slst) (list 0 '* 1))
				 (values (sub-tensor~ A slst nil) (sub-tensor~ A slst nil)))
    (let-typed ((argstd (aref (strides A) arg) :type index-type)
		(hd-sl (head t2) :type index-type)
		(idiv (store perm) :type pindex-store-vector))
	       (very-quickly
		 (loop :for i :from 0 :below (length idiv)
		    :do (progn
			  (unless (= i (aref idiv i))
			    (setf (slot-value t2 'head) (the index-type (+ hd-sl (the index-type (* (aref idiv i) argstd)))))
			    (swap! t1 t2))
			  (setf (slot-value t1 'head) (the index-type (+ argstd (the index-type (head t1))))))))))
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
  (let-typed ((arr (store act) :type pindex-store-vector))
	     (labels ((find-cycle (x0)
			;; This function obtains the cycle starting from x_0.
			(declare (type pindex-type x0))
			(if (= (aref arr x0) x0) (values 0 nil)
			    (very-quickly
			      (loop
				 :for x :of-type pindex-type := (aref arr x0) :then (aref arr x)
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
				     (cons (make-array clen :element-type 'pindex-type :initial-contents clst) cyc))
				 (nconc ignore (if (= clen 0) (list x0) clst))))))))
	       (make-instance 'permutation-cycle :store (cycle-walk nil nil)))))

(defun action->pivot-flip (act)
  (declare (type permutation-action act))
  (let*-typed ((size (permutation-size act) :type index-type)
	       (actr (store act) :type pindex-store-vector)
	       (ret (pindex-id size) :type pindex-store-vector)
	       (inv (pindex-id size) :type pindex-store-vector)
	       (for (pindex-id size) :type pindex-store-vector))
	      (very-quickly
		(loop :for i :of-type index-type :from 0 :below size
		   :do (let ((flip (aref inv (aref actr i))))
			 (setf (aref ret i) flip
			       (aref inv (aref for i)) flip
			       (aref for flip) (aref for i)))))
	      (make-instance 'permutation-pivot-flip :store ret)))

(defun cycle->action (cyc)
  "
   (cycle->action cyc)

   This function obtains the action representation of a permutation
   from the cyclic one. The first argument \"cyc\" is the cyclic
   representation of the permutation: an object of the class
   permutation-cycle.
"
  (declare (type permutation-cycle cyc))
  (let-typed ((act-repr (pindex-id (permutation-size cyc)) :type pindex-store-vector)
	      (cycs (store cyc)))
	     (very-quickly
	       (loop :for cyc :of-type pindex-store-vector :in cycs
		  :do (apply-cycle! act-repr cyc)))
	     (make-instance 'permutation-action :store act-repr)))

(defun pivot-flip->action (pflip)
  (declare (type permutation-pivot-flip pflip))
  (let*-typed ((idiv (store pflip) :type pindex-store-vector)
	       (len (permutation-size pflip) :type index-type)
	       (ret (pindex-id len) :type pindex-store-vector))
	      (make-instance 'permutation-action :store (very-quickly (apply-flips! ret idiv)))))

;;Uber-functional stuff
;;None of these are ever useful (I've found); neat things for showing off though :]
(defun permute-arguments-and-compile (func perm)
  (declare (type function func)
	   (type permutation perm))
  (let ((args (loop :for i :from 0 :below (permutation-size perm)
		 :collect (gensym))))
    (compile-and-eval `(lambda (,@args &rest rest)
			 (apply ,func (append (list ,@(permute! args perm)) rest))))))

(defun permute-arguments (func perm)
  (declare (type function func)
	   (type permutation perm))
  (lambda (&rest args)
    (apply func (permute! args perm))))

(defun curry (func perm &rest curried-args)
  (declare (type function func)
	   (type permutation perm))
  (lambda (&rest args)
    (apply func (permute! (append curried-args args) perm))))

(defun curry-and-compile (func perm &rest curried-args)
  (declare (type function func)
	   (type permutation perm))
  (let ((args (loop :for i :from 0 :below (permutation-size perm)
		 :collect (gensym))))
    (compile-and-eval
     `(let (,@(mapcar #'(lambda (a b) `(,a ,b)) args curried-args))
	(lambda (,@(nthcdr (length curried-args) args) &rest rest)
	  (apply ,func (append (list ,@(permute! args perm)) rest)))))))

(defun compose (func-a func-b perm)
  (declare (type function func-a func-b)
	   (type permutation perm))
  (lambda (&rest args)
    (apply func-a (permute! (multiple-value-list (funcall func-b args)) perm))))

(defun compose-and-compile (func-a func-b perm)
  (declare (type function func-a func-b)
	   (type permutation perm))
  (let ((syms (loop :for i :from 0 :below (permutation-size perm)
		 :collect (gensym))))
    (compile-and-eval     
     `(lambda (&rest args)
	(destructuring-bind (,@syms &rest rest) (multiple-value-list (apply ,func-b args))
	  (apply ,func-a (append (list ,@(permute! syms perm)) rest)))))))

;;Back to practical matters.
;;This function is ugly of-course, but is also very very quick!
(definline sort-permute-base (seq predicate &key (key #'matlisp-utilities:id))
  "
  Sorts a lisp-vector in-place, by using the function @arg{predicate} as the
  order. Also computes the permutation action which would sort the original
  sequence @arg{seq} when applied.
  "
  (declare (type vector seq))
  (let*-typed ((len (length seq) :type fixnum)
	       (perm (pindex-id len) :type pindex-store-vector)
	       (jobs (make-array len :adjustable t :fill-pointer 0)))
	      (loop
		 :for bounds := (cons 0 len) :then (unless (zerop (length jobs))
						     (vector-pop jobs))
		 :until (null bounds)
		 :do (let*-typed ((below-idx (car bounds) :type fixnum)
				  (above-idx (cdr bounds) :type fixnum)
				  (piv (+ below-idx (floor (- above-idx below-idx) 2)) :type fixnum))
				 (loop
				    :with ele :=  (funcall key (aref seq piv))
				    :with lbound :of-type fixnum := below-idx
				    :with ubound :of-type fixnum := (1- above-idx)
				    :until (progn
					     (loop :for i :of-type fixnum :from lbound :to piv
						:until (or (= i piv) (funcall predicate ele (funcall key (aref seq i))))
						:finally (setq lbound i))
					     (loop :for i :of-type fixnum :downfrom ubound :to piv
						:until (or (= i piv) (funcall predicate (funcall key (aref seq i)) ele))
						:finally (setq ubound i))
					     (cond
					       ((= ubound lbound piv)
						(when (> (- piv below-idx) 1)
						  (vector-push-extend (cons below-idx piv) jobs))
						(when (> (- above-idx (1+ piv)) 1)
						  (vector-push-extend (cons (1+ piv) above-idx) jobs))
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
		 :finally (return (values seq perm)))))

(definline sort-permute (seq predicate &key (key #'matlisp-utilities:id))
  (multiple-value-bind (seq perm) (sort-permute-base seq predicate :key key)
    (values seq (make-instance 'permutation-action :store perm))))
