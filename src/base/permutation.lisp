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
	(very-quickly
	  (loop
	     :for cyc :of-type pindex-store-vector :in (store per)
	     :with ss :of-type pindex-type := 0
	     :do (loop
		    :for i :of-type index-type :from 1 :below (length cyc)
		    :with scyc :of-type pindex-store-vector := (sort (copy-seq cyc) #'<)
		    :do (assert (/= (aref scyc (1- i)) (aref scyc i)) nil 'permutation-invalid-error)
		    :finally (setf ss (max ss (aref scyc (1- (length scyc))))))
	     :finally (setf (permutation-size per) (1+ ss)))))))

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
	       (setf (store-size per) len))))

;;
(defclass permutation-matrix (permutation)
  ((store :type pindex-store-vector)))

(defmethod initialize-instance :after ((perm permutation-matrix) &rest initargs)
  (declare (ignore initargs))
  (when *check-after-initializing?*
    (let-typed ((repr (store perm) :type pindex-store-vector))
	       (very-quickly
		 (loop :for i :of-type index-type :from 0 :below (length repr)
		    :with srepr :of-type pindex-store-vector := (sort (copy-seq repr) #'<)
		    :do (assert (= (aref srepr i) i) nil 'permutation-invalid-error)))
	       (setf (permutation-size perm) (length repr)))))

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

;; ;;Action
;; (defmethod permute! ((seq cons) (perm permutation-action) &optional arg)
;;   (declare (ignore arg))
;;   (let ((cseq (make-array (length seq) :initial-contents seq))
;; 	(act (repr perm))
;; 	(glen (group-rank perm)))
;;     (mapl
;;      (let ((i 0))
;;        (declare (type fixnum i))
;;        (lambda (x)
;; 	 (when (< i glen)
;; 	   (rplaca x (aref cseq (aref act i)))
;; 	   (incf i)))) seq)))

;; (defmethod permute! ((seq vector) (perm permutation-action) &optional arg)
;;   (declare (ignore arg))
;;   (let ((cseq (make-array (length seq) :initial-contents seq))
;; 	(act (repr perm)))
;;     (loop
;;        :for i :from 0 :below (group-rank perm)
;;        :do (unless (= i (aref act i))
;; 	     (setf (aref seq i) (aref cseq (aref act i))))
;;        :finally (return seq))))

;; (defmethod permute! ((ten standard-tensor) (perm permutation-action) &optional (arg 0))
;;   (let ((cyc (action->cycle perm)))
;;     (permute! ten cyc arg)))

;; ;;Cycle
;; ;;Might be useful ?
;; (defun apply-cycle! (seq pcyc)
;;   (declare (type pindex-store-vector pcyc)
;; 	   (type vector seq))
;;   (let ((xl (aref seq (aref pcyc (1- (length pcyc))))))
;;     (loop :for i :of-type index-type :downfrom (1- (length pcyc)) :to 0
;;        :do (setf (aref seq (aref pcyc i))
;; 		 (if (= i 0) xl
;; 		     (aref seq (aref pcyc (1- i))))))))

;; (defmethod permute! ((seq cons) (perm permutation-cycle) &optional arg)
;;   (declare (ignore arg))
;;   (let ((cseq (make-array (length seq) :initial-contents seq))
;; 	(glen (group-rank perm)))
;;     (dolist (cyc (repr perm))
;;       (declare (type pindex-store-vector cyc))
;;       (apply-cycle! cseq cyc))
;;     (mapl
;;      (let ((i 0))
;;        (lambda (x)
;; 	 (when (< i glen)
;; 	   (rplaca x (aref cseq i))
;; 	   (incf i)))) seq)))

;; (defmethod permute! ((seq vector) (perm permutation-cycle) &optional arg)
;;   (declare (ignore arg))
;;   (dolist (cyc (repr perm) seq)
;;     (declare (type pindex-store-vector cyc))
;;     (apply-cycle! seq cyc)))

;; (defmethod permute! ((A standard-tensor) (perm permutation-cycle) &optional (arg 0))
;;   (multiple-value-bind (tone ttwo) (let ((slst (make-list (rank A) :initial-element '\:)))
;; 				     (rplaca (nthcdr arg slst) 0)
;; 				     (values (sub-tensor~ A slst) (sub-tensor~ A slst)))
;;     (let-typed ((cyclst (repr perm) :type cons)
;; 		(cp-ten (make-instance (class-of tone)
;; 				       :dimensions (copy-seq (dimensions tone))))
;; 		(std-arg (aref (strides A) arg) :type index-type)
;; 		(hd-sl (head ttwo) :type index-type))
;; 	       (dolist (cyc cyclst)
;; 		 (declare (type pindex-store-vector cyc))
;; 		 (setf (head tone) (+ hd-sl (* std-arg (aref cyc (1- (length cyc))))))
;; 		 (copy! tone cp-ten)
;; 		 (loop :for i :of-type index-type :downfrom (1- (length cyc)) :to 0
;; 		    :do (progn
;; 			  (setf (head tone) (+ hd-sl (* std-arg (aref cyc i))))
;; 			  (copy!
;; 			   (if (= i 0) cp-ten
;; 			       (progn
;; 				 (setf (head ttwo) (+ hd-sl (* std-arg (aref cyc (1- i)))))
;; 				 ttwo))
;; 			   tone))))))
;;   A)

;; ;;Pivot idx
;; (defmethod permute! ((seq vector) (perm permutation-pivot-flip) &optional arg)
;;   (declare (ignore arg))
;;   (let-typed ((pidx (repr perm) :type pindex-store-vector))
;; 	     (loop :for i :of-type index-type :from 0 :below (group-rank perm)
;; 		:unless (= i (aref pidx i))
;; 		:do (rotatef (aref seq i) (aref seq (aref pidx i)))
;; 		:finally (return seq))))

;; (defmethod permute! ((seq cons) (perm permutation-pivot-flip) &optional arg)
;;   (declare (ignore arg))
;;   (let ((cseq (make-array (length seq) :initial-contents seq))
;; 	(glen (group-rank perm)))
;;     (permute! cseq perm)
;;     (mapl
;;      (let ((i 0))
;;        (lambda (x)
;; 	 (when (< i glen)
;; 	   (rplaca x (aref cseq i))
;; 	   (incf i)))) seq)))

;; (defmethod permute! ((A standard-tensor) (perm permutation-pivot-flip) &optional (arg 0))
;;   (let ((idiv (repr perm)))
;;     (multiple-value-bind (tone ttwo) (let ((slst (make-list (rank A) :initial-element '\:)))
;; 				       (rplaca (nthcdr arg slst) 0)
;; 				       (values (sub-tensor~ A slst nil) (sub-tensor~ A slst nil)))
;;       (let ((argstd (aref (strides A) arg))
;; 	    (hd-sl (head ttwo)))
;; 	(loop :for i :from 0 :below (length idiv)
;; 	   :do (progn
;; 		 (unless (= i (aref idiv i))
;; 		   (setf (head ttwo) (+ hd-sl (* (aref idiv i) argstd)))
;; 		   (swap! tone ttwo))
;; 		 (incf (head tone) argstd))))))
;;   A)

;; ;;Conversions----------------------------------------------------;;
;; (defun action->cycle (act)
;;   "
;;   (action->cycle act)

;;   This function obtains the canonical cycle representation
;;   of a permutation. The first argument \"act\" is the action of the
;;   permutation on the array #(0 1 2 3 ..): an object of the class
;;   permutation-action.

;;   \"Canonical\" may be a bit of an overstatement; this is the way
;;   S_n was presented in Van der Waerden's book.
;; "
;;   (declare (type permutation-action act))
;;   (mlet*
;;    ((arr (repr act) :type pindex-store-vector))
;;    (labels ((find-cycle (x0)
;; 	      ;; This function obtains the cycle starting from x_0.
;; 	      (declare (type pindex-type x0))
;; 	      (if (= (aref arr x0) x0) (values 0 nil)
;; 		  (very-quickly
;; 		    (loop
;; 		       :for x :of-type pindex-type := (aref arr x0) :then (aref arr x)
;; 		       :and ret :of-type cons := (list x0) :then (cons x ret)
;; 		       :counting t :into i :of-type index-type
;; 		       :when (= x x0)
;; 		       :do (return (values i ret))))))
;; 	    (cycle-walk (cyc ignore)
;; 	      ;; Finds all cycles
;; 	      (let ((x0 (find-if-not #'(lambda (x) (member x ignore)) arr)))
;; 		(if (null x0)
;; 		    cyc
;; 		    (multiple-value-bind (clen clst) (find-cycle x0)
;; 		      (declare (type index-type clen)
;; 			       (type list clst))
;; 		      (cycle-walk
;; 		       (if (= clen 0) cyc
;; 			   (cons (make-array clen :element-type 'pindex-type :initial-contents clst) cyc))
;; 		       (nconc ignore (if (= clen 0) (list x0) clst))))))))
;;      (let ((cyc-lst (cycle-walk nil nil)))
;;        (make-instance 'permutation-cycle
;; 		      :repr cyc-lst)))))

;; (defun cycle->action (cyc)
;;   "
;;    (cycle->action cyc)

;;    This function obtains the action representation of a permutation
;;    from the cyclic one. The first argument \"cyc\" is the cyclic
;;    representation of the permutation: an object of the class
;;    permutation-cycle.
;; "
;;   (declare (type permutation-cycle cyc))
;;   (let ((act-repr (pindex-id-action (group-rank cyc)))
;; 	(cycs-repr (repr cyc)))
;;     (declare (type pindex-store-vector act-repr))
;;     (dolist (cyc cycs-repr)
;;       (declare (type pindex-store-vector cyc))
;;       (let ((xl (aref act-repr (aref cyc (1- (length cyc))))))
;; 	(very-quickly
;; 	  (loop
;; 	     :for i :of-type index-type :downfrom (1- (length cyc)) :to 0
;; 	     :do (setf (aref act-repr (aref cyc i))
;; 		       (if (= i 0) xl
;; 			   (aref act-repr (aref cyc (1- i)))))))))
;;     (make-instance 'permutation-action :repr act-repr)))

;; (defun pivot-flip->action (pflip)
;;   (declare (type permutation-pivot-flip pflip))
;;   (let* ((idiv (repr pflip))
;; 	 (len (length idiv)))
;;     (declare (type pindex-store-vector idiv)
;; 	     (type index-type len))
;;     (let ((act (pindex-id-action len)))
;;       (declare (type pindex-store-vector act))
;;       (very-quickly
;; 	(loop :for i :from 0 :below len
;; 	   :do (let ((val (aref idiv i)))
;; 		 (unless (= val i)
;; 		   (rotatef (aref act i) (aref act val))))))
;;       (make-instance 'permutation-action :repr act))))

;; (defun mod-max (seq lidx uidx)
;;   (declare (type pindex-store-vector seq))
;;   (let ((len (length seq)))
;;     (very-quickly
;;       (loop :for idx :of-type index-type :downfrom uidx :above lidx
;; 	 :with max :of-type pindex-type := (aref seq uidx)
;; 	 :with max-idx :of-type index-type := uidx
;; 	 :do (let ((ele (aref seq (mod idx len))))
;; 	       (when (> ele max)
;; 		 (setf max ele
;; 		       max-idx idx)))
;; 	 :finally (return (values max max-idx))))))

;; #|

;; (defun cycle->pivot-flip (cyc)
;;   (let ((cp-cyc (copy-seq cyc)))
;;     (let
;; 	(labels ((mod-max (seq lidx uidx)
;; 		   (declare (type pindex-store-vector seq))
;; 		   (let ((len (length cyc)))
;; 		     (very-quickly
;; 		       (loop :for idx :of-type index-type :downfrom uidx :above lidx
;; 			  :with max :of-type pindex-type := (aref seq uidx)
;; 			  :with max-idx :of-type index-type := uidx
;; 			  :do (let ((ele (aref seq (mod idx len))))
;; 				(when (> ele max)
;; 				  (setf max ele
;; 					max-idx idx)))
;; 			  :finally (return (values max max-idx))))))
;; 		 (get-flip (lidx uidx)
;; 		   (multiple-value-bind (max max-idx) (mod-max cyc lidx uidx)
;; 		     (let ((ele-0 (aref cyc (mod max-idx len)))
;; 			   (ele-1 (aref cyc (mod (1- max-idx) len))))
;; 		       (setf (aref pidx (max ele-0 ele-1))
;; 			     (min ele-0 ele-1))
;; 		       |#

;; #+nil
;; (defun permute-argument (func-symbol perm)
;;   (declare (type symbol func-symbol)
;; 	   (type permutation perm))
;;   (let* ((glen (group-rank perm))
;; 	 (args (loop :for i :from 0 :below glen
;; 		  :collect (gensym))))
;;     (eval `(lambda (,@args &rest rest)
;; 	     (apply ',func-symbol (append (list ,@(permute! args perm)) rest))))))

;; (defun argument-permute (func perm)
;;   (declare (type function func)
;; 	   (type permutation perm))
;;   (lambda (&rest args)
;;     (apply func (permute! args perm))))

;; (defun curry (func perm &rest curried-args)
;;   (declare (type function func)
;; 	   (type permutation perm))
;;   (lambda (&rest args)
;;     (apply func (permute! (append curried-args args) perm))))

;; (defun compose (func-a func-b perm)
;;   (declare (type function func-a func-b)
;; 	   (type permutation perm))
;;   (lambda (&rest args)
;;     (apply func-a (permute! (multiple-value-list (funcall func-b args)) perm))))
;; ;;


;; (defstruct (ring (:constructor nil))
;;   (circular-list nil :type list)
;;   (end-list nil :type list))

;; (defun make-ring (n)
;;   (let ((ret (cons nil nil)))
;;     (loop :for i :from 0 :below (1- n)
;;        :with tail :of-type cons := ret
;;        :do (setf (cdr tail) (cons nil nil)
;; 		 tail (cdr tail))
;;        :finally (setf (cdr tail) ret))
;;     ret))


;;This function is ugly of-course, but is also very very quick!
(definline sort-permute (seq predicate &key (key #'matlisp-utilities:id))
  "
  Sorts a lisp-vector in-place, by using the function @arg{predicate} as the
  order. Also computes the permutation which would sort the original sequence
  @arg{seq}.
  "
  (declare (type vector seq))
  (let*-typed ((len (length seq) :type fixnum)
	       (perm (pindex-id len) :type pindex-store-vector)
	       (jobs (list `(0 ,len))))
	      (loop
		 :for bounds := (car jobs) :then (pop jobs)
		 :until (null bounds)
		 :do (let*-typed ((below-idx (first bounds) :type fixnum)
				  (above-idx (second bounds) :type fixnum)
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
						:until (or (= i piv) (funcall predicate ele (funcall key (aref seq i))))
						:finally (setq ubound i))
					     (cond
					       ((= ubound lbound piv)
						(when (> (- piv below-idx) 1)
						  (push `(,below-idx ,piv) jobs))
						(when (> (- above-idx (1+ piv)) 1)
						  (push `(,(1+ piv) ,above-idx) jobs))
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
		 :finally (return (values seq (make-instance 'permutation-action :store perm))))))
