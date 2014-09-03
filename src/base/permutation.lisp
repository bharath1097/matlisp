(in-package #:matlisp)

;;This must match the type used in LAPACK
;;(unsigned-byte 32)

(deftype pindex-type ()
  'fixnum)

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

(defun pick-random (k n)
  (let ((ret nil)
	(perm (allocate-pindex-store k)))
    (loop :for i :from 0 :below k
       :do (let ((sd (random (- n i))))
	     (loop :for ele :in ret
		:do (if (> ele sd) (return) (incf sd)))
	     (setf (aref perm i) sd)
	     (setf ret (merge 'list (list sd) ret #'<))))
    (values ret perm)))
#+nil(sort seq #'> :key #'(lambda (x) (declare (ignore x)) (random 1.0)))

(defun shuffle! (seq)
  "Randomize the elements of a sequence. Destructive on SEQ."
  (let* ((len (length seq))
	 (perm (nth-value 1 (pick-random len len))))
    (apply-action! seq perm)))
;;Class definitions----------------------------------------------;;
(defclass permutation ()
  ((store :reader store :initarg :store)
   (permutation-size :reader permutation-size :initarg :size :type index-type)))

(defmethod print-object ((per permutation) stream)
  (print-unreadable-object (per stream :type t)
    (format stream "S_~a~%" (permutation-size per))
    (if (<= (permutation-size per) 1)
	(format stream "ID~%")
	(format stream "~a~%" (store per)))))

(defclass permutation-index-stored (permutation) ())
;;
(defclass permutation-action (permutation-index-stored)
  ((store :type pindex-store-vector)))

(defmethod initialize-instance :after ((perm permutation-action) &rest initargs)
  (declare (ignore initargs))
  (when *check-after-initializing?*
    (let-typed ((repr (store perm) :type pindex-store-vector))
	       (very-quickly
		 (loop :for i :of-type index-type :from 0 :below (length repr)
		    :with srepr :of-type pindex-store-vector := (sort (copy-seq repr) #'<)
		    :do (assert (= (aref srepr i) i) nil 'permutation-invalid-error)))
	       (setf (slot-value perm 'permutation-size) (length repr)))))

;;
(defclass permutation-cycle (permutation)
  ((store :type list)))

(defmethod initialize-instance :after ((per permutation-cycle) &rest initargs)
  (declare (ignore initargs))
  (when *check-after-initializing?*
    (if (null (store per))
	(setf (slot-value per 'permutation-size) 0)
	(loop
	   :for cyc :of-type pindex-store-vector :in (store per)
	   :with ss :of-type pindex-type := 0
	   :do (very-quickly
		 (loop
		    :for i :of-type index-type :from 1 :below (length cyc)
		    :with scyc :of-type pindex-store-vector := (sort (copy-seq cyc) #'<)
		    :do (assert (/= (aref scyc (1- i)) (aref scyc i)) nil 'permutation-invalid-error)
		    :finally (setf ss (max ss (aref scyc (1- (length scyc)))))))
	   :finally (setf (slot-value per 'permutation-size) (1+ ss))))))

;;
(defclass permutation-pivot-flip (permutation-index-stored)
  ((store :type pindex-store-vector)))

(defmethod initialize-instance :after ((per permutation-pivot-flip) &rest initargs)
  (declare (ignore initargs))
  (when *check-after-initializing?*
    (let*-typed ((repr (store per) :type pindex-store-vector)
		 (len (length repr) :type index-type))
	       (very-quickly
		 (loop :for i :of-type index-type :from 0 :below len
		    :do (assert (< -1 (aref repr i) len) nil 'permutation-invalid-error)))
	       (setf (slot-value per 'permutation-size) len))))
;;

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
	   (assert (>= (dims ten arg) (permutation-size perm)) nil
		   'permutation-permute-error :seq-len (dims ten arg) :permutation-size (permutation-size perm))))

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
  (permute! ten (copy perm 'permutation-pivot-flip) arg))

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
  (permute! A (copy perm 'permutation-pivot-flip) arg))

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
  (let ((t1 (slice~ A arg)) (t2 (slice~ A arg)))
    (let-typed ((argstd (strides A arg) :type index-type)
		(hd-sl (head t2) :type index-type)
		(idiv (store perm) :type pindex-store-vector))
	       (very-quickly
		 (loop :for i :from 0 :below (length idiv)
		    :do (progn
			  (unless (= i (aref idiv i))
			    (setf (slot-value t2 'head) (the index-type (+ hd-sl (the index-type (* (aref idiv i) argstd)))))
			    (swap! t1 t2))
			  (setf (slot-value t1 'head) (the index-type (+ argstd (the index-type (head t1)))))))))) ;;type optimization
  A)

;;Conversions----------------------------------------------------;;
(defmethod copy! ((from permutation) (to permutation))
  (if (typep to (type-of from))
      (copy! (store from) (store to))
      (copy! (store (copy from (type-of to))) (store to))))

(defmethod copy-generic ((act permutation-action) (type (eql 'permutation-cycle)))
  (let-typed ((arr (store act) :type pindex-store-vector)
	      (midx 0 :type pindex-type))
	     (labels ((find-cycle (x0)
			;; This function obtains the cycle starting from x_0.
			(declare (type pindex-type x0))
			(if (= (aref arr x0) x0) (values 0 nil)
			    (very-quickly
			      (loop
				 :for x :of-type pindex-type := (aref arr x0) :then (aref arr x)
				 :and ret :of-type cons := (list x0) :then (cons x ret)
				 :maximizing x :into m.x
				 :counting t :into i :of-type index-type
				 :when (= x x0)
				 :do (progn
				       (setf midx (max midx m.x))
				       (return (values i ret)))))))
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
	       (with-no-init-checks (make-instance 'permutation-cycle :store (cycle-walk nil nil) :size (1+ midx))))))

(defmethod copy-generic ((act permutation-action) (type (eql 'permutation-pivot-flip)))
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
     (with-no-init-checks (make-instance 'permutation-pivot-flip :store ret :size size))))

(defmethod copy-generic ((act permutation-action) (type (eql 'permutation-action)))
  (with-no-init-checks
      (make-instance 'permutation-action :store (copy-seq (store act)) :size (permutation-size act))))
;;
(defmethod copy-generic ((cyc permutation-cycle) (type (eql 'permutation-action)))
  (let-typed ((act-repr (pindex-id (permutation-size cyc)) :type pindex-store-vector)
	      (cycs (store cyc)))
	     (very-quickly
	       (loop :for cyc :of-type pindex-store-vector :in cycs
		  :do (apply-cycle! act-repr cyc)))
    (with-no-init-checks (make-instance 'permutation-action :store act-repr :size (length act-repr)))))

(defmethod copy-generic ((cyc permutation-cycle) (type (eql 'permutation-pivot-flip)))
  (copy (copy cyc 'permutation-action) 'permutation-pivot-flip))

(defmethod copy-generic ((cyc permutation-cycle) (type (eql 'permutation-cycle)))
  (with-no-init-checks (make-instance 'permutation-cycle :store (mapcar #'copy-seq (store cyc)) :size (permutation-size cyc))))
;;
(defmethod copy-generic ((pflip permutation-pivot-flip) (type (eql 'permutation-action)))
  (let*-typed ((idiv (store pflip) :type pindex-store-vector)
	       (len (permutation-size pflip) :type index-type)
	       (ret (pindex-id len) :type pindex-store-vector))
    (with-no-init-checks (make-instance 'permutation-action :store (very-quickly (apply-flips! ret idiv)) :size len))))

(defmethod copy-generic ((pflip permutation-pivot-flip) (type (eql 'permutation-cycle)))
  (copy (copy pflip 'permutation-action) 'permutation-cycle))

(defmethod copy-generic ((pflip permutation-pivot-flip) (type (eql 'permutation-pivot-flip)))
  (with-no-init-checks (make-instance 'permutation-pivot-flip :store (copy-seq (store pflip)) :size (permutation-size pflip))))
;;
(defgeneric inv (obj))

(defmethod inv ((obj permutation-action))
  (let*-typed ((sto (store obj) :type pindex-store-vector)
	       (rsto (allocate-pindex-store (length sto)) :type pindex-store-vector))
    (loop :for i :from 0 :below (length rsto)
       :for ele of-type pindex-type :across sto
       :do (setf (aref rsto ele) i))
    (with-no-init-checks (make-instance 'permutation-action :store rsto :size (length rsto)))))

(defmethod inv ((obj permutation-cycle))
  (let ((sto (store obj)))
    (with-no-init-checks
	(make-instance 'permutation-cycle
		       :store (loop :for cyc :of-type pindex-store-vector :in sto
				 :collect (reverse cyc))
		       :size (permutation-size obj)))))

(defmethod inv ((flip permutation-pivot-flip))
  (copy (inv (copy flip 'permutation-action)) 'permutation-pivot-flip))
;;
(defgeneric compose (a b)
  (:method ((a permutation) (b permutation))
    (let ((ret (pindex-id (max (permutation-size a) (permutation-size b)))))
      (permute! ret b)
      (permute! ret a)
      (loop :for i :from (1- (length ret)) :downto 0
	 :do (when (/= i (aref ret i)) (loop-finish))
	 :finally (return (with-no-init-checks (make-instance 'permutation-action :store (subseq ret 0 (1+ i)) :size (1+ i))))))))

;;Uber-functional stuff
;;None of these are ever useful (I've found); neat things for showing off though :]
;; (defun permute-arguments-and-compile (func perm)
;;   (declare (type function func)
;;	   (type permutation perm))
;;   (let ((args (loop :for i :from 0 :below (permutation-size perm)
;;		 :collect (gensym))))
;;     (compile-and-eval `(lambda (,@args &rest rest)
;;			 (apply ,func (append (list ,@(permute! args perm)) rest))))))

;; (defun permute-arguments (func perm)
;;   (declare (type function func)
;;	   (type permutation perm))
;;   (lambda (&rest args)
;;     (apply func (permute! args perm))))

;; (defun curry (func perm &rest curried-args)
;;   (declare (type function func)
;;	   (type permutation perm))
;;   (lambda (&rest args)
;;     (apply func (permute! (append curried-args args) perm))))

;; (defun curry-and-compile (func perm &rest curried-args)
;;   (declare (type function func)
;;	   (type permutation perm))
;;   (let ((args (loop :for i :from 0 :below (permutation-size perm)
;;		 :collect (gensym))))
;;     (compile-and-eval
;;      `(let (,@(mapcar #'(lambda (a b) `(,a ,b)) args curried-args))
;;	(lambda (,@(nthcdr (length curried-args) args) &rest rest)
;;	  (apply ,func (append (list ,@(permute! args perm)) rest)))))))

;; (defun compose (func-a func-b perm)
;;   (declare (type function func-a func-b)
;;	   (type permutation perm))
;;   (lambda (&rest args)
;;     (apply func-a (permute! (multiple-value-list (funcall func-b args)) perm))))

;; (defun compose-and-compile (func-a func-b perm)
;;   (declare (type function func-a func-b)
;;	   (type permutation perm))
;;   (let ((syms (loop :for i :from 0 :below (permutation-size perm)
;;		 :collect (gensym))))
;;     (compile-and-eval
;;      `(lambda (&rest args)
;;	(destructuring-bind (,@syms &rest rest) (multiple-value-list (apply ,func-b args))
;;	  (apply ,func-a (append (list ,@(permute! syms perm)) rest)))))))

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
    (values seq (with-no-init-checks (make-instance 'permutation-action :store perm :size (length perm))))))
