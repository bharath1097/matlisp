(in-package :matlisp)

(define-condition permutation-error (generic-error)
  ((message :reader message :initform "Object is not a permutation."))
  (:documentation "Object is not a permutation."))

;;Class definitions----------------------------------------------;;
(defclass permutation ()
  ((representation :accessor repr
		   :initarg :repr)
   (group-rank :accessor group-rank
	       :type index-type)))
;;
(defclass permutation-cycle (permutation)
  ((representation :type cons)))

(defmethod initialize-instance :after ((per permutation-cycle) &rest initargs)
  (declare (ignore initargs))
  (let ((cls 0))
    (declare (type index-type cls))
    (unless (very-quickly
	      (dolist (cyc (r-value per) t)
		(unless (cycle-p cyc)
		  (return nil))
		(setf cls (max cls (idx-max cyc)))))
      (error 'permutation-error))
    (setf (group-rank per) (the index-type (1+ cls)))))
;;
(defclass permutation-action (permutation)
  ((representation :type (index-array *))))

(defmethod initialize-instance :after ((per permutation-action) &rest initargs)
  (declare (ignore initargs))
  (let ((act (r-value per)))
    (declare (type (index-array *) act))
    (unless (action-p act)
      (error 'permutation-error))
    (setf (group-rank per) (idx-max act))))

;;Conversions and validation-------------------------------------;;
(defun insert-element (x sort l-b u-b)
  "Does a binary-esque sort to keep track of elements in
   a permutation, in descending order. If there are duplicates
   of X in sort between L-B and U-B (both inclusive), or if X < 0,
   then throws a PERMUTATION-ERROR."
  (declare (type index-type x l-b u-b)
	   (type (index-array *) sort))
  (let* ((len u-b))
    (labels ((insert-ele (l-b u-b)
	       (declare (type index-type l-b u-b))
	       (let* ((midx (+ l-b (floor (- u-b l-b) 2)))
		      (mid (aref sort midx)))
		 (declare (type index-type midx mid))
		 (cond
		   ((or (< x 0) (member x `(,(aref sort u-b) ,(aref sort l-b) ,mid)))
		    (error 'permutation-error))
		   ((= midx l-b)
		    (when (> x (aref sort u-b))
		      (very-quickly
			(loop
			   with sidx of-type index-type = (+ midx (if (> x mid) 0 1))
			   for i of-type index-type downfrom (1- len) to sidx
			   do (setf (aref sort (+ i 1)) (aref sort i))
			   finally (setf (aref sort sidx) x)))))
		   ((< x mid) (insert-ele midx u-b))
		   ((> x mid) (insert-ele l-b midx)))
		 sort)))
      (insert-ele l-b u-b))))

(defun cycle-new-p (perm)
  "Does a sorting operation to check for duplicate elements in
   the cycle representation of a permutation."
  (declare (type (index-array *) perm))
  (let* ((len (length perm))
	 (sort (very-quickly (sort (copy-seq perm) #'<))))
    (declare (type (index-array *) sort)
	     (type index-type len))
    (very-quickly
      (loop for i of-type index-type from 1 below len
	 when (= (aref sort i) (aref sort (1- i)))
	 do (return nil)
	 finally (return t)))))

(defun action-p (act)
  "Checks if ARR is a possible permutation vector.  A permutation pi
   is characterized by a vector containing the indices from 0,...,
   @function{length}(@arg{perm})-1 in some order."
  (declare (type (index-array *) act))
  (let* ((len (length act))
	 (sort (very-quickly (sort (copy-seq act) #'<))))
    (declare (type (index-array *) sort)
	     (type index-type len))
    (very-quickly
      (loop for i of-type index-type from 0 below len
	 unless (= (aref sort i) i)
	 do (return nil)
	 finally (return t)))))

(defun action->cycle (act)
  ;;Caution: will go into an infinite loop if object is not proper.
  "
   This function obtains the canonical cycle representation
   of a permutation. The first argument is the action of the
   permutation on the array #(0 1 2 3 ..).
   \"Canonical\" may be a bit of an overstatement; this is the way
   S_n was presented by Van der Waerden.
"
  (declare (type permutation-action per))
  (mlet*
   ((arr (r-value per) :type (index-array *)))
   (labels ((find-cycle (arr x0)
	      "This function obtains a permutation cycle starting from x_0.
               The first argument is the action of the permutation on the
               array #(0 1 2 ..)"
	      (declare (type (index-array *) arr)
		       (type index-type x0))
	      (if (= (aref arr x0) x0) (values #() nil)
		  (destructuring-bind (n lst)
		      (do ((i 0 (+ i 1))
			   (x x0 (aref arr x))
			   (ret nil (cons x ret))
			   (count 0 (+ count (if (= x x0) 1 0))))
			  ((and (= count 1) (= x x0)) (list i ret)))
		    (values (make-array n :element-type 'index-type :initial-contents lst) lst))))
	    (cycle-walk (cyc ignore)
	      (declare (optimize (speed 3) (safety 0)))
	      (let ((x0 (find-if-not #'(lambda (x) (member x ignore)) arr)))
		(if (null x0) cyc
		    (multiple-value-bind (cnew clst) (find-cycle arr x0)
		      (cycle-walk (if (null clst) cyc (cons cnew cyc))
				  (nconc ignore (if (null clst) (list x0) clst))))))))
     (cycle-walk nil nil))))
;;---------------------------------------------------------------;;


(defun cycles->action (cyc)
  )

;;
(defun apply-cycle! (seq cyc)
  (declare (type (index-array *) cyc)
	   (type (vector * *) seq))
  (unless (cycle-p cyc)
    (error 'permutation-error))
  (when (> (length cyc) 1)
    (let ((xl (aref seq (aref cyc (- (length cyc) 1)))))
      (loop for i downfrom (- (length cyc) 1) to 0
	 do (setf (aref seq (aref cyc i))
		  (if (= i 0) xl
		      (aref seq (aref cyc (- i 1))))))))
  seq)
    
(defun permute! (seq cycs)
  (unless (or (null cycs) (= (length seq) 0))
    (dolist (cyc cycs)
      (apply-cycle! seq cyc)))
  seq)

(defun arg-perm (func cycs)
  (if (null cycs)
      func
      (lambda (&rest args)
	(let ((argvec (permute! (apply #'vector args) cycs)))
	  (apply func (loop for i from 0 below (length argvec)
			 collect (aref argvec i)))))))

(defun compose (func func)

;; (defun compose (..)
;;   )

(defun seqrnd (seq)
  "Randomize the elements of a sequence. Destructive on SEQ."
  (sort seq #'> :key #'(lambda (x) (random 1.0))))

;;

(defun allocate-unit-permutation (n)
  (declare (type fixnum n))
  (let ((ret (allocate-index-store n)))
    (declare (type (index-array *) ret))
    (very-quickly
      (loop
	 for i of-type index-type from 0 below n
	 do (setf (aref ret i) i)))
    ret))

(defun sort-permute (seq predicate)
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
	 (perm (allocate-unit-permutation len)))
    (declare (type index-type len)
	     (type (index-array *) perm))
    (labels ((qsort-bounds (lb ub)
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
		     (when (> (- ub ele-idx) 2)
		       (qsort-bounds (1+ ele-idx) ub))
		     (when (> (- ele-idx lb) 1)
		       (qsort-bounds lb ele-idx))))))
      (qsort-bounds 0 len)
      (values seq perm))))

(quicksort-with-action (idxv 10 9 8 7 6 5 4 3 2 1) #'<)
