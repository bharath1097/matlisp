(in-package :matlisp)

(define-condition permutation-error (generic-error)
  ((message :reader message :initform "Object is not a permutation."))
  (:documentation "Object is not a permutation."))

;;---------------------------------------------------------------;;
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
		 (cond
		   ((or (< x 0) (member x `(,(aref sort u-b) ,(aref sort l-b) ,mid)))
		    (error 'permutation-error))
		   ((= midx l-b)
		    (when (> x (aref sort u-b))
		      (loop
			 with sidx = (+ midx (if (> x mid) 0 1))
			 for i downfrom (- len 1) to sidx
			 do (setf (aref sort (+ i 1)) (aref sort i))
			 finally (setf (aref sort sidx) x))))
		   ((< x mid) (insert-ele midx u-b))
		   ((> x mid) (insert-ele l-b midx)))
		 sort)))
      (insert-ele l-b u-b))))

(defun cycle-p (perm)
  "Does a sorting operation to check for duplicate elements in
   the cycle representation of a permutation."
  (let* ((len (length perm))
	 (sort (allocate-index-store len -1)))
    (dotimes (i len t)
      (handler-case (insert-element (aref perm i) sort 0 i)
	(permutation-error () (return nil))))))

(defun action-p (arr)
  "Checks if ARR is a possible permutation vector.  A permutation pi
   is characterized by a vector containing the indices from 0,...,
   @function{length}(@arg{perm})-1 in some order."
  (declare (type (index-array *) arr))
  (let ((s-arr (sort (copy-seq arr) #'<)))
    (dotimes (i (length s-arr) t)
      (unless (= i (aref s-arr i))
	(return nil)))))

(defun action->cycle (per)
  ;;Caution: will go into an infinite loop if object is not proper.
  "This function obtains the canonical cycle representation
   of a permutation. The first argument is the action of the
   permutation on the array #(0 1 2 3 ..).
   \"Canonical\" may be a bit of an overstatement; this is the way
   S_n was presented by Van der Waerden."
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

(defclass permutation ()
  ((representation :accessor r-value
		   :initarg :r-value)
   (group-rank :accessor group-rank
	       :type index-type)))

(defclass permutation-cycle (permutation)
  ((representation :type cons)))

(defmethod initialize-instance :after ((per permutation-cycle) &rest initargs)
  (declare (ignore initargs))
  (let ((cls 0))
    (unless (dolist (cyc (r-value per) t)
	      (unless (cycle-p cyc)
		(return nil))
	      (setf cls (max cls (reduce #'max cyc))))
      (error 'permutation-error))
    (setf (group-rank per) (1+ cls))))

(defclass permutation-action (permutation)
  ((:representation :type (index-array *))))

(defmethod initialize-instance :after ((per permutation-action) &rest initargs)
  (declare (ignore initargs))
  (unless (action-p (r-value per))
    (error 'permutation-error)))

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