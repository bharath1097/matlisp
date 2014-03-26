(in-package #:matlisp-utilities )
;;
(setf *print-circle* t)

(defun dcons (obj)
  (let ((lst (list* nil nil obj)))
    (setf (first lst) lst
	  (second lst) lst)
    lst))

(defmacro dpush (obj dll &environment env)
  (multiple-value-bind (dummies vals new setter getter) (get-setf-expansion dll env)
    (when (cdr new)
      (error "Can't expand this."))
    (with-gensyms (left right ele ncon)
      (let ((new (car new)))
	`(let* (,@(zip dummies vals)
		(,new ,getter)
		(,ncon (dcons ,obj)))
	   (when ,new
	     (destructuring-bind (,left ,right . ,ele) ,new
	       (declare (ignore ,right ,ele))
	       (setf (first ,ncon) ,left
		     (second ,left) ,ncon
		     (second ,ncon) ,new
		     (first ,new) ,ncon)))
	   (setf ,new ,ncon)
	   ,setter)))))

(defmacro dpop (dll &environment env)
  (multiple-value-bind (dummies vals new setter getter) (get-setf-expansion dll env)
    (when (cdr new)
      (error "Can't expand this."))
    (with-gensyms (left right ele)
      (let ((new (car new)))
	`(let* (,@(zip dummies vals)
		(,new ,getter))
	   (when ,new
	     (destructuring-bind (,left ,right . ,ele) ,new
	       (prog1 ,ele
		 ;;update cons cell
		 (setf (first ,new) ,new
		       (second ,new) ,new)
		 ;;update place
		 (if (and (eql ,new ,left) (eql ,new ,right))
		     (setf ,new nil)
		     (setf (second ,left) ,right
			   (first ,right) ,left
			   ,new ,right))
		 ,setter))))))))

(defun dlist (&rest objs)
  (let* ((rev (reverse objs))
	 (ret (dcons (car rev))))
    (loop :for ele :in (cdr rev)
       :do (dpush ele ret))
    ret))

(defun dappend! (&rest dlsts)
  (loop :for se :in (cdr dlsts)
     :with ft := (car dlsts)
     :do (progn
	   (rotatef (first ft) (first se))
	   (rotatef (second (first ft)) (second (first se))))
     :finally (return ft)))

(iter:defmacro-clause (FOR var IN-DLIST v)
  "All unique elements in the dlist."
  (let ((dlist (gensym))
	(clist (gensym)))
    `(progn
       (with ,dlist = ,v)
       (while ,dlist)
       (for ,clist initially ,dlist then (or (let ((rgt (second ,clist)))
					       (and (not (eql rgt ,dlist)) rgt))
					     (terminate)))
       (for ,var = (cddr ,clist)))))

(iter:defmacro-clause (FOR clist ON-DLIST v)
  "All unique elements on the dlist."
  (with-gensyms (dlist end)
    `(progn
       (with ,dlist = ,v)
       (while ,dlist)
       (with ,end = (first ,dlist))
       (for ,clist initially ,dlist then (or (let ((rgt (second ,clist)))
					       (prog1 (and ,end rgt)
						 (when (eql rgt ,end)
						   (setf ,end nil))))
					     (terminate))))))
;;
(defstruct hnode
  (degree 0 :type fixnum)
  (mark? nil :type boolean)
  key parent children
  dcons)

(defmethod print-object ((nd hnode) stream)
  (print-unreadable-object (nd stream :type t)
    (format stream "key: ~A, degree: ~A, mark?: ~A" (hnode-key nd) (hnode-degree nd) (hnode-mark? nd))))
;;
(defclass fib-heap ()
  ((root :initform nil :accessor root)
   (number-of-trees :initform 0 :accessor number-of-trees)
   (number-of-elements :initform 0 :accessor number-of-elements)
   (heap-order :initarg :heap-order :initform #'<)))

(defmethod print-object ((fib fib-heap) stream)
  (print-unreadable-object (fib stream :type t)
    (format stream "size: ~A, trees: ~A" (number-of-elements fib) (number-of-trees fib))))

(defun fib-min (fib)
  (when (root fib)
    (destructuring-bind (left right . ele) (root fib)
      (declare (ignore left right))
      (hnode-key ele))))

(defun fib-insert (obj fib)
  (let ((pmin (fib-min fib))
	(node (make-hnode :key obj :parent nil)))
    (setf (hnode-dcons node) (dpush node (root fib)))
    (with-fslots ((ord heap-order)) fib
      (unless (and pmin (ord obj pmin))
	(setf (root fib) (second (root fib)))))
    (incf (number-of-elements fib))
    (incf (number-of-trees fib))
    node))

(defun fib-extract-min (fib)
  (let ((z (dpop (root fib))))
    ;;move the children of z into root
    (when z
      (decf (number-of-elements fib))
      (decf (number-of-trees fib))
      (let ((rt (root fib)))
	(iter (for node in-dlist (hnode-children z))
	      (counting t into i)
	      (progn
		(setf (hnode-parent node) nil)
		(dpush node rt))
	      (finally (progn
			 (setf (root fib) rt)
			 (incf (number-of-trees fib) i))))))
    (when (> (number-of-elements fib) 1)
      ;;consolidate
      (let ((an (make-array (+ 2 (integer-length (number-of-elements fib))) :initial-element nil))
	    (stack nil))
	  (with-fslots ((ord heap-order)) fib
	    (iter (for w on-dlist (root fib))
		  (iter (with x = w)
			(with d = (hnode-degree (cddr x)))
			(while (aref an d))
			;;
			(let ((y (aref an d)))
			  (when (ord (hnode-key (cddr y)) (hnode-key (cddr x)))
			    (rotatef y x))
			  ;;fib-heap-link
			  (let ((y.node (cddr y)))
			    (setf (hnode-parent y.node) (cddr x)
				  (hnode-mark? y.node) nil)
			    (dpush y.node (hnode-children (cddr x)))
			    (decf (number-of-trees fib)))
			  (push y stack)
			  ;;
			  (setf (aref an d) nil)
			  (incf d)
			  (setf (hnode-degree (cddr x)) d))
			;;
			(finally (setf (aref an d) x)))))
	  ;;destructive update
	  (let ((rt (root fib)))
	    (iter (for ele in stack)
		  (dpop ele) (setf rt ele))
	    (setf (root fib) rt)))
	;;update min
	(with-fslots ((ord heap-order)) fib
	  (iter (for rot on-dlist (root fib))
		(with fmin = nil)
		(when (or (null fmin) (ord (hnode-key (cddr rot)) (hnode-key (cddr fmin))))
		  (setf fmin rot))
		(finally (setf (root fib) fmin)))))
    ;;
    (and z (hnode-key z))))
;;
(defun fib-decrease-key (node key fib)
  (with-fslots ((ord heap-order)) fib
    (when (ord (hnode-key node) key)
      (error 'invalid-value :message "new key is greater than the current."))
    (labels ((cut (x y)
	       (decf (hnode-degree y))
	       (let ((tmp (hnode-dcons x)))
		 (dpop tmp)
		 (setf (hnode-children y) tmp))
	       (setf (hnode-parent x) nil
		     (hnode-mark? x) nil)
	       (dappend! (hnode-dcons x) (root fib)))
	     (ccut (y)
	       ;;cascading cut
	       (when-let (z (hnode-parent y))
		 (if (hnode-mark? y)
		     (progn
		       (cut y z)
		       (ccut z))
		     (setf (hnode-mark? y) t)))))
      (setf (hnode-key node) key)
      (let ((y (hnode-parent node)))
	(when (and y (ord key (hnode-key y)))
	  (cut node y)
	  (ccut y)))
      ;;update min
      (when (ord key (fib-min fib))
	(setf (root fib) (hnode-dcons node)))))
  node)
;;
(defun fib-delete (node fib)
  (let ((oldf (slot-value fib 'heap-order)))
    (labels ((nord (x y)
	       (cond
		 ((and (eql x (hnode-key node)) (eql y (hnode-key node))) nil)
		 ((eql x (hnode-key node)) t)
		 (t (funcall oldf x y)))))
      (setf (slot-value fib 'heap-order) #'(lambda (x y) (nord x y)))
      (fib-decrease-key node (hnode-key node) fib)
      (fib-extract-min fib)
      (setf (slot-value fib 'heap-order) oldf)))
  node)
;;
;; (let ((fib (make-instance 'fib-heap)))
;;   (fib-insert 0 fib)
;;   (fib-extract-min fib))

;; (progn
;;   (setf *fib* (make-instance 'fib-heap))
;;   (setf *node* (fib-insert 0 *fib*))
;;   (loop :for i from 0 :below 100 :do (fib-insert (- (random 100) 50) *fib*))
;;   (loop :for i from 0 :below 100 :do (unless (= i (fib-extract-min *fib*)) (print "noo!"))))
