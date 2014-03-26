(in-package #:matlisp)

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
      (iter (for node in-dlist (hnode-children z))
	    (counting t into i)
	    (setf (hnode-parent node) nil)
	    (finally (incf (number-of-trees fib) i)))
      (setf (root fib) (dappend! (hnode-children z) (root fib)))
      (setf (hnode-children z) nil
	    (hnode-degree z) 0))
    ;;
    (when (> (number-of-trees fib) 1)
      ;;consolidate
      (let ((an (make-array (+ 2 (integer-length (number-of-elements fib))) :initial-element nil)))
	(with-fslots ((ord heap-order)) fib
	    (iter (for w on-dlist (root fib))
		  (with fw = (list t t))
		  ;;This hack allows for destructive updates. See the iter clause/
		  (setf (second fw) (second w))
		  (iter (with x = w)
			(with d = (hnode-degree (cddr x)))
			(while (aref an d))
			;;
			(let ((y (aref an d)))
			  (when (ord (hnode-key (cddr y)) (hnode-key (cddr x)))
			    (rotatef y x))
			  ;;fib-heap-link
			  (let ((y.node (dpop y)))
			    (setf (root fib) y
				  (hnode-parent y.node) (cddr x)
				  (hnode-mark? y.node) nil)
			    (decf (number-of-trees fib))
			    (setf (hnode-children (cddr x)) (dappend! (hnode-dcons y.node) (hnode-children (cddr x))))))
			(setf (aref an d) nil)
			;;
			(incf d)
			(setf (hnode-degree (cddr x)) d)
			;;
			(finally (setf (aref an d) x)))
		  (setq w fw))
	    ;;update min
	    (iter (for rot on-dlist (root fib))
		  (with fmin = nil)
		  (when (or (null fmin) (ord (hnode-key (cddr rot)) (hnode-key (cddr fmin))))
		    (setf fmin rot))
		  (finally (setf (root fib) fmin))))))
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
