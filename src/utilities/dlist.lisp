(in-package #:matlisp-utilities )

;; (setf *print-circle* t)
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

(definline drdc (buf) (first buf))
(definline dcdr (buf) (second buf))
(definline dcar (buf) (cddr buf))

(defun dappend! (&rest dlsts)
  (let ((dlsts (remove-if #'null dlsts)))
    (loop :for se :in (cdr dlsts)
       :with ft := (car dlsts)
       :do (progn
	     (rotatef (first ft) (first se))
	     (rotatef (second (first ft)) (second (first se))))
       :finally (return ft))))

;;
(defmacro-clause (FOR clist ON-DLIST v &optional IN-REVERSE r? UNTIL end-lst)
  "All unique elements on the dlist."
  (with-gensyms (dlist end nxt)
    `(progn
       (with ,dlist = ,v)
       (while ,dlist)
       (with ,end = ,(or end-lst (if r? `(second ,dlist) `(first ,dlist))))
       (for ,clist initially ,dlist then (if (or (null ,end) (eql ,clist ,end)) (terminate)
					     (let ((,nxt ,(if r? `(first ,clist) `(second ,clist))))
					       (when (null ,nxt) (terminate))
					       (when (eql ,nxt ,end) (setf ,end nil))
					       ,nxt))))))

(defmacro-clause (FOR var IN-DLIST v &optional IN-REVERSE r? UNTIL end-lst)
  "All unique elements in the dlist."
  (with-gensyms (clist)
    `(progn
       (for ,clist on-dlist ,v in-reverse ,r? until ,end-lst)
       (for ,var = (cddr ,clist)))))
