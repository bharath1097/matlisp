(in-package #:matlisp-utilities)

(defmacro looped-mapcar ((func lst) &rest body)
  "
  A macro to use when caught between the efficiency of imperative looping, and
  the elegance of the mapcar (in a dozen places).

  Works by collecting references to the symbol @arg{func} and replacing them with a varible
  inside a loop. Note that although we traverse through the list only once, the collected
  lists aren't freed until the macro is closed.

  Example:
  @lisp
  > (macroexpand-1
      `(looped-mapcar (lmap '(1 2 3 4 5 6 7 8 9 10))
			(cons (lmap #'even) (lmap #'(lambda (x) (+ x 1))))))
  => (LET ((#:|lst1118| '(1 2 3 4 5 6 7 8 9 10)))
	(LOOP FOR #:|ele1117| IN #:|lst1118|
	    COLLECT (FUNCALL #'(LAMBDA (X) (+ X 1))
			     #:|ele1117|) INTO #:|collect1116|
	    COLLECT (FUNCALL #'EVEN #:|ele1117|) INTO #:|collect1115|
	    FINALLY (RETURN (PROGN (CONS #:|collect1115| #:|collect1116|)))))
  @end lisp
  "
  (let ((ret nil))
    (labels ((collect-funcs (code tf-code)
	       (cond
		 ((null code)
		  (reverse tf-code))
		 ((atom code)
		  (let ((ret (reverse tf-code)))
		    (rplacd (last ret) code)
		    ret))
		 ((consp code)
		  (let ((carcode (car code)))
		    (cond
		      ((and (consp carcode)
			    (eq (first carcode) func))
		       (assert (null (cddr carcode)) nil 'invalid-arguments
			       :message "The mapper only takes one argument.")
		       (let ((col-sym (gensym "collect")))
			 (push `(,col-sym ,(second carcode)) ret)
			 (collect-funcs (cdr code) (cons col-sym tf-code))))
		      ((consp carcode)
		       (collect-funcs (cdr code) (cons (collect-funcs carcode nil) tf-code)))
		      (t
		       (collect-funcs (cdr code) (cons carcode tf-code)))))))))
      (let ((tf-code (collect-funcs body nil))
	    (ele-sym (gensym "ele"))
	    (lst-sym (gensym "lst")))
	(if (null ret)
	    `(progn
	       ,@tf-code)
	    `(let ((,lst-sym ,lst))
	       (loop :for ,ele-sym :in ,lst-sym
		  ,@(loop :for decl :in ret
		       :append `(collect (funcall ,(second decl) ,ele-sym) into ,(first decl)))
		  :finally (return
			     (progn
			       ,@tf-code)))))))))

(defmacro let-rec (name arglist &rest code)
  "
  This works implements the named let used in Scheme for recursion
  using labels.

  Example:
  @lisp
  > (macroexpand-1
      `(let-rec rev ((x '(1 2 3 4)) (ret nil))
	 (if (null x) ret
	   (rev (cdr x) (cons (car x) ret)))))
  => (LABELS ((REV (X RET)
		(IF (NULL X)
		   RET
		  (REV (CDR X) (CONS (CAR X) RET)))))
       (REV '(1 2 3 4) NIL))

  > (let-rec rev ((x '(1 2 3 4)) (ret nil))
       (if (null x) ret
	  (rev (cdr x) (cons (car x) ret))))
  => (4 3 2 1)
  @end lisp
  "
  (let ((init (mapcar #'second arglist))
	(args (mapcar #'first arglist)))
    `(labels ((,name (,@args)
		,@code))
       (,name ,@init))))
;;

;;Circular buffer
(defstruct circular-buffer
  (store #() :type vector)
  (idx 0 :type fixnum)
  (count 0 :type fixnum))

(defun make-cbuf (size)
  (declare (type fixnum size))
  (make-circular-buffer :store (make-array size)))

(defun push-cbuf (ele buf)
  (declare (type circular-buffer buf))
  (with-slots (store idx count) buf
    (let* ((len (length store))
	   (nidx (mod (1- idx) len)))
      (when (< count len)
	(incf count))
      (setf (aref store nidx) ele
	    idx nidx)))
  buf)

(defun push-at-end-cbuf (ele buf)
  (declare (type circular-buffer buf))
  (with-slots (store idx count) buf
    (let* ((len (length store)))
      (if (= count len)
	  (setf idx (mod (1+ idx) len))
	  (incf count))
      (setf (aref store (mod (+ idx count -1) len)) ele)))
  buf)

(defun pop-cbuf (buf)
  (declare (type circular-buffer buf))
  (with-slots (store idx count) buf
    (if (= count 0) nil
	(prog1 (aref store idx)
	  (setf idx (mod (1+ idx) (length store))
		count (1- count))))))

(defun pop-from-end-cbuf (buf)
  (declare (type circular-buffer buf))
  (with-slots (store idx count) buf
    (if (= count 0) nil	
	(let* ((len (length store)))
	  (prog1 (aref store (mod (+ idx count -1) len))
	    (decf count))))))
;;
