(in-package #:matlisp-utilities)

;;These functions are used all over the place inside Matlisp's macros.
(eval-when (:compile-toplevel :load-toplevel :execute)

(declaim (inline id))
(defun id (x) x)

(defun ieql (&rest args)
  (loop :for ele :in (cdr args)
     :do (unless (eql (car args) ele)
	   (return nil))
     :finally (return t)))

(declaim (inline vectorify))
(defun vectorify (seq n &optional (element-type t))
  (declare (type (or vector list) seq))
  (etypecase seq
    (cons
     (let ((ret (make-array n :element-type element-type)))
       (loop :for i :of-type fixnum :from 0 :below n
	  :for lst := seq :then (cdr lst)
	  :do (setf (aref ret i) (car lst))
	  :finally (return ret))))
    (vector
     (let ((ret (make-array n :element-type element-type)))
       (loop :for i :of-type fixnum :from 0 :below n
	  :for ele :across seq	    
	  :do (setf (aref ret i) ele)
	  :finally (return ret))))))

(defun zipsym (lst)
  (zip (loop :repeat (length lst)
	  :collect (gensym))
       lst))

(defun list-eq (a b &optional (test #'eq))
  (if (or (atom a) (atom b)) (eq a b)
      (and (funcall test (car a) (car b)) (list-eq (cdr a) (cdr b) test))))

(defun remmeth (func spls &optional quals)
  (let ((meth (find-method func quals (mapcar #'(lambda (x) (if (consp x) x (find-class x))) spls) nil)))
    (when meth
      (remove-method func meth)
      meth)))
      
(defun setadd (lst a &optional (test #'eq))
  (if (null lst) (list a)
      (if (funcall test (car lst) a)
	  (cons a (cdr lst))
	  (cons (car lst) (setadd (cdr lst) a test)))))

(defun setrem (lst a &optional (test #'eq))
  (unless (null lst)
    (if (funcall test (car lst) a)
	(cdr lst)
	(cons (car lst) (setrem (cdr lst) a test)))))

(defun set-eq (a b &key (test #'eql))
  (and (loop :for ele :in a
	  :do (unless (member ele b :test test)
		(return nil))
	  :finally (return t))
       (loop :for ele :in b
	  :do (unless (member ele a :test test)
		(return nil))
	  :finally (return t))))

(declaim (inline copy-n))
(defun copy-n (vec lst n)
  (declare (type vector vec)
	   (type list lst)
	   (type fixnum n))
  (loop :for i :of-type fixnum :from 0 :below n
     :for vlst := lst :then (cdr vlst)
     :do (setf (car vlst) (aref vec i)))
  lst)

(defun getcons (lst sym)
  (if (atom lst) nil
      (if (eq (car lst) sym)
	  (list lst)
	  (append (getcons (car lst) sym) (getcons (cdr lst) sym)))))

(defun mapcons (func lst keys)
  (if (atom lst) lst
      (let ((tlst (if (member (car lst) keys)
			(funcall func lst)
			lst)))
	(if (atom tlst) tlst
	    (mapcar #'(lambda (x) (mapcons func x keys)) tlst)))))

(defun find-tag (lst tag)
  (let ((car (car lst)))
    (if (atom car)
	(if (or (null car) (eq car tag))
	    (cadr lst)
	    (find-tag (cdr lst) tag))
	(or (find-tag car tag) (find-tag (cdr lst) tag)))))

(defun ensure-args (args)
  (if (null args) t
      (and (symbolp (car args)) (ensure-args (cdr args)))))

(defun repsym (lst sym rep)
  (if (atom lst) lst
      (if (and (symbolp (car lst)) (eq (car lst) sym))
	  (cons rep (repsym (cdr lst) sym rep))
	  (cons (repsym (car lst) sym rep) (repsym (cdr lst) sym rep)))))

(defun findsym (lst sym)
  (if (atom lst) (eq lst sym)
      (or (findsym (car lst) sym) (findsym (cdr lst) sym))))

(declaim (inline slot-values))
(defun slot-values (obj slots)
  "
  Returns the slots of the @arg{obj} corresponding to symbols in the list @arg{slots}.

  Example:
  @lisp
  > (defstruct obj a b)
  => OBJ

  > (let ((thing (make-obj :a 1 :b 2)))
      (slot-values thing '(a b)))
  => 1 2
  @end lisp
  "
  (values-list
   (loop :for slt :in slots
      :collect (slot-value obj slt))))

(declaim (inline linear-array-type))
(defun linear-array-type (type-sym &optional (size '*))
  "
  Creates the list representing simple-array with type @arg{type-sym}.

  Example:
  @lisp
  > (linear-array-type 'double-float 10)
  => (simple-array double-float (10))
  @end lisp
  "
  `(simple-array ,type-sym (,size)))

(declaim (inline ensure-list))
(defun ensure-list (lst)
  "
  Ensconses @arg{lst} inside a list if it is an atom.

  Example:
  @lisp
  > (ensure-list 'a)
  => (a)
  @end lisp
  "
  (if (listp lst) lst `(,lst)))

(defun cut-cons-chain! (lst test)
  "
  Destructively cuts @arg{lst} into two parts, at the element where the function
  @arg{test} returns a non-nil value.

  Example:
  @lisp
  > (let ((x (list 3 5 2 1 7 9)))
      (values-list (cons x (multiple-value-list (cut-cons-chain! x #'evenp)))))
  => (3 5) (3 5) (2 1 7 9)
  @end lisp
  "    
  (declare (type list lst))
  (labels ((cut-cons-chain-tin (lst test parent-lst)
	     (cond
	       ((null lst) nil)
	       ((funcall test (cadr lst))
		(let ((keys (cdr lst)))
		  (setf (cdr lst) nil)
		  (values parent-lst keys)))
	       (t (cut-cons-chain-tin (cdr lst) test parent-lst)))))
    (cut-cons-chain-tin lst test lst)))

(declaim (inline zip))
(defun zip (&rest args)
  "
  Zips the elements of @arg{args}.

  Example:
  @lisp
  > (zip '(2 3 4) '(a b c) '(j h c s))
  => ((2 A J) (3 B H) (4 C C))
  @end lisp
  "
  (apply #'map 'list #'list args))

(defun recursive-append (&rest lsts)
  "
  Appends lists in a nested manner, mostly used to bring in the charm of
  non-lispy languages into macros.

  Basically does
  @lisp
  (reduce
    #'(lambda (x y)
        (if (null x)
          (if (typep (car y) 'symbol) y (car y))
            (append x (if (null y) nil
                        (if (typep (car y) 'symbol) `(,y) y)))))
    lsts :from-end t)
  @end lisp

  Examples:
  @lisp
  > (recursive-append
      '(let ((x 1)))
      '(+ x 2))
  => (LET ((X 1))
       (+ X 2))

  > (recursive-append
      '(let ((x 1)))
      '((let ((y 2))
          (setq y 3))
        (let ((z 2))
          z)))
  => (LET ((X 1))
       (LET ((Y 2))
         (SETQ Y 3))
       (LET ((Z 2))
         Z))

  > (recursive-append
      nil
      '((let ((x 1)) x)
        (progn (+ 1 2))))
  => (LET ((X 1))
       X)

  > (recursive-append nil '(let ((x 1)) x))
  => (LET ((X 1))
       X)
  @end lisp
  "
  (labels ((bin-append (x y)
	     (if (null x)
		 (if (typep (car y) 'symbol) y (car y))
		 (append x (if (null y) nil
			       (if (typep (car y) 'symbol) `(,y) y))))))
    (reduce #'bin-append lsts :from-end t)))

(defun unquote-args (lst args)
  "
  Makes a list suitable for use inside macros (sort-of), by building a
  new list quoting every symbol in @arg{lst} other than those in @arg{args}.
  CAUTION: DO NOT use backquotes!

  @lisp
  Example:
  > (unquote-args '(+ x y z) '(x y))
  => (LIST '+ X Y 'Z)

  > (unquote-args '(let ((x 1)) (+ x 1)) '(x))
  => (LIST 'LET (LIST (LIST X '1)) (LIST '+ X '1))
  @end lisp
  "
  (labels ((replace-atoms (lst ret)
	     (cond
	       ((null lst) (reverse ret))
	       ((atom lst)
		(let ((ret (reverse ret)))
		  (rplacd (last ret) lst)
		  ret))
	       ((consp lst)
		(replace-atoms (cdr lst) (let ((fst (car lst)))
					   (cond 
					     ((atom fst)
					      (if (member fst args)
						  (cons fst ret)
						  (append `(',fst) ret)))
					     ((consp fst)
					      (cons (replace-lst fst nil) ret))))))))
	   (replace-lst (lst acc)
	     (cond
	       ((null lst) acc)
	       ((consp lst)
		(if (eq (car lst) 'quote)
		    lst
		    (cons 'list (replace-atoms lst nil))))
	       ((atom lst) lst))))
    (replace-lst lst nil)))

(defun flatten (x)
  "
  Returns a new list by collecting all the symbols found in @arg{x}.
  Borrowed from Onlisp.

  Example:
  @lisp
  > (flatten '(let ((x 1)) (+ x 2)))
  => (LET X 1 + X 2)
  @end lisp
  "
  (labels ((rec (x acc)
	     (cond ((null x) acc)
		   ((atom x) (cons x acc))
		   (t (rec
		       (car x)
		       (rec (cdr x) acc))))))
    (rec x nil)))

(defun list-dimensions (lst)
  "
  Returns the dimensions of the nested list @arg{lst}, by finding the length
  of the immediate list, recursively. This does not ensure the uniformity of
  lengths of the lists.

  Example:
  @lisp
  > (list-dimensions '((1 2 3) (4 5 6)))
  => (2 3)
  @end lisp
  "
  (declare (type list lst))
  (labels ((lst-tread (idx lst)
	     (if (null lst) (reverse idx)
		 (progn
		   (setf (car idx) (length lst))
		   (if (consp (car lst))
		       (lst-tread (cons 0 idx) (car lst))
		       (reverse idx))))))
    (lst-tread (list 0) lst)))

(defun compile-and-eval (source)
  "
  Compiles and evaluates the given @arg{source}.  This should be
  an ANSI compatible way of ensuring method compilation."
  (funcall (compile nil `(lambda () ,source))))

)

