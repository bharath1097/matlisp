(in-package #:matlisp-utilities)

;;These functions are used all over the place inside Matlisp's macros.
(eval-when (:compile-toplevel :load-toplevel :execute)

(declaim (inline id))
(defun id (x) x)

(defun pophash (key hash-table &optional default)
  (multiple-value-bind (value existsp) (gethash key hash-table default)
    (when existsp (remhash key hash-table))
    (values value existsp)))

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

(declaim (inline copy-n))
(defun copy-n (vec lst n)
  (declare (type vector vec)
	   (type list lst)
	   (type fixnum n))
  (loop :for i :of-type fixnum :from 0 :below n
     :for vlst := lst :then (cdr vlst)
     :do (setf (car vlst) (aref vec i)))
  lst)

(defun remmeth (func spls &optional quals)
  (let ((meth (find-method func quals (mapcar #'(lambda (x) (if (consp x) x (find-class x))) spls) nil)))
    (when meth
      (remove-method func meth)
      meth)))

(defun maptree-if (predicate transformer tree)
  "
  Returns a new tree by recursively calling @arg{transformer} on sub-trees which satisfy the @arg{predicate}.
  @arg{predicate} : tree -> boolean
  @arg{transformer}: tree -> (or tree atom) *control
  If the transformer returns a @arg{control} function, then the tree returned by
  the transformer is replaced in-turn by the result of:
  > (funcall @arg{control} #'(lambda (x) (maptree-if @arg{predicate} @arg{transformer} x)) transformed-tree)
  , otherwise it is left as it is.

  Example:
  @lisp
  > (maptree-if #'(λ (x) (and (consp x) (eq (car x) 'ping)))
                #'(λ (x) `(pong ,@(cdr x)))
                '(progn (ping (ping (ping 1)))))
  >= (PROGN (PONG (PING (PING 1))))
  > (maptree-if #'(λ (x) (and (consp x) (eq (car x) 'ping)))
                #'(λ (x) (values `(pong ,@(cdr x)) #'mapcar))
                '(progn (ping (ping (ping 1)))))
  >= (PROGN (PONG (PONG (PONG 1))))
  @end lisp
  "
  (multiple-value-bind (t-tree control) (if (funcall predicate tree)
					    (funcall transformer tree)
					    (values tree #'mapcar))
    (if (and (consp t-tree) control)
	(funcall control #'(lambda (x) (maptree-if predicate transformer x)) t-tree)
	t-tree)))

(defun maptree (keys transformer tree)
  (maptree-if #'(lambda (x) (and (consp x) (member (car x) keys)))
	      transformer tree))

(defun flatten (x)
  "
  Returns a new list by collecting all the symbols found in @arg{x}.

  Example:
  @lisp
  > (flatten '(let ((x 1)) (+ x 2)))
  => (LET X 1 + X 2)
  @end lisp
  "
  (let ((acc nil))
    (maptree-if #'atom #'(lambda (x) (push x acc)) x)
    (reverse acc)))

(declaim (inline slot-values-list))
(defun slot-values (obj slots)
  "
  Returns a list containing slot-values of @arg{obj} corresponding to symbols in the list @arg{slots}.

  Example:
  @lisp
  > (defstruct obj a b)
  => OBJ

  > (let ((thing (make-obj :a 1 :b 2)))
      (slot-values thing '(a b)))
  => (1 2)
  @end lisp
  "
  (mapcar #'(lambda (s) (slot-value obj s)) slots))

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
  (if (listp lst) lst (list lst)))

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

(defun ziptree (&rest args)
  (if (and (some #'atom args) (some #'consp args)) nil
      (if (every #'atom args) args
	  (apply #'mapcar #'ziptree args))))

(declaim (inline zipsym))
(defun zipsym (lst)
  "
  Zips a unique gensym with each element of @arg{lst}.

  Example:
  @lisp
  > (zipsym '(a b c))
  => ((#:G1064 A) (#:G1065 B) (#:G1066 C))
  @end lisp
  "  
  (map 'list #'(lambda (x) (list (gensym) x)) lst))

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
  (reduce #'(lambda (x y)
	      (if (null x)
		  (if (typep (car y) 'symbol) y (car y))
		  (append x (and y (if (typep (car y) 'symbol) `(,y) y)))))
	  lsts :from-end t))

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
  (maptree-if #'(lambda (x) (or (symbolp x) (consp x)))
	      #'(lambda (x) (etypecase x
			      (symbol (if (member x args) x `(quote ,x)))
			      (cons (values `(list ,@x) #'mapcar))))
	      lst))

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
  (if (atom lst) nil
      (cons (length lst) (list-dimensions (car lst)))))

(defun compile-and-eval (source)
  "
  Compiles and evaluates the given @arg{source}.  This should be
  an ANSI compatible way of ensuring method compilation."
  (funcall (compile nil `(lambda () ,source))))

)

