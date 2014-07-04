(defpackage "MATLISP-LRI"
  (:use #:cl #:iterate #:yacc #:matlisp-utilities))

(in-package #:matlisp-lri)

(defparameter *linfix-reader* (copy-readtable))
(defparameter *blank-characters* '(#\Space #\Tab #\Newline))

(defparameter *operator-tokens*
  `(("^" ^) ("**" **)
    ("./" ./) ("/" /)
    ("*" *) (".*" .*) ("@" @)
    (".+" +) ("+" +)
    (".-" -) ("-" -)
    ("(" \() (")" \))
    ("[" \[) ("]" \])
    (":" |:|)
    ("=" =) ("==" ==)
    ("," \,)
    ("'" htranspose) (".'" transpose)))

(defun find-token (str stream)
  (let ((stack nil))
    (iter (for r.i in-vector str)
	  (for m.i next (read-char stream t nil t))
	  (push m.i stack)
	  (when (char/= r.i m.i)
	      (map nil #'(lambda (x) (unread-char x stream)) stack)
	      (return nil))
	  (finally (return t)))))

(defun token-reader (stream &optional (enclosing-chars '(#\( . #\))))
  (let* ((stack nil)
     	 (expr nil)
	 (lspe nil))
    (labels ((read-stack (&optional (empty? t))
	       (let* ((fstack (reverse (remove-if #'(lambda (x) (member x *blank-characters*)) stack)))
		      (tok (and fstack (read-from-string (coerce fstack 'string)))))
		 (prog1 tok
		   (when empty?
		     (when fstack (push tok expr))
		     (setf stack nil))))))
      (iter (for c next (peek-char nil stream t nil t))
	    (summing (cond ((char= c (cdr enclosing-chars)) -1) ((char= c (car enclosing-chars)) +1) (t 0)) into count)
	    (cond
	      ((and (char= c (cdr enclosing-chars)) (= count -1))
	       (read-char stream t nil t)
	       (read-stack)
	       (return (values (reverse expr) lspe)))
	      ((member c '(#\# #\\))
	       (when (char= c #\\) (read-char stream t nil t))
	       (let ((word (read stream))
		     (sym (gensym)))
		 (push sym expr)
		 (push (list sym word) lspe)))
	      ((when-let (tok (find-if #'(lambda (x) (find-token (first x) stream)) (sort (remove-if-not #'(lambda (x) (char= c (aref (first x) 0))) *operator-tokens*) #'> :key #'(lambda (x) (length (first x))))))
		 (read-stack)
		 (push (second tok) expr)))
	      ((and (char= c #\i) (numberp (read-stack nil)))
	       (read-char stream t nil t)
	       (push (complex 0 (read-stack nil)) expr)
	       (setf stack nil))
	      ((member c *blank-characters*)
	       (read-char stream t nil t)
	       (read-stack))
	      ((char= c #\\)
	       (read-char stream t nil t)
	       (read-stack)
	       (push (read stream t nil nil) expr))
	      (t
	       (push (read-char stream t nil t) stack)))))))

(defun list-lexer (list)
  #'(lambda () (if (null list) (values nil nil)
		   (let* ((value (pop list)))
		     (values (cond ((member value *operator-tokens* :key #'second) value)
				   ((numberp value) 'number)
				   ((symbolp value) 'id)
				   (t (error "Unexpected value ~S" value)))
			     value)))))
;;
(yacc:define-parser *linfix-parser*
  (:start-symbol expr)
  (:terminals (** ./ / * .* @ ^ + - = == |(| |)| [ ] |:| |,| htranspose transpose id number))
  (:precedence ((:left htranspose transpose)
		(:right **)
		(:left ./ / * .* @ ^)
		(:left + -)
		(:left = ==)))
  (expr
   (expr htranspose #'(lambda (a b) (list b a)))
   (expr transpose #'(lambda (a b) (list b a)))
   (expr + expr #'(lambda (a b c) (list b a c)))
   (expr - expr #'(lambda (a b c) (list b a c)))
   (expr / expr #'(lambda (a b c) (list b a c)))
   (expr ./ expr #'(lambda (a b c) (list b a c)))
   (expr * expr #'(lambda (a b c) (list b a c)))
   (expr .* expr #'(lambda (a b c) (list b a c)))
   (expr @ expr #'(lambda (a b c) (list b a c)))
   (expr ^ expr #'(lambda (a b c) (list b a c)))
   (expr ** expr #'(lambda (a b c) (list b a c)))
   (expr = expr #'(lambda (a b c) (declare (ignore b)) (list 'setf a c)))
   (expr == expr #'(lambda (a b c) (list b a c)))
   callable slice
   term)
  ;;
  (args
   (expr #'list)
   (expr |,| args #'(lambda (a b c) (declare (ignore b)) (if (consp c) (list* a c) (list a c)))))
  (callable
   (term |(| |)| #'(lambda (a b c) (declare (ignore b c)) (list a)))
   (term |(| args |)| #'(lambda (a b c d) (declare (ignore b d)) (list* a c))))
  ;;
  (idxs
   expr
   (expr |:| expr #'(lambda (a b c) (declare (ignore b)) (list :slice a c)))
   (expr |:| expr |:| expr #'(lambda (a b c d e) (declare (ignore b d)) (list :slice a c e))))
  (sargs
   (idxs #'list)
   (idxs |,| sargs #'(lambda (a b c) (declare (ignore b)) (if (consp c) (list* a c) (list a c)))))
  (slice
   (term [ ] #'(lambda (a b c) (declare (ignore b c)) (list 'matlisp-infix::generic-ref a)))
   (term [ sargs ] #'(lambda (a b c d) (declare (ignore b d)) (list* 'matlisp-infix::generic-ref a c))))
  ;;
  (term
   number
   id
   (- term)
   (/ term)
   (./ term)
   (|(| expr |)| #'(lambda (a b c) (declare (ignore a c)) b))))
;;
(defparameter *ref-list* '((cons elt) (array aref) (matlisp::base-tensor matlisp:ref)))

(defun process-slice (args)
  (mapcar #'(lambda (x) (if (and (consp x) (eql (car x) :slice)) `(list* ,@(cdr x)) x)) args))

(defmacro generic-ref (x &rest args)
  (cond
    ((null args) x)
    ((find-if #'(lambda (sarg) (and (consp sarg) (eql (car sarg) ':slice))) args)
     `(matlisp::subtensor~ ,x (list ,@(process-slice args))))
    (t
     `(etypecase ,x
	,@(mapcar #'(lambda (l) `(,(car l) (,(cadr l) ,x ,@args))) (if (> (length args) 1) (cdr *ref-list*) *ref-list*))))))

(define-setf-expander generic-ref (x &rest args &environment env)
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion x env)
    (with-gensyms (store)
      (values (append dummies newval)
	      (append vals (list getter))
	      `(,store)
	      (let ((arr (car newval)))
		`(prog1 ,(cond
			  ((null args)
			   `(matlisp::copy! ,store ,arr))
			  ((find-if #'(lambda (sarg) (and (consp sarg) (eql (car sarg) ':slice))) args)
			   `(setf (matlisp::subtensor~ ,arr (list ,@(process-slice args))) ,store))
			  (t`(etypecase ,arr
			       ,@(mapcar #'(lambda (l) `(,(car l) (setf (,(cadr l) ,arr ,@args) ,store))) (if (> (length args) 1) (cdr *ref-list*) *ref-list*)))))
		   ,setter))
	      `(generic-ref ,getter ,@args)))))

(defmacro generic-incf (x expr &optional (alpha 1) &environment env)
  (multiple-value-bind (dummies vals new setter getter) (get-setf-expansion x env)
    (when (cdr new)
      (error "Can't expand this."))
    (with-gensyms (val)
      (let ((new (car new)))
	`(let* (,@(zip dummies vals)
		(,new ,getter)
		(,val ,expr))
	   (etypecase ,new
	     (matlisp::base-tensor (matlisp::axpy! ,alpha ,val ,new))
	     (t (setq ,new (+ ,new ,val))))
	   ,setter)))))
;;
(defparameter *operator-assoc-table* '((* matlisp::tb*-opt)
				       (.* matlisp::tb.*)
				       (@ matlisp::tb@)
				       (^ matlisp::tb^)
				       (+ matlisp::tb+)
				       (- matlisp::tb-)
				       (\\ matlisp::tb\\)
				       (/ matlisp::tb/)
				       (./ matlisp::tb./)
				       (== matlisp::tb==)
				       (transpose matlisp::transpose)
				       (htranspose matlisp::htranspose)))

(defun op-overload (expr)
  (labels ((walker (expr)
	     (dwalker
	      (cond
		((atom expr) expr)
		((and (member (car expr) '(+ * progn)) (not (cddr expr))) (walker (second expr)))
		((eq (car expr) '*)
		 (if (and (consp (second expr)) (eq (car (second expr)) '/) (not (cddr (second expr)))) ;;ldiv
		     `(\\ (* ,@(cddr expr)) ,(cadr (second expr)))
		     (iter (for op in (cdr expr))
			   (for lst on (cdr expr))
			   (if (and (consp op) (eq (car op) '/) (not (cddr op)))
			       (return
				 (walker
				  (let ((left `(/ (* ,@oplist) ,(second op)) ))
				    (if (cdr lst)
					`(* ,left ,@(cdr lst))
					left))))
			       (collect op into oplist))
			   (finally (return expr)))))
		(t expr))))
	   (dwalker (expr)
	     (if (atom expr) expr
		 (cond
		   ((and (eq (car expr) '/) (not (cddr expr)))
		    `(,(or (second (assoc (car expr) *operator-assoc-table*)) (car expr)) ,(walker (second expr)) nil))
		   (t
		    `(,(or (second (assoc (car expr) *operator-assoc-table*)) (car expr))
		       ,@(mapcar #'walker (cdr expr))))))))
    (walker expr)))
;;
(defun infix-reader (stream subchar arg)
  ;; Read either #I(...) or #I"..."
  (declare (ignore subchar))
  (assert (null arg) nil "given arg where none was required.")
  ;;(ignore-characters +blank-characters+ stream)
  (multiple-value-bind (iexpr bind) (token-reader stream (ecase (read-char stream t nil t) (#\( (cons #\( #\))) (#\[ (cons #\[ #\]))))
    (setf iexpr (nconc (list 'progn '\() iexpr (list '\))))
    (let ((lexpr (op-overload (yacc:parse-with-lexer (list-lexer iexpr) *linfix-parser*))))
      (map nil #'(lambda (x) (setf lexpr (subst (second x) (first x) lexpr))) bind)
      lexpr)))
;;
(defparameter *tensor-symbol*
  '((#\D matlisp::real-tensor)
    (#\Z matlisp::complex-tensor)
    (#\N matlisp::integer-tensor)
    (#\B matlisp::boolean-tensor)))

(defun tensor-reader (stream subchar arg)
  (assert (null arg) nil "given arg where none was required.")
  (labels ((ignore-characters (stream ignore)
	     (iter (for c next (peek-char nil stream t nil t))
		   (if (member c ignore :test #'char=) (read-char stream t nil t) (terminate))))
	   (list-reader (stream &optional (enclosing-chars (list #\[ #\])))
	     (iter (for c next (peek-char nil stream t nil t))
		   (with first-p = t)
		   (cond
		     ((char= c (second enclosing-chars))
		      (read-char stream t nil t)
		      (return (cons 'list ret)))
		     ((member c *blank-characters*)
		      (read-char stream t nil t))
		     ((char= c #\[)
		      (if first-p (setq first-p nil) (error "Missing delimiter ~a" #\,))
		      (read-char stream t nil t)
		      (collect (list-reader stream enclosing-chars) into ret))
		     ((char= c #\,)
		      (ignore-characters stream (cons #\, *blank-characters*))
		      (unless (char= (read-char stream t nil t) #\[)
			(error "Missing bracket"))
		      (collect (list-reader stream enclosing-chars) into ret))
		     (t (multiple-value-bind (iexpr bind) (token-reader stream (cons #\[ #\]))
			  (setf iexpr (nconc (list 'list '\() iexpr (list '\))))
			  (let ((lexpr (op-overload (yacc:parse-with-lexer (list-lexer iexpr) *linfix-parser*))))
			    (map nil #'(lambda (x) (setf lexpr (subst (second x) (first x) lexpr))) bind)
			    (return lexpr))))))))
    (let ((cl (second (find subchar *tensor-symbol* :key #'car))))
      (let ((c (read-char stream t nil t)))
	(assert (char= c #\[) nil "given unknown token ~a" c))
      `(matlisp::copy ,(list-reader stream) ',cl))))

;;Define a readtable with dispatch characters
(macrolet ((tensor-symbol-enumerate ()
	     `(named-readtables:defreadtable :lri-table
		(:merge :standard)
		(:dispatch-macro-char #\# #\I #'infix-reader)
		,@(mapcar #'(lambda (x) `(:dispatch-macro-char #\# ,(car x) #'tensor-reader)) *tensor-symbol*))))
  (tensor-symbol-enumerate))
