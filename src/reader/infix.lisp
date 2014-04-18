(in-package #:matlisp-infix)
(pushnew :matlisp-infix *features*)

;;Precedence
(defparameter *operator-ordering* 
  '((\( \! \[ \.)                       ; \[ is array reference
    (quote transpose ** )      	        ; exponentiation
    ( ~ )				; lognot 
    (@ .* ./ * / \\ % )			; % is mod
    ( + - )
    ( << >> )
    ( < == > <= != >= )
    ( & )				; logand
    ( ^ )				; logxor
    ( \| )				; logior
    ( not )
    ( and )
    ( or )
    ;; Where should setf and friends go in the precedence?
    ( = += -= *= /=)
    (|:|) ;;slicing
    ( \, newline )			; progn (statement delimiter)
    ( \] \) )
    ( %infix-end-token% ))		; end of infix expression
  "Ordered list of operators of equal precedence.")
(defparameter *right-associative-operators* '(** =))

(defun same-token-p (x y)
  (and (symbolp x) (symbolp y) (string-equal (symbol-name x) (symbol-name y))))

(defun same-operator-p (x y)
  (same-token-p x y))

(defun operator-lessp (op1 op2)
  (dolist (ops *operator-ordering* nil)
    (cond ((find op1 ops :test #'same-token-p) (return nil))
	  ((find op2 ops :test #'same-token-p) (return t)))))

(defun operator-right-associative-p (operator)
  (find operator *right-associative-operators*))

;; Matlisp helpers
(defparameter *ref-list* '((cons elt) (array aref) (matlisp::base-tensor matlisp:ref) ))

(defun process-slice (args)
  (mapcar #'(lambda (x)
	      (cond
		((consp x)
		 (if (eql (car x) ':slice)
		     `(list* ,@(cdr x))
		     (with-gensyms (idx)
		       `(let ((,idx ,x)) (declare (type matlisp::index-type ,idx)) (list ,idx (unless (= ,idx -1) (1+ ,idx)))))))
		((or (numberp x) (symbolp x)) `(list ,x (1+ ,x)))
		(t (error 'parser-error :arguments x :message "unknown argument type"))))
	  args))

(defmacro generic-ref (x &rest args)
  (if (find-if #'(lambda (sarg) (and (consp sarg) (eql (car sarg) ':slice))) args)
      `(matlisp::subtensor~ ,x (list ,@(process-slice args)) nil nil)
      `(etypecase ,x
	 ,@(mapcar #'(lambda (l) `(,(car l) (,(cadr l) ,x ,@args))) (if (> (length args) 1) (cdr *ref-list*) *ref-list*)))))

(define-setf-expander generic-ref (x &rest args &environment env)
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion x env)
    (with-gensyms (store)
      (values (append dummies newval)
	      (append vals (list getter))
	      `(,store)
	      (let ((arr (car newval)))
		`(prog1 ,(if (find-if #'(lambda (sarg) (and (consp sarg) (eql (car sarg) ':slice))) args)
			     `(setf (matlisp::subtensor~ ,arr (list ,@(process-slice args)) nil t) ,store)
			     `(etypecase ,arr
				,@(mapcar #'(lambda (l) `(,(car l) (setf (,(cadr l) ,arr ,@args) ,store))) (if (> (length args) 1) (cdr *ref-list*) *ref-list*))))
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
;; Peeking Token Reade
(defvar *peeked-token* nil)
(defun read-token (stream)
  (if *peeked-token*
      (pop *peeked-token*)
      (read stream t nil t)))
(defun peek-token (stream)
  (unless *peeked-token*
    (push (read stream t nil t) *peeked-token*))
  (car *peeked-token*))

;;
(define-constant +blank-characters+ '(#\^m #\space #\tab #\return #\newline))
(define-constant +newline-characters+ '(#\newline #\^m #\linefeed #\return))

(defmacro infix-error (format-string &rest args)
  `(progn
     (setf *peeked-token* nil)
     (error 'parser-error :message (format nil ,format-string ,@args))))

(defun ignore-characters (ignore stream)
  (iter (for char next (peek-char nil stream t nil t))
  	(if (not (member char ignore :test #'char=)) (terminate)
	    (collect (read-char stream t nil t) at beginning))))

(defun unread-characters (chars stream)
  (mapcar #'(lambda (x) (unread-char x stream)) chars))

;;; Readtable
(defparameter *infix-readtable* (copy-readtable nil))

(defun infix-reader (stream subchar arg)
  ;; Read either #I(...) or #I"..."
  (declare (ignore arg subchar))
  ;;(ignore-characters +blank-characters+ stream)
  (ecase (peek-char nil stream t nil t)
    (#\( (read-char stream)
	 (let ((*readtable* *infix-readtable*))
	   (op-overload (read-infix stream))))
    (#\[ (iter (for c next (read-char stream t nil t))
	       (collect c into clist)
	       (when (char= c #\])
		 (return (string->prefix (coerce clist 'string))))))))

(defparameter *operator-assoc-table* '((* matlisp::t*)
				       (.* matlisp::t.*)
				       (@ matlisp::t@)
				       (+ matlisp::t+)
				       (- matlisp::t-)
				       (\\ matlisp::t\\)
				       (/ matlisp::t/)
				       (./ matlisp::t./)))
(defun op-overload (expr)
  (labels ((walker (expr)
	     (dwalker
	      (cond
		((atom expr) expr)
		((and (member (car expr) '(+ * progn)) (not (cddr expr))) (second expr))
		((eq (car expr) '*)
		 (if (and (consp (second expr)) (eq (car (second expr)) '/) (not (cddr (second expr)))) ;;ldiv
		     `(\\ ,(cadr (second expr)) (* ,@(cddr expr)))
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

;; (set-dispatch-macro-character #\# #\I #'infix-reader *readtable*)
(defreadtable :infix-dispatch-table
  (:merge :standard)
  (:dispatch-macro-char #\# #\I #'infix-reader))

(defmacro with-readtable ((name) &rest body)
  `(let ((*readtable* (named-readtables:ensure-readtable ',name)))
     ,@body))

;;
(defun string->prefix (string)
  "Convert a string to a prefix s-expression using the infix reader.
  If the argument is not a string, just return it as is."
  (if (stringp string)
      (with-input-from-string (stream (concatenate 'string "#I(" string ")"))
	(with-readtable (:infix-dispatch-table)
	  (read stream)))
      string))

(defun read-infix (stream)
  (let* ((result (gather-superiors '\) stream)) ; %infix-end-token%
	 (next-token (read-token stream)))
    (unless (same-token-p next-token '\)) ; %infix-end-token%
      (infix-error "Infix expression ends with ~A." next-token))
    result))

(defun read-regular (stream)
  (with-readtable (:infix-dispatch-table)
    (read stream t nil t)))

;;; Hack to work around + and - being terminating macro characters,
;;; so 1e-3 doesn't normally work correctly.
(defun fancy-number-format-p (left operator stream)
  (when (symbolp left)
    (let* ((name (symbol-name left))
	   (length (length name)))
      (when (valid-numberp (subseq name 0 (1- length)))
	;; Exponent, Single, Double, Float, or Long, Imaginary
	(cond
	  ((member (aref name (1- length)) '(#\I #\J))
	   (with-readtable (:common-lisp)
	     (complex 0 (read-from-string (subseq name 0 (1- length))))))
	  ((and (find operator '(+ -) :test #'same-operator-p)
		(member (aref name (1- length)) '(#\E #\S #\D #\F #\L)))
	   (read-token stream)
	   (let* ((right (peek-token stream))
		  (rname (and (symbolp right) (not (token-operator-p right)) (symbol-name right))))
	     (cond ((integerp right) ;; it is one of the fancy numbers, so return it
		    (read-token stream)
		    (with-readtable (:common-lisp)
		      (read-from-string (format nil "~A~A~A" left operator right))))
		   ((and rname (> (length rname) 1) (every #'(lambda (x) (find x "0123456789" :test #'char=)) (subseq rname 0 (1- (length rname)))) (member (aref rname (1- (length rname))) '(#\I #\J)))
		    (read-token stream)
		    (complex 0 (with-readtable (:common-lisp)
				 (read-from-string (format nil "~A~A~A" left operator (subseq rname 0 (1- (length rname))))))))
		   (t ;; it isn't one of the fancy numbers, so unread the token
		    (push operator *peeked-token*)
		    nil)))))))));; and return nil

(defun valid-numberp (string)
  (when (stringp string)
    (with-readtable (:common-lisp)
      (realp (read-from-string string nil nil)))))
;; (let ((saw-dot nil))
;;   (when (> (length string) 0)
;;     (dolist (char (coerce string 'list) t)
;; 	(cond ((char= char #\.)
;; 	       (if saw-dot
;; 		   (return nil)
;; 		   (setq saw-dot t)))
;; 	      ((not (find char "01234567890" :test #'char=))
;; 	       (return nil)))))))

;;; Gobbles an expression from the stream.
(defun gather-superiors (previous-operator stream)
  "Gathers an expression whose operators all exceed the precedence of
   the operator to the left."
  (let ((left (get-first-token stream)))
    (loop
       (setq left (post-process-expression left))
       (let* ((peeked-token (peek-token stream))
	      (fancyp (fancy-number-format-p left peeked-token stream)))
	 (when fancyp
	   (setq left fancyp
		 peeked-token (peek-token stream)))
	 (when (eql peeked-token 'blank)
	   (setq peeked-token (progn (read-token stream) (peek-token stream))))
	 (unless (or (operator-lessp previous-operator peeked-token)
		     (and (same-operator-p peeked-token previous-operator)
			  (operator-right-associative-p previous-operator)))
	   ;; The loop should continue when the peeked operator is
	   ;; either superior in precedence to the previous operator,
	   ;; or the same operator and right-associative.
	   (return left)))
       (setq left (get-next-token stream left)))))

(defun get-first-token (stream)
  (let ((token (iter (for tok next (read-token stream))
		     (unless (eql tok 'blank) (return tok)))))
    (if (token-operator-p token)
	;; It's an operator in a prefix context.
	(apply-token-prefix-operator token stream)
	;; It's a regular token
	token)))

(defun apply-token-prefix-operator (token stream)
  (let ((operator (get-token-prefix-operator token)))
    (if operator
	(funcall operator stream)
	(infix-error "\"~A\" is not a prefix operator" token))))

(defun get-next-token (stream left)
  (let ((token (read-token stream)))
    (apply-token-infix-operator token left stream)))

(defun apply-token-infix-operator (token left stream)
  (let ((operator (get-token-infix-operator token)))
    (if operator
	(funcall operator stream left)
	(infix-error "\"~A\" is not an infix operator" token))))

;;; Fix to read-delimited-list so that it works with tokens, not
;;; characters.
(defun infix-read-delimited-list (end-token delimiter-token stream)
    (iter (for next-token next (peek-token stream))
	  (counting t into count)	
	  (if (same-token-p next-token end-token) (progn (read-token stream)
							 (terminate))
	      (progn (when (and (> count 1) (not (same-token-p (read-token stream) delimiter-token)))
		       (infix-error "Missing delimiter: ~A" delimiter-token))
		     (collect (gather-superiors delimiter-token stream))))))

(defun read-slice (separator-token end-token delimiter-token stream)
  (let ((stop-tokens (list separator-token end-token)))
    (iter (for next-token next (peek-token stream))
	  (counting t into count)
	  (if (member next-token stop-tokens :test #'same-token-p) (terminate)
	      (progn (when (and (> count 1) (not (same-token-p (prog1 (read-token stream) (setq next-token (peek-token stream))) delimiter-token)))
		       (infix-error "Missing delimiter: ~A" delimiter-token))
		     (collect (if (or (member next-token stop-tokens :test #'same-token-p) (eq next-token delimiter-token)) nil
				  (gather-superiors delimiter-token stream))))))))
;;; Syntactic Modifications
;;; Post processes the expression to remove some unsightliness caused
;;; by the way infix processes the input. Note that it is also required
;;; for correctness in the a<b<=c case.

;; a / b => a b^{-1}
;; a \ b => a^{-1} b
(defun post-process-expression (expression)
  (or (cond
	((and (consp expression)
	      (eq (first expression) 'progn)
	      (= (length expression) 2))
	 (second expression))
	((and (consp expression)
	      (= (length expression) 3))
	 (destructuring-bind (operator left right) expression
	   (cond ((and (consp left)
		       (same-operator-p (first left) operator)
		       (find operator '(+ @ .* * ./ / - and or < > <= >= progn)
			     :test #'same-operator-p))
		  ;; Flatten the expression if possible
		  (cond ((and (eq operator '-)
			      (= (length left) 2))
			 ;; -a-b --> (+ (- a) (- b)). 
			 `(+ ,left (- ,right)))
			((and (eq operator './)
			      (= (length left) 2))
			 ;; prefer multiplications over inversions.
			 ;; works only with commutative rings!
			 `(./ ,(post-process-expression `(.* ,(second left) ,right))))
			(t
			 ;; merges a+b+c as (+ a b c).
			 (append left (list right)))))
		 ((and (eq operator '*)
		       (consp right) (eq (car right) '/)
		       (consp left) (eq (car left) '/))
		  `(* ,@(if (cddr left)
			    `(,(cadr left) ,@(mapcar #'(lambda (x) `(/ ,x)) (cddr left)))
			    `((/ ,(cadr left))))
		      ,@(if (cddr right)
			    `(,(cadr right) ,@(mapcar #'(lambda (x) `(/ ,x)) (cddr right)))
			    `((/ ,(cadr right))))))
		 ((and (eq operator '/))
		  (post-process-expression `(* ,left (/ ,right))))
		 ;; ((and (eq operator '*)
		 ;;       (consp right)
		 ;;       (= (length right) 2)
		 ;;       (eq (car right) '/))
		 ;;  `(/ ,left ,(second right)))
		 ;; ((and (eq operator '*)
		 ;;       (consp left)
		 ;;       (
		 ((and (consp left)
		       (eq operator '-)
		       (eq (first left) '+))
		  ;; merges a+b-c as (+ a b (- c)).
		  (append left (list `(- ,right))))
		 ((and (consp left)
		       (find operator '(< > <= >=))
		       (find (first left) '(< > <= >=)))
		  ;; a<b<c --> a<b and b<c
		  `(and ,left
			(,operator ,(first (last left))
				   ,right)))))))
      expression))
;;; ********************************
;;; Define Operators ***************
;;; ********************************

(defparameter *token-operators* nil)
(defparameter *token-prefix-operator-table* (make-hash-table))
(defparameter *token-infix-operator-table* (make-hash-table))
(defun token-operator-p (token)
  (find token *token-operators*))
(defun get-token-prefix-operator (token)
  (gethash token *token-prefix-operator-table*))
(defun get-token-infix-operator (token)
  (gethash token *token-infix-operator-table*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-token-operator (operator-name &key
						   (prefix nil prefix-p)
						   (infix nil infix-p))
    `(progn
       (pushnew ',operator-name *token-operators*)
       ,(when prefix-p
	      `(setf (gethash ',operator-name *token-prefix-operator-table*)
		     #'(lambda (stream)
			 ,@(cond ((and (consp prefix)
				       (eq (car prefix) 'infix-error))
				  ;; To avoid ugly compiler warnings.
				  `((declare (ignore stream))
				    ,prefix))
				 (t
				  (list prefix))))))
       ,(when infix-p
	      `(setf (gethash ',operator-name *token-infix-operator-table*)
		     #'(lambda (stream left)
			 ,@(cond ((and (consp infix)
				       (eq (car infix) 'infix-error))
				  ;; To avoid ugly compiler warnings.
				  `((declare (ignore stream left))
				    ,infix))
				 (t
				  (list infix)))))))))

;;; Readtable definitions for characters, so that the right token is returned.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-character-tokenization (char function)
    `(set-macro-character ,char ,function nil *infix-readtable*)))


;;; ********************************
;;; Operator Definitions ***********
;;; ********************************

(define-token-operator and
    :infix  `(and ,left ,(gather-superiors 'and stream)))
(define-token-operator or
    :infix  `(or ,left ,(gather-superiors 'or stream)))
(define-token-operator not
    :prefix `(not ,(gather-superiors 'not stream)))

;;---------------------------------------------------------------;;
(define-character-tokenization #\+
    #'(lambda (stream char)
	(declare (ignore char))
	(cond ((char= (peek-char nil stream t nil t) #\=)
	       (read-char stream t nil t)
	       '+=)
	      (t
	       '+))))

(define-token-operator +
    :infix `(+ ,left ,(gather-superiors '+ stream))
    :prefix (gather-superiors '+ stream))
(define-token-operator +=
    :infix `(generic-incf ,left ,(gather-superiors '+= stream)))

;;---------------------------------------------------------------;;
(define-character-tokenization #\-
    #'(lambda (stream char)
	(declare (ignore char))
	(cond ((char= (peek-char nil stream t nil t) #\=)
	       (read-char stream t nil t)
	       '-=)
	      (t
	       '-))))
(define-token-operator -
    :infix `(- ,left ,(gather-superiors '- stream))
    :prefix `(- ,(gather-superiors '- stream)))
(define-token-operator -=
    :infix `(generic-incf ,left ,(gather-superiors '-= stream) -1))

;;*--------------------------------------------------------------;;

;; (defun read-num (stream char)
;;   (unread-char char stream)
;;   (with-readtable (:common-lisp)
;;     (read stream)))

;; (macrolet ((num-toks ()
;; 	     `(eval-every
;; 		,@(loop :for c :from 0 :below 10
;; 		     :collect `(define-character-tokenization ,(aref (princ-to-string c) 0)
;; 			      #'read-num))))
;; 	   (unum-toks ()
;; 	     `(eval-every
;; 		,@(loop :for c :from 0 :below 10
;; 		     :collect `(set-macro-character ,(aref (princ-to-string c) 0) nil)))))
;;   (unum-toks))

;;
(define-character-tokenization #\.
    #'(lambda (stream char)
	(declare (ignore char))
	(case (peek-char nil stream t nil t)
	  (#\* (read-char stream t nil t) '.*)
	  (#\/ (read-char stream t nil t) './)
	  ((#\') (read-char stream t nil t) 'transpose)
	  ((#\^m #\space #\tab #\return #\newline) 'blank)
	  (t '\.))))

(define-token-operator \.
    :infix (let ((right (iter (for char next (peek-char nil stream t nil t))
			      (if (or (member char +blank-characters+)
				      (get-macro-character char *infix-readtable*))
				  (return clist)
				  (collect (read-char stream t nil t) into clist)))))
	     (with-readtable (:common-lisp)
	       (read-from-string (format nil "~a.~a" left (coerce right 'string))))))

(define-character-tokenization #\Space
    #'(lambda (stream char) (iter (for c next (peek-char nil stream t nil t))
				  (if (member c +blank-characters+)
				      (read-char stream t nil t)
				      (return 'blank)))))

(define-token-operator transpose
    :infix `(matlisp::transpose ,left))

(define-character-tokenization #\'
    #'(lambda (stream char) (declare (ignore stream char)) 'quote))

(define-token-operator quote
    :infix `(matlisp::htranspose ,left)
    :prefix `(cl:quote  ,(read-regular stream)))
;;
(define-character-tokenization #\*
    #'(lambda (stream char)
	(declare (ignore char))
	(let ((pchar (peek-char nil stream t nil t)))
	  (case pchar
	    (#\=
	     (read-char stream t nil t)
	     '*=)
	    (#\*
	     (read-char stream t nil t)
	     '**)
	    (t
	     '*)))))

(define-character-tokenization #\@
    #'(lambda (stream char) (declare (ignore stream char)) '@))

(define-token-operator @
    :infix `(@ ,left ,(gather-superiors '@  stream)))

(define-token-operator .*
    :infix `(.* ,left ,(gather-superiors '.* stream)))

(define-token-operator *
    :infix `(* ,left ,(gather-superiors '* stream)))

(define-token-operator *=
    :infix `(,(if (symbolp left)
		  'setq
		  'setf)
	      ,left 
	      (* ,left ,(gather-superiors '*= stream))))

(define-token-operator **
    :infix `(expt ,left ,(gather-superiors '** stream)))

;;---------------------------------------------------------------;;
(define-character-tokenization #\/
    #'(lambda (stream char)
	(declare (ignore char))
	(cond ((char= (peek-char nil stream t nil t) #\=)
	       (read-char stream t nil t)
	       '/=)
	      (t
	       '/))))

;; Sure we can do what MATLAB does, but this means one can't use *'s in symbol names!
;; (define-character-tokenization #\\
;;     #'(lambda (stream char)
;; 	(declare (ignore char stream))
;; 	'\\))

;; (define-token-operator \\
;;     :infix `(\\ ,left ,(gather-superiors '\\ stream)))

(define-token-operator ./
    :infix `(./ ,left ,(gather-superiors './ stream))
    :prefix `(./ ,(gather-superiors './ stream)))

(define-token-operator /
    :infix `(/ ,left ,(gather-superiors '/ stream))
    :prefix `(/ ,(gather-superiors '/ stream)))

(define-token-operator /=
    :infix `(,(if (symbolp left)
		  'setq
		  'setf)
	      ,left
	      (/ ,left ,(gather-superiors '/= stream))))

;;---------------------------------------------------------------;;
(define-character-tokenization #\^
    #'(lambda (stream char)
	(declare (ignore stream char))
	'^))

(define-token-operator ^
    :infix `(logxor ,left ,(gather-superiors '^ stream)))

;;---------------------------------------------------------------;;
(define-character-tokenization #\|
    #'(lambda (stream char)
	(declare (ignore char))
	(cond ((char= (peek-char nil stream t nil t) #\|)
	       (read-char stream t nil t)
	       'or)
	      (t
	       '\|))))
(define-token-operator \|
    :infix `(logior ,left ,(gather-superiors '\| stream)))

;;---------------------------------------------------------------;;
(define-character-tokenization #\&
    #'(lambda (stream char)
	(declare (ignore char))
	(cond ((char= (peek-char nil stream t nil t) #\&)
	       (read-char stream t nil t)
	       'and)
	      (t
	       '\&))))
(define-token-operator \&
    :infix `(logand ,left ,(gather-superiors '\& stream)))

;;---------------------------------------------------------------;;
(define-character-tokenization #\%
    #'(lambda (stream char)
	(declare (ignore stream char))
	'\%))

(define-token-operator \%
    :infix `(mod ,left ,(gather-superiors '\% stream)))

;;---------------------------------------------------------------;;
(define-character-tokenization #\~
    #'(lambda (stream char)
	(declare (ignore stream char))
	'\~))

(define-token-operator \~
    :prefix `(lognot ,(gather-superiors '\~ stream)))

;;---------------------------------------------------------------;;
(define-character-tokenization #\,
    #'(lambda (stream char)
	(declare (ignore char stream))
	'\,))

;;Get rid of this
(define-token-operator \,
    :infix `(progn ,left ,(gather-superiors '\, stream)))

(define-character-tokenization #\Newline
    #'(lambda (stream char)
	(declare (ignore char stream))
	'newline))

(define-token-operator newline
    :infix (progn
	     (ignore-characters +blank-characters+ stream)
	     (case (peek-char nil stream t nil t)
	       (#\)
		left)
	       (t
		`(progn ,left ,(gather-superiors 'newline stream))))))
;;---------------------------------------------------------------;;

(define-character-tokenization #\=
    #'(lambda (stream char)
	(declare (ignore char))
	(cond ((char= (peek-char nil stream t nil t) #\=)
	       (read-char stream t nil t)
	       '==)
	      (t
	       '=))))
(define-token-operator ==
    :infix `(= ,left ,(gather-superiors '== stream)))
(define-token-operator =
    :infix `(,(if (symbolp left)
		  'setq
		  'setf)
	      ,(if (and (consp left) (eql (car left) 'progn))
		   `(values ,@(cdr left))
		   left)
	      ,(gather-superiors '= stream)))

(define-character-tokenization #\:	
    #'(lambda (stream char)
        (declare (ignore char))
        (cond ((char= (peek-char nil stream t nil t) #\=)
               (read-char stream t nil t)
               '|:=|)
              (t
               '|:|))))

(define-token-operator |:|
    :infix (destructuring-bind (inc &optional (end nil endp)) (or (read-slice '\, '\] '|:| stream) (list nil nil))
	     (unless endp (rotatef inc end))
	     `(:slice ,left ,end ,inc))
    :prefix (destructuring-bind (inc &optional (end nil endp)) (or (read-slice '\, '\] '|:| stream) (list nil nil))
	      (unless endp (rotatef inc end))
	      `(:slice nil ,end ,inc)))

(define-token-operator |:=|
    :infix `(,(if (symbolp left)
                  'setq
                  'setf)
              ,left
              ,(gather-superiors '|:=| stream)))

(define-character-tokenization #\<
    #'(lambda (stream char)
	(declare (ignore char))
	(cond ((char= (peek-char nil stream t nil t) #\=)
	       (read-char stream t nil t)
	       '<=)
	      ((char= (peek-char nil stream t nil t) #\<)
	       (read-char stream t nil t)
	       '<<)
	      (t
	       '<))))

(define-token-operator <
    :infix `(< ,left ,(gather-superiors '< stream)))

(define-token-operator <=
    :infix `(<= ,left ,(gather-superiors '<= stream)))

(define-token-operator <<
    :infix `(ash ,left ,(gather-superiors '<< stream)))

(define-character-tokenization #\>
    #'(lambda (stream char)
	(declare (ignore char))
	(cond ((char= (peek-char nil stream t nil t) #\=)
	       (read-char stream t nil t)
	       '>=)
	      ((char= (peek-char nil stream t nil t) #\>)
	       (read-char stream t nil t)
	       '>>)
	      (t
	       '>))))

(define-token-operator >
    :infix `(> ,left ,(gather-superiors '> stream)))

(define-token-operator >=
    :infix `(>= ,left ,(gather-superiors '>= stream)))

(define-token-operator >>
    :infix `(ash ,left (- ,(gather-superiors '>> stream))))

(define-character-tokenization #\!
    #'(lambda (stream char)
	(declare (ignore char))
	(cond ((char= (peek-char nil stream t nil t) #\=)
	       (read-char stream t nil t)
	       '!=)
	      (t
	       '!))))

(define-token-operator !=
    :infix `(not (= ,left ,(gather-superiors '!= stream))))

(define-token-operator !
    :prefix (read-regular stream))

(define-character-tokenization #\[
    #'(lambda (stream char)
	(declare (ignore stream char))
	'\[))

(define-token-operator \[
    :infix (let ((indices (infix-read-delimited-list '\] '\, stream)))
	     (if (null indices)
		 (infix-error "No indices found in array reference.")
		 `(generic-ref ,left ,@indices)))
    :prefix (let ((ele (infix-read-delimited-list '\] '\, stream)))
	      (if (find-if #'(lambda (sarg) (and (consp sarg) (eql (car sarg) ':slice))) ele)
		  `(list ,@(process-slice ele))
		  `(list ,@ele))))

(define-character-tokenization #\(
    #'(lambda (stream char)
	(declare (ignore stream char))
	'\())

(define-token-operator \(
    :infix `(,left ,@(infix-read-delimited-list '\) '\, stream))
    :prefix (let ((list (infix-read-delimited-list '\) '\, stream)))
	      `(progn ,@list)))

(define-character-tokenization #\]
    #'(lambda (stream char)
	(declare (ignore stream char))
	'\]))

(define-token-operator \]
    :infix (infix-error "Extra close bracket \"]\" in infix expression"))

(define-character-tokenization #\)
    #'(lambda (stream char)
	(declare (ignore stream char))
	'\)))

(define-token-operator \)
    :infix (infix-error "Extra close paren \")\" in infix expression"))
