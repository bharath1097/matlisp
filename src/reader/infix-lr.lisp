(in-package #:matlisp-infix)

(defparameter *linfix-reader* (copy-readtable))
(defparameter *blank-characters* '(#\Space #\Tab))

(defparameter *operator-tokens*
  `(("^" ^) (".^" .^)
    ("./" ./) ("/" /)
    ("*" *) (".*" .*) ("@" @)
    (".+" +) ("+" +)
    (".-" -) ("-" -)
    ("(" \() (")" \))
    ("[" \[) ("]" \])
    (":" |:|) ("," \,) (";" \;)
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

(defun token-reader (stream)
  (let* ((stack nil)
     	 (expr nil))
    (labels ((read-stack (&optional (empty? t))
	       (let* ((fstack (reverse (remove-if #'(lambda (x) (member x *blank-characters*)) stack)))
		      (tok (and fstack (read-from-string (coerce fstack 'string)))))
		 (prog1 tok
		   (when empty?
		     (when fstack (push tok expr))
		     (setf stack nil))))))
      (iter (for c next (peek-char nil stream t nil t))
	    (cond
	      ((char=  c #\})
	       (read-stack)
	       (return (reverse expr)))
	      ((when-let (tok (find-if #'(lambda (x) (find-token (first x) stream)) (sort (remove-if-not #'(lambda (x) (char= c (aref (first x) 0))) *operator-tokens*) #'< :key #'(lambda (x) (length (first x))))))
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
;;
(defun list-lexer (list)
  #'(lambda () (if (null list) (values nil nil)
		   (let* ((value (pop list)))
		     (values (cond ((member value *operator-tokens* :key #'second) value)
				   ((or (numberp value) (symbolp value)) 'id)
				   (t (error "Unexpected value ~S" value)))
			     value)))))

(yacc:define-parser *logical-parser*
  (:start-symbol expr)
  (:terminals (^ .^ ./ / * .* @ + + - - |(| |)| [ ] |:| |,| htranspose transpose))
  (:precedence ((:left htranspose transpose)
		(:right ^ .^)
		(:left ./ / * .* @)
		(:left + -)))  
  (expr
   (expr expr trans #'(lambda (a b) (list b a)))
   (expr expr arith expr #'(lambda (a b c) (list b a c)))
   term)

  (expon ^ .^)
  (trans htranspose transpose)
  (arith ./ / * .* @ + -)

  (args
   (id #'list)
   (callable #'list)
   (id |,| args #'(lambda (a b c) (declare (ignore b)) (if (consp c) (list* a c) (list a c)))))

  (callable
   (id |(| args |)| #'(lambda (a b c d) (declare (ignore b d)) (list* a c))))

  (term
   id
   bool
   (not term)
   callable
   (quantifier |(| expression |)| #'(lambda (a b c d) (declare (ignore b d)) (list (first a) (list (second a)) c)))
   (|(| expression |)| #'(lambda (a b c) (declare (ignore a c)) b))))
