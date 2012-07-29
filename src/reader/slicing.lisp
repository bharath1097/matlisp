(in-package #:matlisp)
;;TODO move things from old/reader.lisp; must adapt things to reading tensors.

(define-constant +parser-ignored-characters+ '(#\^m #\space #\tab #\return #\newline))
(define-constant +newline-characters+ '(#\newline #\^m #\linefeed #\return))

;;General stuff--------------------------------------------------;;
(defun peek-ahead-no-hang (&optional (stream *standard-input*) (eof-error t) eof-value recursive-p)
  (symbol-macrolet ((pop-char (read-char-no-hang stream eof-error eof-value recursive-p)))
    (loop
       for char = pop-char then pop-char
       and c-prev = nil then char
       until (cond
	       ((member char +parser-ignored-characters+) nil)
	       (t t))
       finally (progn
		 (if char
		     (unread-char char stream)
		     (when (member c-prev +newline-characters+)
		       (unread-char c-prev stream)))
		 (return char)))))

(defun peek-char-no-hang (&optional (stream *standard-input*) (eof-error t) eof-value recursive-p)
  (let ((char (read-char-no-hang stream eof-error eof-value recursive-p)))
    (when char
      (unread-char char stream))
    char))

(defun read-interesting-char (ignored-characters interesting-characters &optional (stream *standard-input*) (eof-error t) eof-value recursive-p)
  (symbol-macrolet ((pop-char (read-char-no-hang stream eof-error eof-value recursive-p)))
    (loop
       for char = pop-char then pop-char
       and c-prev = nil then char
       until (cond
	       ((member char ignored-characters) nil)
	       ((or (null interesting-characters)
		    (member char interesting-characters)) t)
	       ((null char) t)
	       (t nil))
       finally (return char))))

(defun read-until (break-chars &optional (stream *standard-input*)  (eof-error t) eof-value recursive-p)
  (symbol-macrolet ((pop-char (read-char-no-hang stream eof-error eof-value recursive-p)))
    (loop
       :for char = pop-char :then pop-char
       :counting t :into n
       :if (and char (not (member char break-chars)))
         :collect char into ret
       :else
         :do (let ((out (read-from-string
			 (make-array (1- n) :element-type 'character
				     :initial-contents ret))))
	       (return (values out char))))))

;;Array slicer---------------------------------------------------;;
(defun slicing-sym? (x)
  (or (consp x)
      (and (symbolp x)
	   (not (member x '(t nil))))
      (numberp x)))

(defun get-slicing-subscript (lst)
  (flet ((idxp (x)
	   (or (consp x)
	       (and (symbolp x)
		    (not (member x '(t nil))))
	       (numberp x))))
    (cond
      ((eq (first lst) #\:) ; '(\: * *)
       (cond
	 ((null (cdr lst))            ; '(\:)
	  '(quote \:))
	 ((eq (second lst) #\:)       ; '(\: \: *)
	  (cond
	    ((null (cddr lst))                      ; '(\: \:)
	     '(quote \:))
	    ((idxp (third lst))                  ; '(\: \: num)
	     `(list (list '\: ,(third lst)) 0))
	    (t
	     (error 'parser-error))))
	 ((idxp (second lst))      ; '(\: num *)
	  (cond
	    ((or (null (cddr lst))
		 (null (cdddr lst)))                ; '(\: num) or '(\: num \:)
	     `(list '\: 0 ,(second lst)))
	    ((and (eq (third lst) #\:)              ; '(\: num \: num)
		  (idxp (fourth lst)))
	     `(list (list '\: ,(third lst)) 0 ,(second lst)))
	    (t
	     (error 'parser-error))))))
      ((idxp (first lst)) ; '(num *)
       (cond
	 ((null (cdr lst))                ; '(num)
	  (first lst))
	 ((and (eq (second lst) #\:)      ; '(num \:)
	       (null (cddr lst)))
	  `(list '\: ,(first lst)))
	 ((and (eq (second lst) #\:)   ; '(num \: \: *)
	       (eq (third lst) #\:))
	  (cond
	    ((null (cdddr lst))                     ; '(num \: \:)
	     `(list '\: ,(first lst)))
	    ((idxp (fourth lst))                 ; '(num \: \: num)
	     `(list (list '\: ,(fourth lst)) ,(first lst)))
	    (t
	     (error 'parser-error))))
	 ((and (eq (second lst) #\:)   ; '(num \: num *)
	       (idxp (third lst)))
	  (cond
	    ((or (null (cdddr lst))                 ; '(num \: num) or '(num \: num \:)
		 (and (eq (fourth lst) #\:)
		      (null (cddddr lst))))
	     `(list '\: ,(first lst) ,(third lst)))
	    ((and (eq (fourth lst) #\:)     	  ; '(num \: num \: num)
		  (idxp (fifth lst)))
	     `(list (list '\: ,(fifth lst)) ,(first lst) ,(third lst)))
	    (t
	     (error 'parser-error))))))
      (t
       (error 'parser-error)))))

(defun parse-indexing-expression (stream macro-char)
  (declare (ignore macro-char))
  ;;macro-char is assumed to be #\$
  ;;#\[...#\] uses sub-tensor~ (displaced)
  ;;#\{...#\} uses sub-tensor (copied)
  (labels ((pop-char () (read-char stream t nil t))
	   (pop-ichar () (read-interesting-char stream t nil t))
	   (peek () (peek-ahead-no-hang stream t nil t))
	   (idxp (x) (or (consp x)
			 (and (symbolp x)
			      (not (member x '(t nil))))
			 (numberp x)))
	   (get-idx-expr (limlst)
	     (format t "~a~%" limlst)
	     (loop
		for char = (pop-char) then (pop-char)
		counting t into n		
		if (not (member char limlst))
		  collect char into ret
		else
		  do (progn		       
		       (unread-char char stream)
		       (format t "~a ~%" ret)
		       (return (read-from-string (make-array (1- n) :element-type 'character :initial-contents ret) nil nil)))
		end)))
    (let* ((tensor (get-idx-expr `(#\[ #\{ #\$)))
	   (idx-char (pop-ichar))
	   (sub-func (ecase idx-char
		       (#\[ 'matlisp:sub-tensor~)
		       (#\{ 'matlisp:sub-tensor)
		       (#\$ nil)))
	   (cidx-char (case idx-char
			(#\[ #\])
			(#\{ #\}))))
      #+nil(format t "~a ~a ~a~%" tensor idx-char sub-func)
      (labels ((get-index-list (cur-idx ret)
		 ;;#\, is the delimiting character
		 ;;#\: is the slicing character
		 (let ((pchar (peek)))
		   #+nil(format t "pchar: ~a ~%" pchar)
		   (cond
		     ((or (eq pchar cidx-char)
			  (eq pchar #\,))
		      (pop-char)
		      (let ((idx-lst (reverse cur-idx)))
			(when (null idx-lst)
			  (error 'parser-error :message "No slicing argument given."))
			(loop
			   for cur in idx-lst
			   and pcur = nil then cur
			   counting (eq cur #\:) into cnt
			   unless (<= cnt 2)
			   do (error 'parser-error :message "Too many slicing characters.")
			   when (and (idxp pcur) (idxp cur))
			   do (error 'parser-error :message "Invalid syntax specify slicing operation."))
			(push (get-slicing-subscript idx-lst) ret))
		      (if (eq pchar #\,)
			  (get-index-list nil ret)
			  (progn
			    (unless (eq (pop-ichar) #\$)
			      (error 'parser-error :message "Invalid syntax: cannot find closing #\$."))
			    ;;And finally!
			    (cons 'list (reverse ret)))))
		     ((eq pchar #\:)
		      (pop-char)
		      (get-index-list (cons #\: cur-idx) ret))
		     (t
		      (let ((idxe (get-idx-expr (append +parser-ignored-characters+ `(#\: #\, ,cidx-char #\$)))))
			(get-index-list (cons idxe cur-idx) ret)))))))
	(if (null sub-func)
	    tensor	   
	    `(,sub-func ,tensor ,(get-index-list nil nil)))))))

(set-macro-character #\$ #'parse-indexing-expression)

(set-dispatch-macro-character #\# #\d #'msco
#+nil(with-input-from-string (ostr "x[0:5, 0, 0]$ ")
  (parse-indexing-expression ostr #\$))

;;Tensor reader--------------------------------------------------;;
