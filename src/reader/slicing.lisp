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



(set-macro-character #\$ #'parse-indexing-expression)

(set-dispatch-macro-character #\# #\d #'msco
#+nil(with-input-from-string (ostr "x[0:5, 0, 0]$ ")
  (parse-indexing-expression ostr #\$))

;;Tensor reader--------------------------------------------------;;
