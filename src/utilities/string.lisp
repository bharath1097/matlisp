(in-package #:matlisp-utilities)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (inline string+))
  (defun string+ (&rest strings)
    (apply #'concatenate (cons 'string strings)))

  (defun format-to-string (fmt &rest args)
    (apply #'format (append (list nil fmt) args)))

  #+(not :sbcl)
  (defun file->string (path)
    "Sucks up an entire file from PATH into a freshly-allocated string,
returning two values: the string and the number of bytes read."
    (declare (optimize (safety 0) (speed 3)))
    (with-open-file (s path :external-format :iso8859-1)
      (let* ((len (file-length s))
	     (data (make-array len :element-type 'standard-char)))
	(values data (read-sequence data s)))))

  #+sbcl
  (defun file->string (path)
    "Sucks up an entire file from PATH into a freshly-allocated string,
returning two values: the string and the number of bytes read."
    (let* ((fsize (with-open-file (s path)
		    (file-length s)))
	   (data (make-array fsize :element-type 'standard-char))
	   (fd (sb-posix:open path 0)))
      (unwind-protect (sb-posix:read fd (sb-sys:vector-sap data) fsize)
	(sb-posix:close fd))
      (values data fsize)))

  (defun split-seq (test seq &key (filter-empty? t) max-cuts from-end?)
    "Split a sequence, wherever the given character occurs."
    (if (not from-end?)
	(let ((split-list nil)
	      (split-count 0))
	    (loop :for i :from 0 :to (length seq)
	       :with len := (length seq)
	       :with prev := 0
	       :do (let ((cuts-exceeded? (and max-cuts (>= split-count max-cuts))))
		     (when (or (= i len) cuts-exceeded? (funcall test (aref seq i)))
		       (let* ((str (subseq seq prev (if cuts-exceeded? len i))))
			 (when (or cuts-exceeded? (< prev i) (not filter-empty?))
			   (incf split-count)
			   (push str split-list))
			 (setf prev (1+ i))))
		     (when cuts-exceeded? (return))))
	    (values (reverse split-list) (1- split-count)))
	(let ((split-list nil)
	      (split-count 0))       
	  (loop :for i :from (1- (length seq)) :downto -1
	     :with prev := (length seq)
	     :do (let ((cuts-exceeded? (and max-cuts (>= split-count max-cuts))))
		   (when (or (< i 0) cuts-exceeded? (funcall test (aref seq i)))
		     (let ((str (subseq seq (if cuts-exceeded? 0 (1+ i)) prev)))
		       (when (or cuts-exceeded? (< (1+ i) prev) (not filter-empty?))
			 (incf split-count)
			 (push str split-list))
		       (setf prev i)))
		   (when cuts-exceeded? (return))))
	  (values split-list split-count))))
  ;;
  (defun splitlines (string)
    "Split the given string wherever the Carriage-return occurs."
    (split-seq #'(lambda (x) (or (char= x #\Newline) (char= x #\Return))) string))

)
