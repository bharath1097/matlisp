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

  (declaim (inline split-seq))
  (defun split-seq (test seq &key max-cuts)
    "Split a sequence, wherever the given character occurs."
    (let ((split-list nil) (split-count 0) (deletes nil))
      (labels ((left-split (prev i)
		 (if (not deletes)
		     (when (< prev i)
		       (push (subseq seq prev i) split-list)
		       (incf split-count))
		     (do ((dlst deletes (or (cdr dlst) (cons (1- prev) t)))
			  (pele i (car dlst))
			  (ret nil))
			 ((eql dlst t) (progn (setf deletes nil)
					      (when ret
						(push (apply #'string+ ret) split-list)
						(incf split-count))))
		       (let ((ele (car dlst)))
			 (when (< (1+ ele) pele)
			   (push (subseq seq (1+ ele) pele) ret)))))))
	(loop :for i :from 0 :to (length seq)
	   :with len := (length seq)
	   :with prev := 0
	   :do (let ((cmd nil))
		 (cond
		   ((or (= i len) (and max-cuts (>= split-count max-cuts)))
		    (left-split prev len)
		    (return))
		   ((setf cmd (funcall test (aref seq i)))
		    (case cmd
		      (:left
		       (left-split prev (1+ i))
		       (setf prev (1+ i)))
		      (:right
		       (left-split prev i)
		       (setf prev i))
		      (:keep
		       (left-split prev i)
		       (push (string (aref seq i)) split-list)
		       (incf split-count)
		       (setf prev (1+ i)))
		      (:delete
		       (push i deletes))
		      (t
		       (left-split prev i)
		       (setf prev (1+ i)))))))))
      (values (nreverse split-list) (1- split-count))))

  ;;
  (defun splitlines (string)
    "Split the given string wherever the Carriage-return occurs."
    (split-seq #'(lambda (x) (or (char= x #\Newline) (char= x #\Return))) string))

)
