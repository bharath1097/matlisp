(in-package :matlisp)
(defvar *current-octave-process* nil)

(defun open-octave-stream (&key (octave-binary "/usr/bin/octave"))
  (setf *current-octave-process* (#+:sbcl
				   sb-ext:run-program
				   #+:ccl
				   ccl:run-program
				   octave-binary nil :input :stream :wait nil :output :stream))
  (octave-send "format long E; 1 ~%")
  (let ((stream (#+:sbcl
		 sb-ext:process-output
		 *current-octave-process*)))
    (loop :repeat 1000
       :do (let ((out (car (split-seq #'(lambda (x) (char= x #\Space)) (read-line stream)))))
	     (when (string= out "ans")
	       (return *current-octave-process*))))))

(defun close-octave-stream ()
  (when *current-octave-process*
    (octave-send "quit~%")
    (setf *current-octave-process* nil)))
;;
(defun octave-send (str &rest args)
  (unless *current-octave-process*
    (setf *current-octave-process* (open-octave-stream)))
  (let ((stream (#+:sbcl
		 sb-ext:process-input
		 #+:ccl
		 ccl:external-process-input-stream
		 *current-octave-process*)))
    (apply #'format (append (list stream str) args))
    (finish-output stream)))

(defun octave-clear ()
  (octave-send "clear~%"))

(defun octave-readnum ()
  (let ((stream (#+:sbcl
		 sb-ext:process-output
		 *current-octave-process*)))
    (let ((str (split-seq #'(lambda (c) (char= c #\Space)) (read-line stream))))
      (cond
	((= (length str) 5)
	 (let ((real (third str))
	       (imag (fifth str)))
	   (setf (aref real (position #\E real)) #\D)
	   (setf (aref imag (position #\E imag)) #\D)
	   (setf imag (subseq imag 0 (1- (length imag))))
	   (if (string= (fourth str) "+")
	       (complex (read-from-string real) (read-from-string imag))
	       (complex (read-from-string real) (- (read-from-string imag))))))
	((= (length str) 3)
	 (let ((real (third str)))
	   (setf (aref real (position #\E real)) #\D)
	   (nth-value 0 (read-from-string real))))))))

(defun octave-send-tensor (mat name)
  (octave-send "~a = zeros(~{~a~^, ~});~%" name (if (> (order mat) 1) (dims mat) (append (dims mat) (list 1))))
  (mod-dotimes (idx (dimensions mat))
    :do (let ((ref (ref mat idx)))
	  (if (complexp ref)
	      (octave-send "~a(~{~a~^, ~}) = ~a + ~a * 1i;~%" name (mapcar #'1+ (lvec->list idx)) (realpart ref) (imagpart ref)) 
	      (octave-send "~a(~{~a~^, ~}) = ~a;~%" name (mapcar #'1+ (lvec->list idx)) ref)))))

(defun octave-read-tensor (name)
    (let ((stream (#+:sbcl
		   sb-ext:process-output
		   *current-octave-process*))
	  (dims nil)
	  (ret nil)
	  (complex? nil))
      ;;
      (octave-send "size(~a)~%" name)
      (read-line stream) (read-line stream)
      (let ((out (mapcar
		   #'(lambda (x) (setf (aref x (position #\E x)) #\D) (ceiling (read-from-string x)))
		   (split-seq #'(lambda (c) (char= c #\Space)) (read-line stream)))))
	(read-line stream)
	(setq dims out))
      ;;
      (octave-send "sum(abs(imag(~a(:))))~%" name)
      (setq ret
	    (if (= (octave-readnum) 0d0)
		(zeros dims 'real-tensor)
		(prog1 (zeros dims 'complex-tensor) (setq complex? t))))
      ;;
      (mod-dotimes (idx (dimensions ret))
	:do (setf (ref ret idx) (progn (octave-send "~a(~{~a~^, ~})~%" name (mapcar #'1+ (lvec->list idx))) (octave-readnum))))
      ret))
