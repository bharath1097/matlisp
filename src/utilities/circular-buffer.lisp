(in-package #:matlisp-utilities)

;;Circular buffer
(defstruct circular-buffer
  (store #() :type vector)
  (idx 0 :type fixnum)
  (count 0 :type fixnum))

(defun make-cbuf (size)
  (declare (type fixnum size))
  (make-circular-buffer :store (make-array size)))

(defun push-cbuf (ele buf)
  (declare (type circular-buffer buf))
  (with-slots (store idx count) buf
    (let* ((len (length store))
	   (nidx (mod (1- idx) len)))
      (when (< count len)
	(incf count))
      (setf (aref store nidx) ele
	    idx nidx)))
  buf)

(defun push-at-end-cbuf (ele buf)
  (declare (type circular-buffer buf))
  (with-slots (store idx count) buf
    (let* ((len (length store)))
      (if (= count len)
	  (setf idx (mod (1+ idx) len))
	  (incf count))
      (setf (aref store (mod (+ idx count -1) len)) ele)))
  buf)

(defun pop-cbuf (buf)
  (declare (type circular-buffer buf))
  (with-slots (store idx count) buf
    (if (= count 0) nil
	(prog1 (aref store idx)
	  (setf idx (mod (1+ idx) (length store))
		count (1- count))))))

(defun pop-from-end-cbuf (buf)
  (declare (type circular-buffer buf))
  (with-slots (store idx count) buf
    (if (= count 0) nil	
	(let* ((len (length store)))
	  (prog1 (aref store (mod (+ idx count -1) len))
	    (decf count))))))
;;


