(in-package #:matlisp-utilities)

(defun-compiler-macro lvec-foldl (func vec)
  (declare (type vector))
  (loop
     :for i :of-type fixnum :from 0 :below (length vec)
     :for ret = (aref vec 0) :then (funcall func (aref vec i) ret)
     :finally (return ret)))

(defun-compiler-macro lvec-foldr (func vec)
  (declare (type vector))
  (loop
     :for i :of-type fixnum :downfrom (1- (length vec)) :to 0
     :for ret = (aref vec (1- (length vec))) :then (funcall func (aref vec i) ret)
     :finally (return ret)))

(defun-compiler-macro lvec-max (vec)
  (declare (type vector vec))
  (loop :for ele :across vec
     :for idx :of-type fixnum = 0 :then (+ idx 1)
     :with max :of-type fixnum = (aref vec 0)
     :with max-idx :of-type index-type = 0
     :do (when (> ele max)
	   (setf max ele
		 max-idx idx))
     :finally (return (values max max-idx))))

(defun-compiler-macro lvec-min (vec)
  (declare (type vector vec))
  (loop :for ele :across vec
     :for idx :of-type fixnum = 0 :then (+ idx 1)
     :with min :of-type fixnum = (aref vec 0)
     :with min-idx :of-type index-type = 0
     :do (when (< ele min)
	   (setf min ele
		 min-idx idx))
     :finally (return (values min min-idx))))

(defun-compiler-macro lvec-eq (va vb &optional (test #'eq))
  (declare (type vector va vb))
  (let ((la (length va))
	(lb (length vb)))
    (if (/= la lb) nil
	(loop
	   :for ele-a :across va
	   :for ele-b :across vb
	   :unless (funcall test ele-a ele-b)
	     :do (return nil)
	   :finally (return t)))))

(defun-compiler-macro lvec->list (va)
  (declare (type vector va))
  (loop :for ele :across va
       :collect ele))

(defun-compiler-macro lvec->list! (va la)
  (declare (type vector va)
	   (type list la))
  (loop
     :for ele :across va
     :for lst = la :then (cdr lst)
     :do (setf (car lst) ele))
  la)
