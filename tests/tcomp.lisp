(in-package :matlisp)

(defparameter *tclause* '(einstein-sum (ref C i j) (* (ref A i k) (ref A j k))))
(defparameter *mclause* '(einstein-sum (ref C i j) (* (ref A i k) (ref B k j))))

(loop-generator 'real-tensor '(k j i) (second *tclause*) (third *tclause*))
(loop-generator 'real-tensor '(i j k) (second *mclause*) (third *mclause*) :tight-iloop t)

(defun mm-test-simple (a b c)
  (declare (type real-tensor a b c))  
  (einstein-sum real-tensor (j k i) (ref c i j) (* (ref a i k) (ref b k j))))

(define-einstein-sum mm-test (a b c) (real-tensor (ref c i j) (* (ref a i k) (ref b k j))))

(let ((thingy #'(lambda (a b c) (einstein-sum real-tensor (j k i) (ref c i j) (* (ref a i k) (ref b k j))))))
  (let ((x (copy! #2a((1 2) (3 4)) (zeros '(2 2))))
	(y (copy! #2a((4 5) (6 5)) (zeros '(2 2))))
	(z (zeros '(2 2))))
    (time (dotimes (i 1000) (funcall thingy x y z)))))

(defun mat-square (a c)
  (einstein-sum real-tensor (j k i) (ref c i j) (* (ref a i k) (ref a k j)) t))

(let ((x (copy! #2a((1 2) (3 4)) (zeros '(2 2))))
      (y (copy! #2a((4 5) (6 5)) (zeros '(2 2))))
      (z (zeros '(2 2))))
  (time
   (dotimes (i 1000)
     (copy! 0 z)
     (mm-test x y z)))
  (print (mm-test x y (zeros '(2 2) 'real-tensor))))

(let ((x (copy! #2a((1 2) (3 4)) (zeros '(2 2))))
      (y (copy! #2a((4 5) (6 5)) (zeros '(2 2))))
      (z (zeros '(2 2))))
  (time  (mm-test x y z)))

(let ((x (zeros '(1000 1000)))
      (y (transpose! (zeros '(1000 1000))))
      (z (zeros '(1000 1000))))
  (let-typed ((sto-x (store x) :type (simple-array double-float))
	      (sto-y (store y) :type (simple-array double-float)))
	     (loop :for i :from 0 :below (array-dimension sto-x 0)
		:do (setf (aref sto-x i) (random 1d0)
			  (aref sto-y i) (random 1d0))))
  (time (mm-test x y z))
  t)
