(in-package #:matlisp)

;;Field templates
(deft/generic (t/f+ #'subtypep) ty (&rest nums))
(deft/generic (t/f- #'subtypep) ty (&rest nums))
(deft/generic (t/f* #'subtypep) ty (&rest nums))
(deft/generic (t/f/ #'subtypep) ty (&rest nums))
(deft/generic (t/f= #'subtypep) ty (&rest nums))

(macrolet ((def-marith (tname clop)
	     `(deft/method ,tname (ty number) (&rest nums)
		(if (and (consp ty) (eql (first ty) 'mod))
		    `(mod (,',clop ,@(mapcar #'(lambda (x) `(the ,ty ,x)) nums)) ,(second ty))
		    `(,', clop ,@(mapcar #'(lambda (x) `(the ,ty ,x)) nums)))))
	   (genarith ((&rest args))
	     `(progn ,@(mapcar #'(lambda (x) `(def-marith ,(car x) ,(cadr x))) args))))
  (genarith ((t/f+ cl:+)
	     (t/f- cl:-)
	     (t/f* cl:*))))

(deft/method t/f= (ty number) (&rest nums)
 `(cl:= ,@(mapcar #'(lambda (x) `(the ,ty ,x)) nums)))

;;
(definline eeuclid (a b)
  (declare (type fixnum a b))
  (let ((ss 0) (s.pr 1)
	(tt 1) (t.pr 0)
	(r b) (r.pr a)
	(tmp 0))
    (declare (type fixnum ss s.pr tt t.pr r r.pr tmp))
    (very-quickly
      (loop :while (/= r 0)
	 :do (multiple-value-bind (quo rem) (floor r.pr r)
	       (declare (type fixnum quo rem))
	       (setf r.pr r
		     r rem
		     tmp ss
		     ss (- s.pr (the fixnum (* quo ss)))
		     s.pr tmp
		     tmp tt
		     tt (- t.pr (the fixnum (* quo tt)))
		     t.pr tmp))))
    (values s.pr t.pr r.pr)))

(deft/method t/f/ (ty number) (&rest nums)
  (if (and (consp ty) (eql (car ty) 'mod))
      (cond
	((cddr nums) `(t/f/ ,ty ,(car nums) (t/f* ,ty ,@(cdr nums))))
	((not (cdr nums)) `(t/f/ ,ty (t/fid* ,ty) ,(car nums)))
	(t
	 (with-gensyms (s tt g a b)
	   `(let ((,a ,(first nums)) (,b ,(second nums)))
	      (declare (type ,ty ,a ,b))
	      (multiple-value-bind (,s ,tt ,g) (eeuclid ,(second ty) ,b)
		(declare (ignore ,s))
		(if (cl:= ,g (cl:gcd ,a ,g))
		    (t/coerce ,ty (cl:* ,tt (cl:/ ,a ,g)))
		    (error "Cannot solve equation ~a * x = ~a mod ~a" ,a ,b ,(second ty))))))))
      `(cl:/ ,@(mapcar #'(lambda (x) `(the ,ty ,x)) nums))))

;;
(deft/generic (t/fid+ #'subtypep) ty ())
(deft/method t/fid+ (ty number) ()
  (coerce 0 ty))

(deft/generic (t/fid* #'subtypep) ty ())
(deft/method t/fid* (ty number) ()
  (coerce 1 ty))
;;
(deft/generic (t/fc #'subtypep) ty (num))
(deft/method t/fc (ty number) (num)
 `(cl:conjugate ,num))

(deft/method t/fc (ty real) (num)
  num)

(defgeneric fc (x)
  (:method ((x complex))
    (cl:conjugate x))
  (:method ((x real))
    x)
  (:method ((x t))
    (let ((clname (class-name (class-of x))))
      (compile-and-eval
       `(defmethod fconj ((x ,clname))
	  (t/fc ,clname x)))
      (fc x))))
(defun field-realp (fil)
  (eql (macroexpand-1 `(t/fc ,fil phi)) 'phi))
;;

(deft/generic (t/frealpart #'subtypep) ty (num))
(deft/method t/frealpart (ty number) (num)
  `(cl:realpart ,num))
(deft/method t/frealpart (ty real) (num)
  num)

(deft/generic (t/fimagpart #'subtypep) ty (num))
(deft/method t/fimagpart (ty number) (num)
  `(cl:imagpart ,num))
(deft/method t/fimagpart (ty real) (num)
  `(t/fid+ ,ty))

;; (deft/generic (t/random #'subtypep) ty (num &optional random-state))
;; (deft/method t/random (sym real) (num &optional random-state)
;;   (if random-state
;;       `(random ,num ,random-state)
;;       `(random ,num)))

(deft/generic (t/coerce #'subtypep) ty (val))
(deft/method t/coerce (ty number) (val)
  (if (and (consp ty) (eql (first ty) 'mod))
      `(mod (coerce ,val 'fixnum) ,(second ty))
      `(coerce ,val ',ty)))

(eval-every
  (defun strict-compare (func-list a b)
    (loop :for func :in func-list
       :for elea :in a
       :for eleb :in b
       :do (unless (funcall func elea eleb)
	     (return nil))
       :finally (return t)))

  (defun dict-compare (func-list a b)
    (loop :for func :in func-list
       :for elea :in a
       :for eleb :in b
       :do (when (funcall func elea eleb)
	     (return t)))))

;;This one is hard to get one's brain around.
(deft/generic (t/strict-coerce
	       #'(lambda (a b) (strict-compare (list #'subtypep #'(lambda (x y) (subtypep y x))) a b))
	       #'(lambda (a b) (dict-compare (list #'subtypep #'subtypep) b a))
	       sort)
    (from to) (val))

;;Anything can be coerced into type "t"
(deft/method t/strict-coerce ((from t) (to t)) (val)
  val)

;;Any number can be coerced into 'double-float (with loss of precision of course)
(deft/method t/strict-coerce ((from real) (to double-float)) (val)
 `(coerce ,val ',to))

;;-do-
(deft/method t/strict-coerce ((from real) (to single-float)) (val)
 `(coerce ,val ',to))

;;Any number can be coerced into '(complex double-float) (with loss of precision of course)
(deft/method t/strict-coerce ((from number) (to (complex double-float))) (val)
 `(coerce ,val ',to))
;;-do-
(deft/method t/strict-coerce ((from number) (to (complex single-float))) (val)
 `(coerce ,val ',to))

;;
;; (deft/method t/strict-coerce ((from fixnum) (to (complex fixnum))) (val)
;;  `(coerce ,val ',to))

;; (deft/method t/strict-coerce ((from integer) (to (complex integer))) (val)
;;  `(coerce ,val ',to))

;; (t/strict-coerce (number (complex double-float)) x) -> (COERCE X '(COMPLEX DOUBLE-FLOAT))
;; (t/strict-coerce (complex (complex double-float)) x) -> (COERCE X '(COMPLEX DOUBLE-FLOAT))
;; (t/strict-coerce (real (complex double-float)) x) -> (COERCE X '(COMPLEX DOUBLE-FLOAT))
;; (t/strict-coerce (real complex) x) -> error: template not defined
;; (t/strict-coerce (fixnum double-float) x) -> (COERCE X 'DOUBLE-FLOAT)
;; (t/strict-coerce (fixnum fixnum) x) -> error: template not defined
;; (t/strict-coerce (fixnum real) x) -> (COERCE X 'REAL)
;; (t/strict-coerce (double-float t) x) -> X
