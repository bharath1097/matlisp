(in-package #:matlisp)

;;Field templates
(deft/generic (t/f+ #'subtypep) ty (&rest nums))
(deft/method t/f+ (ty number) (&rest nums)
 `(cl:+ ,@(mapcar #'(lambda (x) `(the ,ty ,x)) nums)))

(deft/generic (t/f- #'subtypep) ty (&rest nums))
(deft/method t/f- (ty number) (&rest nums)
 `(cl:- ,@(mapcar #'(lambda (x) `(the ,ty ,x)) nums)))

(deft/generic (t/f* #'subtypep) ty (&rest nums))
(deft/method t/f* (ty number) (&rest nums)
 `(cl:* ,@(mapcar #'(lambda (x) `(the ,ty ,x)) nums)))

(deft/generic (t/f/ #'subtypep) ty (&rest nums))
(deft/method t/f/ (ty number) (&rest nums)
 `(cl:/ ,@(mapcar #'(lambda (x) `(the ,ty ,x)) nums)))

(deft/generic (t/f= #'subtypep) ty (&rest nums))
(deft/method t/f= (ty number) (&rest nums)
 `(cl:= ,@(mapcar #'(lambda (x) `(the ,ty ,x)) nums)))

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
  `(coerce ,val ',ty))

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
	       #'(lambda (a b) (dict-compare (list #'subtypep #'subtypep) b a)))
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
;; (t/strict-coerce (fixnum (complex integer)) x) -> (COERCE X '(COMPLEX INTEGER))
