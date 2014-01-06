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
   (with-gensyms (num-sym)
     `(let ((,num-sym ,num))
	(cl:realpart ,num-sym))))

(deft/method t/frealpart (ty real) (num)
  num)
;;
(deft/generic (t/fimagpart #'subtypep) ty (num))

(deft/method t/fimagpart (ty number) (num)
  `(cl:imagpart ,num))

(deft/method t/fimagpart (ty real) (num)
  `(t/fid+ ,ty))
;;
(deft/generic (t/fid+ #'subtypep) ty ())
(deft/method t/fid+ (ty number) ()
  (coerce 0 ty))

(deft/generic (t/fid* #'subtypep) ty ())
(deft/method t/fid* (ty number) ()
  (coerce 1 ty))

;;
;; (deft/generic (t/random #'subtypep) ty (num &optional random-state))
;; (deft/method t/random (sym real) (num &optional random-state)
;;   (if random-state
;;       `(random ,num ,random-state)
;;       `(random ,num)))
