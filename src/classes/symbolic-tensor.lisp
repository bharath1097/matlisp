(in-package #:matlisp)

(deftype symbolic-type ()
  "Symbolic type associated with Maxima"
  '(or number symbol list))

(defleaf symbolic-tensor (standard-tensor) ())

(deft/method t/field-type (sym symbolic-tensor) ()
  'symbolic-type)

;;Field definitions
(deft/method t/f+ (ty symbolic-type) (&rest nums)
 `(maxima::add ,@(mapcar #'(lambda (x) `(the ,ty ,x)) nums)))

(deft/method t/f- (ty symbolic-type) (&rest nums)
  `(,(if (cdr nums) 'maxima::sub 'maxima::neg) ,@(mapcar #'(lambda (x) `(the ,ty ,x)) nums)))  

(deft/method t/f* (ty symbolic-type) (&rest nums)
 `(maxima::mul ,@(mapcar #'(lambda (x) `(the ,ty ,x)) nums)))

(deft/method t/f/ (ty symbolic-type) (&rest nums)
 `(maxima::div ,@(mapcar #'(lambda (x) `(the ,ty ,x)) nums)))

(deft/method t/fid* (ty symbolic-type) ()
  1)

(deft/method t/fid+ (ty symbolic-type) ()
  0)

(deft/method t/f= (ty symbolic-type) (&rest nums)
 `(maxima::equal ,@(mapcar #'(lambda (x) `(the ,ty ,x)) nums)))

(deft/method t/fc (ty symbolic-type) (num)
 `(maxima::meval (list '(maxima::$conjugate maxima::simp) ,num)))

(deft/method t/coerce (ty symbolic-type) (num)
  num)
;;

(definline symbolic-typed.diff (a x)
  (etypecase a
    (symbolic-type
     (maxima::$diff a x))
    (symbolic-tensor
     (make-instance 'symbolic-tensor
		    :dimensions (copy-seq (dimensions a))
		    :store (map 'symbolic-store-vector #'(lambda (f) (maxima::$diff f x)) (store a))))))
;;
