(in-package :matlisp)

;;Field templates
(deft/generic (t/f+ #'subtypep) ty (&rest nums))
(deft/method t/f+ (ty number) (&rest nums)
  (let* ((decl (zipsym nums))
	 (args (mapcar #'car decl)))
    `(let (,@decl)
       (declare (type ,ty ,@args))
       (cl:+ ,@args))))

(deft/generic (t/f- #'subtypep) ty (&rest nums))
(deft/method t/f- (ty number) (&rest nums)
  (let* ((decl (zipsym nums))
	 (args (mapcar #'car decl)))
    `(let (,@decl)
       (declare (type ,ty ,@args))
       (cl:- ,@args))))

(deft/generic (t/f* #'subtypep) ty (&rest nums))
(deft/method t/f* (ty number) (&rest nums)
  (let* ((decl (zipsym nums))
	 (args (mapcar #'car decl)))
    `(let (,@decl)
       (declare (type ,ty ,@args))
       (cl:* ,@args))))

(deft/generic (t/f/ #'subtypep) ty (&rest nums))
(deft/method t/f/ (ty number) (&rest nums)
  (let* ((decl (zipsym nums))
	 (args (mapcar #'car decl)))
    `(let (,@decl)
       (declare (type ,ty ,@args))
       (cl:/ ,@args))))

;;
(deft/generic (t/fc #'subtypep) ty (num))
(deft/method t/fc (ty number) (num)
  (with-gensyms (num-sym)
    `(let ((,num-sym ,num))
       (cl:conjugate ,num-sym))))
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
(deft/generic (t/f= #'subtypep) ty (&rest nums))
(deft/method t/f= (ty number) (&rest nums)
  (let* ((decl (zipsym nums))
	 (args (mapcar #'car decl)))
    `(let (,@decl)
       (declare (type ,ty ,@args))
       (cl:= ,@args))))

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

;;Tensor specializations
(deft/generic (t/field-type #'subtypep) sym ())
(deft/method t/field-type (sym standard-tensor) ()
  t)

(defun field-type (clname)
  (macroexpand-1 `(t/field-type ,clname)))

;;Beware of infinite loops here.
(deft/generic (t/store-element-type #'subtypep) sym ())
(deft/method t/store-element-type (sym standard-tensor) ()
  (macroexpand-1 `(t/field-type ,sym)))

(defun store-element-type (clname)
  (macroexpand-1 `(t/store-element-type ,clname)))

(deft/generic (t/compute-store-size #'subtypep) sym (size))
(deft/method t/compute-store-size (sym standard-tensor) (size)
  size)

(deft/generic (t/store-allocator #'subtypep) sym (size &optional initial-element))
(deft/method t/store-allocator (sym standard-tensor) (size &optional initial-element)
  (let ((size-sym (gensym))
	(type (macroexpand-1 `(t/store-element-type ,sym))))
    `(let ((,size-sym (t/compute-store-size ,sym ,size)))
       (make-array ,size-sym :element-type ',type :initial-element ,(or initial-element (if (subtypep type 'number) `(t/fid+ ,type) nil))))))

(deft/generic (t/store-type #'subtypep) sym (&optional size))
(deft/method t/store-type (sym standard-tensor) (&optional (size '*))
 `(simple-array ,(store-element-type sym) (,size)))

(defun store-type (cl &optional (size '*))
  (macroexpand-1 `(t/store-type ,cl ,size)))

(deft/generic (t/store-ref #'subtypep) sym (store idx))
(deft/method t/store-ref (sym standard-tensor) (store idx)
  (let ((store-s (gensym))
	(idx-s (gensym)))
    `(let ((,store-s ,store)
	   (,idx-s ,idx))
       (declare (type ,(store-type sym) ,store-s))
       (aref ,store-s ,idx-s))))

(deft/generic (t/store-set #'subtypep) sym (value store idx))
(deft/method t/store-set (sym standard-tensor) (value store idx)
  (let ((store-s (gensym))
	(idx-s (gensym))
	(value-s (gensym))
	(type (macroexpand-1 `(t/field-type ,sym))))
    `(let ((,store-s ,store)
	   (,idx-s ,idx)
	   (,value-s ,value))
       (declare (type ,(store-type sym) ,store-s)
		(type ,type ,value-s))
       (setf (aref ,store-s ,idx-s) ,value-s)
       nil)))

(deft/generic (t/coerce #'subtypep) ty (val))
(deft/method t/coerce (ty number) (val)
  `(coerce ,val ',ty))
;;
;;
(eval-when (:compile-toplevel :load-toplevel :execute)
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
	   (return t))))

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

(defun coerceable? (clx cly)
  (handler-case
      (progn
	(macroexpand-1 `(t/strict-coerce ((t/field-type ,clx) (t/field-type ,cly)) x))
	t)
    (error () nil)))
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

)
