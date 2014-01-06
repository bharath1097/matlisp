(in-package #:matlisp)

;;Tensor specializations
(deft/generic (t/field-type #'subtypep) sym ())
(deft/method t/field-type (sym standard-tensor) ()
  t)

(eval-every
  (defun field-type (clname)
    (macroexpand-1 `(t/field-type ,clname))))
;;This is useful for Eigenvalue decompositions
(deft/generic (t/complexified-type #'subtypep) sym ())
(eval-every 
  (defun complexified-type (type)
    (macroexpand-1 `(t/complexified-type ,type))))

;;Beware of infinite loops here.
(deft/generic (t/store-element-type #'subtypep) sym ())
(deft/method t/store-element-type (sym standard-tensor) ()
  (macroexpand-1 `(t/field-type ,sym)))
(eval-every
  (defun store-element-type (clname)
    (macroexpand-1 `(t/store-element-type ,clname))))
;;
(deft/generic (t/compute-store-size #'subtypep) sym (size))
(deft/method t/compute-store-size (sym standard-tensor) (size)
  size)
;;
(deft/generic (t/store-size #'subtypep) sym (ele))
(deft/method t/store-size (sym standard-tensor) (ele)
  `(length ,ele))
;;
(deft/generic (t/store-allocator #'subtypep) sym (size &optional initial-element))
(deft/method t/store-allocator (sym standard-tensor) (size &optional initial-element)
  (with-gensyms (size-sym arr idx init)
    (let ((type (macroexpand-1 `(t/store-element-type ,sym))))
      `(let*-typed ((,size-sym (t/compute-store-size ,sym ,size))
		    ,@(when initial-element `((,init ,initial-element :type ,(field-type sym))))
		    (,arr (make-array ,size-sym :element-type ',type :initial-element ,(if (subtypep type 'number) `(t/fid+ ,type) nil)) :type ,(store-type sym)))
	,@(when initial-element
		`((very-quickly
		    (loop :for ,idx :from 0 :below ,size-sym
		       :do (t/store-set ,sym ,init ,arr ,idx)))))
	,arr))))
;;
(deft/generic (with-field-element #'subtypep) sym (decl &rest body))
(deft/method with-field-element (sym standard-tensor) (decl &rest body)
  (destructuring-bind (var init &optional (count 1)) decl
    `(let-typed ((,var (t/store-allocator ,sym ,count ,init) :type ,(store-type sym)))
       (locally
	   ,@body))))
;;
(deft/generic (t/store-type #'subtypep) sym (&optional size))
(deft/method t/store-type (sym standard-tensor) (&optional (size '*))
 `(simple-array ,(store-element-type sym) (,size)))

(defun store-type (cl &optional (size '*))
  (macroexpand-1 `(t/store-type ,cl ,size)))

(deft/generic (t/store-ref #'subtypep) sym (store idx))
(deft/method t/store-ref (sym standard-tensor) (store idx)
  `(aref (the ,(store-type sym) ,store) (the index-type ,idx)))

(deft/generic (t/store-set #'subtypep) sym (value store idx))
(deft/method t/store-set (sym standard-tensor) (value store idx)
  `(setf (aref (the ,(store-type sym) ,store) (the index-type ,idx)) (the ,(field-type sym) ,value)))

(deft/generic (t/coerce #'subtypep) ty (val))
(deft/method t/coerce (ty number) (val)
  `(coerce ,val ',ty))
;;
;;
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

(eval-every
  (defun coerceable? (clx cly)
    (handler-case
	(progn
	  (macroexpand-1 `(t/strict-coerce ((t/field-type ,clx) (t/field-type ,cly)) x))
	  t)
      (error () nil))))
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

