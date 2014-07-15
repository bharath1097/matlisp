(in-package :matlisp)

(defleaf rational-tensor (numeric-tensor) ())
(deft/method t/field-type (sym rational-tensor) ()
  'rational)

(defleaf fixnum-tensor (numeric-tensor) ())
(deft/method t/field-type (sym fixnum-tensor) ()
  'fixnum)

;;Define a new tensor class
(defleaf mod2-tensor (standard-tensor) ())

(deft/method t/field-type (sym mod2-tensor) ()
  '(mod 2))

;;and voila..
(defleaf u8-tensor (numeric-tensor) ())
(deft/method t/field-type (sym u8-tensor) ()
  '(unsigned-byte 8))

(defleaf gf2-tensor (numeric-tensor) ())
(deft/method t/field-type (sym gf2-tensor) ()
  '(mod 2))
