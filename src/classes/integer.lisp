(in-package :matlisp)

(defleaf rational-tensor (real-numeric-tensor) ())
(deft/method t/field-type (sym rational-tensor) ()
  'rational)

(defleaf fixnum-tensor (numeric-tensor) ())
(deft/method t/field-type (sym fixnum-tensor) ()
  'fixnum)

;;and voila..
(defleaf u8-tensor (numeric-tensor) ())
(deft/method t/field-type (sym u8-tensor) ()
  '(unsigned-byte 8))

(defleaf boolean-tensor (numeric-tensor) ())
(deft/method t/field-type (sym boolean-tensor) ()
  '(mod 2))
