(in-package #:matlisp-libmatlisp)

(def-fortran-routine descal :void
  "
  (DESCAL n dx incx dy incy)

  Multiplies the vector X and Y element-wise.
  Y <- Y .* E
  "
  (n :integer :input)
  (dx (* :double-float :inc head-x) :input)
  (incx :integer :input)
  (dy (* :double-float :inc head-y) :output)
  (incy :integer :output))

(def-fortran-routine zescal :void
  "
  (ZESCAL n dx incx dy incy)

  Multiplies the vector X and Y element-wise.
  Y <- Y .* E
  "
  (n :integer :input)
  (dx (* :complex-double-float :inc head-x) :input)
  (incx :integer :input)
  (dy (* :complex-double-float :inc head-y) :output)
  (incy :integer :output))

(def-fortran-routine dediv :void
  "
  (DEDIV n dx incx dy incy)

  Divides the vector Y by X element-wise.
  Y <- Y .* E
  "
  (n :integer :input)
  (dx (* :double-float :inc head-x) :input)
  (incx :integer :input)
  (dy (* :double-float :inc head-y) :output)
  (incy :integer :output))

(def-fortran-routine zediv :void
  "
  (ZEDIV n dx incx dy incy)

  Divide the vector Y by X element-wise.
  Y <- Y .* E
  "
  (n :integer :input)
  (dx (* :complex-double-float :inc head-x) :input)
  (incx :integer :input)
  (dy (* :complex-double-float :inc head-y) :output)
  (incy :integer :output))
