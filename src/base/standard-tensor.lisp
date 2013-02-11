(in-package #:matlisp)

;;Alias for fixnum.
(deftype index-type ()
  'fixnum)

(deftype index-store-vector (&optional (size '*))
  `(simple-array index-type (,size)))

(make-array-allocator allocate-index-store 'index-type 0
"
  Syntax
  ======
  (ALLOCATE-INDEX-STORE SIZE [INITIAL-ELEMENT 0])

  Purpose
  =======
  Allocates index storage.")

(definline make-index-store (contents)
  "
  Syntax
  ======
  (MAKE-INDEX-STORE &rest CONTENTS)

  Purpose
  =======
  Allocates index storage with initial elements from the list CONTENTS."
  (make-array (length contents) :element-type 'index-type
	      :initial-contents contents))

(definline idxv (&rest contents)
  (make-index-store contents))

;;
(defclass standard-tensor ()
  ((rank
    :accessor rank
    :type index-type
    :documentation "Rank of the tensor: number of arguments for the tensor")
   (dimensions    
    :accessor dimensions
    :initarg :dimensions
    :type index-store-vector
    :documentation "Dimensions of the vector spaces in which the tensor's arguments reside.")
   (number-of-elements
    :accessor number-of-elements
    :type index-type
    :documentation "Total number of elements in the tensor.")
   ;;
   (parent-tensor
    :accessor parent-tensor
    :initarg :parent-tensor
    :type standard-tensor
    :documentation "If the tensor is a view of another tensor, then this slot is bound.")
   ;;
   (head
    :initarg :head
    :initform 0
    :accessor head
    :type index-type
    :documentation "Head for the store's accessor.")
   (strides
    :initarg :strides
    :accessor strides
    :type index-store-vector
    :documentation "Strides for accesing elements of the tensor.")
   (store-size
    :initarg :store-size
    :accessor store-size
    :type index-type
    :documentation "Size of the store.")
   (store
    :initarg :store
    :accessor store
    :documentation "The actual storage for the tensor."))
  (:documentation "Basic tensor class."))

;;
(defclass standard-matrix (standard-tensor)
  ((rank
    :accessor rank
    :allocation :class
    :type index-type
    :initform 2
    :documentation "For a matrix, rank = 2."))
  (:documentation "Basic matrix class."))

(defmethod initialize-instance :after ((matrix standard-matrix) &rest initargs)
  (declare (ignore initargs))
  (assert (= (rank matrix) 2) nil 'tensor-not-matrix :rank (rank matrix) :tensor matrix))

(defmethod update-instance-for-different-class :before ((old standard-tensor) (new standard-matrix) &rest initargs)
  (declare (ignore initargs))
  (assert (= (rank old) 2) nil 'tensor-not-matrix :rank (rank old)))

;;
(defclass standard-vector (standard-tensor)
  ((rank
    :accessor rank
    :allocation :class
    :type index-type
    :initform 1
    :documentation "For a vector, rank = 1."))
  (:documentation "Basic vector class."))

(defmethod initialize-instance :after ((vector standard-vector) &rest initargs)
  (declare (ignore initargs))
  (assert (= (rank vector) 1) nil 'tensor-not-vector :rank (rank vector) :tensor vector))

(defmethod update-instance-for-different-class :before ((old standard-tensor) (new standard-vector) &rest initargs)
  (declare (ignore initargs))
  (assert (= (rank old) 1) nil 'tensor-not-vector :rank (rank old)))

;;
(defvar *tensor-class-optimizations* (make-hash-table)
  "
  Contains a either:
  o A property list containing:  
  :field-type -> Field type
  :f+ (a b) -> a + b
  :f- (a b) -> a + (- b)
  :finv+ (a) -> -a
  :fid+ () -> + identity
  :f* (a b) -> a * b
  :f/ (a b) -> a * b^{-1}
  :finv* (a) -> 1/a
  :fid* () -> * identity
  :f= (a b) -> (= a b)
  :fconj (a) -> a^* {if nil, Field does not have a conjugation op}

  :coercer (ele) -> Coerced to store-type, with error checking
  :coercer-unforgiving (ele) -> Coerced to store-type, no error checking

  :store-allocator (n) -> Allocates a store of size n
  :store-type
  :reader (store idx) => result
  :value-writer (value store idx) => (store idx) <- value
  :reader-writer (fstore fidx tstore tidx) => (tstore tidx) <- (fstore fidx)
  :swapper       (fstore fidx tstore tidx) => (tstore tidx) <-> (fstore fidx)
  o class-name (symbol) of the superclass whose optimizations
  are to be made use of.")

(definline get-tensor-class-optimization (clname)
  (declare (type symbol clname))
  (symbol-plist clname))

(definline get-tensor-object-optimization (obj)
  (symbol-plist (class-name (class-of obj))))

(defun get-tensor-class-optimization-hashtable (clname)
  (let ((opt (gethash clname *tensor-class-optimizations*)))
    (cond
      ((null opt) nil)
      ((symbolp opt)
       (get-tensor-class-optimization opt))
      (t (values opt clname)))))

(defun (setf get-tensor-class-optimization) (value clname)
  (setf (gethash clname *tensor-class-optimizations*) value)
  (let ((opt (if (symbolp value)
		 (get-tensor-class-optimization-hashtable clname)
		 value)))
    (setf (symbol-plist (getf opt :tensor)) opt
	  (symbol-plist (getf opt :matrix)) opt
	  (symbol-plist (getf opt :vector)) opt)))

;; Akshay: I have no idea what this does, or why we want it
;; (inherited from standard-matrix.lisp
(defmethod make-load-form ((tensor standard-tensor) &optional env)
  "
  MAKE-LOAD-FORM allows us to determine a load time value for
  tensor, for example #.(make-tensors ...)"
  (make-load-form-saving-slots tensor :environment env))

;;
(defun store-indexing-vec (idx hd strides dims)
"
  Syntax
  ======
  (STORE-INDEXING-VEC IDX HD STRIDES DIMS)

  Purpose
  =======
  Does error checking to make sure IDX is not out of bounds.
  Returns the sum:

    length(STRIDES)
       __
  HD + \  STRIDE  * IDX
       /_        i      i
     i = 0
"
  (declare (type index-type hd)
	   (type index-store-vector idx strides dims))
  (let-typed ((rank (length strides) :type index-type))
    (assert (= rank (length idx) (length dims)) nil 'tensor-index-rank-mismatch :index-rank (length idx) :rank rank)
    (very-quickly
      (loop
	 :for i :of-type index-type :from 0 :below rank
	 :for cidx :across idx
	 :with sto-idx :of-type index-type := hd
	 :do (progn
	       (assert (< -1 cidx (aref dims i)) nil 'tensor-index-out-of-bounds :argument i :index cidx :dimension (aref dims i))
	       (incf sto-idx (the index-type (* (aref strides i) cidx))))
	 :finally (return sto-idx)))))

(defun store-indexing-lst (idx hd strides dims)
"
  Syntax
  ======
  (STORE-INDEXING-LST IDX HD STRIDES DIMS)

  Purpose
  =======
  Does error checking to make sure idx is not out of bounds.
  Returns the sum:

    length(STRIDES)
       __
  HD + \  STRIDE  * IDX
       /_        i      i
     i = 0
"
  (declare (type index-type hd)
	   (type index-store-vector strides dims)
	   (type cons idx))
  (let-typed ((rank (length strides) :type index-type))
    (assert (= rank (length dims)) nil 'tensor-dimension-mismatch)
    (very-quickly
      (loop :for cidx :of-type index-type :in idx
	 :for i :of-type index-type := 0 :then (1+ i)
	 :with sto-idx :of-type index-type := hd
	 :do (progn
	       (assert (< -1 cidx (aref dims i)) nil 'tensor-index-out-of-bounds :argument i :index cidx :dimension (aref dims i))
	       (incf sto-idx (the index-type (* (aref strides i) cidx))))
	 :finally (progn
		    (assert (= (1+ i) rank) nil 'tensor-index-rank-mismatch :index-rank (1+ i) :rank rank)
		    (return sto-idx))))))

(definline store-indexing (idx tensor)
"
  Syntax
  ======
  (STORE-INDEXING IDX TENSOR)

  Purpose
  =======
  Returns the linear index of the element pointed by IDX.
  Does error checking to make sure idx is not out of bounds.
  Returns the sum:

    length(STRIDES)
       __
  HD + \  STRIDES  * IDX
       /_        i      i
     i = 0
"
  (declare (type standard-tensor tensor)
	   (type (or index-store-vector cons) idx))
  (typecase idx
    (cons (store-indexing-lst idx (head tensor) (strides tensor) (dimensions tensor)))
    (vector (store-indexing-vec idx (head tensor) (strides tensor) (dimensions tensor)))))
;;

(defmacro with-order (order &rest code)
  `(let ((*default-stride-ordering* ,order))
     ,@code))

;;
(defmethod initialize-instance :after ((tensor standard-tensor) &rest initargs)
  (declare (ignore initargs))
  (let-typed ((dims (dimensions tensor) :type index-store-vector))
    (setf (rank tensor) (length dims))
    (when *check-after-initializing?*
      (assert (>= (head tensor) 0) nil 'tensor-invalid-head-value :head (head tensor) :tensor tensor)
      (if (not (slot-boundp tensor 'strides))
	  (multiple-value-bind (stds size) (make-stride dims)
	    (declare (type index-store-vector stds)
		     (type index-type size))
	    (setf (number-of-elements tensor) size
		  (strides tensor) stds)
	    (assert (<= (+ (head tensor) (1- (number-of-elements tensor))) (store-size tensor)) nil 'tensor-insufficient-store :store-size (store-size tensor) :max-idx (+ (head tensor) (1- (number-of-elements tensor))) :tensor tensor))
	  (very-quickly
	    (let-typed ((stds (strides tensor) :type index-store-vector))
		       (loop :for i :of-type index-type :from 0 :below (rank tensor)
			  :for sz :of-type index-type := (aref dims 0) :then (the index-type (* sz (aref dims i)))
			  :for lidx :of-type index-type := (the index-type (* (aref stds 0) (1- (aref dims 0)))) :then (the index-type (+ lidx (the index-type (* (aref stds i) (1- (aref dims i))))))
			  :do (progn
				(assert (> (aref stds i) 0) nil 'tensor-invalid-stride-value :argument i :stride (aref stds i) :tensor tensor)
				(assert (> (aref dims i) 0) nil 'tensor-invalid-dimension-value :argument i :dimension (aref dims i) :tensor tensor))
			  :finally (progn
				     (assert (>= (the index-type (store-size tensor)) (the index-type (+ (the index-type (head tensor)) lidx))) nil 'tensor-insufficient-store :store-size (store-size tensor) :max-idx lidx :tensor tensor)
				     (setf (number-of-elements tensor) sz)))))))))

;;
(defgeneric tensor-ref (tensor &rest subscripts)
  (:documentation "
  Syntax
  ======
  (tensor-ref store subscripts)

  Purpose
  =======
  Return the element:

    (rank - 1)
       __
  hd + \   stride  * sub
       /_        i      i
      i = 0

  of the store."))

(defgeneric (setf tensor-ref) (value tensor &rest subscripts))

;;
(defgeneric tensor-store-ref (tensor store-idx)
  (:documentation "
  Syntax
  ======
  (tensor-store-ref store store-idx)

  Purpose
  =======
  Return the element store-idx of the tensor store."))

(defgeneric (setf tensor-store-ref) (value tensor idx))

;;
(defgeneric print-element (tensor
			   element stream)
  (:documentation "
  Syntax
  ======
  (PRINT-ELEMENT tensor element stream)

  Purpose
  =======
  This generic function is specialized to TENSOR to
  print ELEMENT to STREAM.  Called by PRINT-TENSOR/MATRIX
  to format a tensor into the STREAM.")
  (:method (tensor element stream)
    (format stream "~a" element)))

;;
(defmacro define-tensor
  ((tensor-class element-type store-element-type store-type &rest class-decls) &key
    f+ f- finv+ fid+ f* f/ finv* fid* fconj f=
    matrix vector
    store-allocator coercer coercer-unforgiving reader value-writer value-incfer reader-writer swapper)
  ;;Error checking
  (assert (and f+ f- finv+ fid+ f* f/ finv* fid* f= store-allocator coercer coercer-unforgiving matrix vector reader value-writer value-incfer reader-writer swapper))
  ;;
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ;;Class definitions
     (defclass ,tensor-class (standard-tensor)
       ((store :type ,store-type))
       ,@class-decls)
     (defclass ,matrix (standard-matrix ,tensor-class)
       ())
     (defclass ,vector (standard-vector ,tensor-class)
       ())
     ;;Store refs
     (defmethod tensor-ref ((tensor ,tensor-class) &rest subs)
       (let-typed ((lidx (store-indexing (if (typep (car subs) '(or cons vector)) (car subs) subs) tensor) :type index-type)
		   (sto-x (store tensor) :type ,(linear-array-type store-element-type)))
	 (,reader sto-x lidx)))
     (defmethod (setf tensor-ref) (value (tensor ,tensor-class) &rest subs)
       (let-typed ((lidx (store-indexing (if (typep (car subs) '(or cons vector)) (car subs) subs) tensor) :type index-type)
		   (sto-x (store tensor) :type ,(linear-array-type store-element-type)))
	 (,value-writer (,coercer-unforgiving value) sto-x lidx)))
     (defmethod tensor-store-ref ((tensor ,tensor-class) lidx)
       (declare (type index-type lidx))
       (let-typed ((sto-x (store tensor) :type ,(linear-array-type store-element-type)))
	 (,reader sto-x lidx)))
     (defmethod (setf tensor-store-ref) (value (tensor ,tensor-class) lidx)
       (declare (type index-type lidx))
       (let-typed ((sto-x (store tensor) :type ,(linear-array-type store-element-type)))
	 (,value-writer (,coercer-unforgiving value) sto-x lidx)))
     ;;
     (let ((hst (list
		 :tensor ',tensor-class
		 :matrix ',matrix
		 :vector ',vector
		 :element-type ',element-type
		 :f+ ',f+
		 :f- ',f-
		 :finv+ ',finv+
		 :fid+ ',fid+
		 :f* ',f*
		 :f/ ',f/
		 :finv* ',finv*
		 :fid* ',fid*
		 :f= ',f=
		 :fconj ',fconj
		 :reader ',reader
		 :value-writer ',value-writer
		 :value-incfer ',value-incfer
		 :reader-writer ',reader-writer
		 :swapper ',swapper
		 :store-allocator ',store-allocator
		 :coercer ',coercer
		 :coercer-unforgiving ',coercer-unforgiving
		 :store-type ',store-element-type)))
       (setf (get-tensor-class-optimization ',tensor-class) hst
	     (get-tensor-class-optimization ',matrix) ',tensor-class
	     (get-tensor-class-optimization ',vector) ',tensor-class)
       (setf (symbol-plist ',tensor-class) hst))))

;;
(defun tensor-typep (tensor subscripts)
  "
  Syntax
  ======
  (tensor-typep tensor subscripts)

  Purpose
  =======
  Check if the given tensor is of a particular size in particular
  arguments.

  Examples
  ========
  Checking for a vector:
  > (tensor-typep ten '(*))

  Checking for a matrix with 2 columns:
  > (tensor-typep ten '(* 2))

  "
  (declare (type standard-tensor tensor))
  (let-typed ((rank (rank tensor) :type index-type)
	      (dims (dimensions tensor) :type index-store-vector))
    (very-quickly 
      (loop :for val :in subscripts
	 :for i :of-type index-type := 0 :then (1+ i)
	 :do (unless (or (eq val '*) (eq val (aref dims i)))
	       (return nil))
	 :finally (return (when (= (1+ i) rank) t))))))

(definline matrix-p (ten)
  (declare (type standard-tensor ten))
  (= (slot-value ten 'rank) 2))

(definline vector-p (ten)
  (declare (type standard-tensor ten))
  (= (slot-value ten 'rank) 1))

(definline square-p (tensor)
  (let-typed ((dims (dimensions tensor) :type index-store-vector))
    (lvec-foldr #'(lambda (a b) (if (eq a b) a nil)) dims)))

;;---------------------------------------------------------------;;
(defun sub-tensor~ (tensor subscripts &optional (preserve-rank nil))
  "
  Syntax
  ======
  (SUB-TENSOR~ TENSOR SUBSCRIPTS)

  Purpose
  =======
  Creates a new tensor data structure, sharing store with
  TENSOR but with different strides and dimensions, as defined
  in the subscript-list SUBSCRIPTS.

  Examples
  ========
  > (defvar X (make-real-tensor 10 10 10))
  X

  ;; Get (:, 0, 0)
  > (sub-tensor~ X '((* * *) (0 * 1) (0 * 1)))

  ;; Get (:, 2:5, :)
  > (sub-tensor~ X '((* * *) (2 * 5)))

  ;; Get (:, :, 0:2:10) (0:10:2 = [i : 0 <= i < 10, i % 2 = 0])
  > (sub-tensor~ X '((* * *) (* * *) (0 2 10)))

  Commentary
  ==========
  Sadly in our parentheses filled world, this function has to be necessarily
  verbose (unlike MATLAB, Python). However, this function has been designed with the
  express purpose of using it with a Lisp reader macro. The slicing semantics is
  essentially the same as MATLAB except for the zero-based indexing.
"
  (declare (type standard-tensor tensor)
	   (type list subscripts)
	   (type boolean preserve-rank))
  (labels ((prune-index-vector! (vec n)
	     (declare (type index-store-vector vec)
		      (type index-type n))
	     (if (= n (length vec)) vec
		 (very-quickly
		   (loop :for i :of-type index-type :from 0 :below n
		      :with ret :of-type index-store-vector := (allocate-index-store n)
		      :do (setf (aref ret i) (aref vec i))
		      :finally (return ret))))))
    (let-typed ((dims (dimensions tensor) :type index-store-vector)
		(stds (strides tensor) :type index-store-vector)
		(rank (rank tensor) :type index-type))
      (loop :for (start step end) :in subscripts
	 :for i :of-type index-type := 0 :then (1+ i)
	 :with ndims :of-type index-store-vector := (allocate-index-store rank)
	 :with nstds :of-type index-store-vector := (allocate-index-store rank)
	 :with nrank :of-type index-type := 0
	 :with nhd :of-type index-type := (head tensor)
	 :do (assert (< i rank) nil 'tensor-index-rank-mismatch :index-rank (1+ i) :rank rank)
	 :do (let* ((start (if (eq start '*) 0
			       (progn
				 (assert (and (typep start 'index-type) (< -1 start (aref dims i))) nil 'tensor-index-out-of-bounds :argument i :index start :dimension (aref dims i))
				 start)))
		    (step (if (eq step '*) 1
			      (progn
				(assert (and (typep step 'index-type) (< 0 step)) nil 'invalid-value :given step :expected '(< 0 step) :message "STEP cannot be <= 0.")
				step)))
		    (end (if (eq end '*) (aref dims i)
			     (progn
			       (assert (and (typep end 'index-type) (<= 0 end (aref dims i))) nil 'tensor-index-out-of-bounds :argument i :index start :dimension (aref dims i))
			       end))))
	       (declare (type index-type start step end))
	       ;;
	       (let-typed ((dim (ceiling (the index-type (- end start)) step) :type index-type))
	         (unless (and (= dim 1) (not preserve-rank))
		   (setf (aref ndims nrank) dim
			 (aref nstds nrank) (* step (aref stds i)))
		   (incf nrank))
		 (when (/= start 0)
		   (incf nhd (the index-type (* start (aref stds i)))))))
	 :finally (return
		    (if (= nrank 0) (tensor-store-ref tensor nhd)
			(make-instance (cond
					 ((or preserve-rank (> nrank 2)) (class-of tensor))
					 ((= nrank 2) (get (class-name (class-of tensor)) :matrix))
					 ((= nrank 1) (get (class-name (class-of tensor)) :vector)))
				       :head nhd
				       :dimensions (prune-index-vector! ndims nrank)
				       :strides (prune-index-vector! nstds nrank)
				       :store (store tensor) :store-size (store-size tensor)
				       :parent-tensor tensor)))))))
