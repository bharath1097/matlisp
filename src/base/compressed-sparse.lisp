(in-package #:matlisp)

;;
(defclass compressed-sparse-matrix (sparse-tensor)
  ((transpose? :initform nil :initarg :transpose? :reader transpose? :type boolean
	       :documentation "If NIL the matrix is in CSC, else if T, then matrix is CSR.")
   (neighbour-start :initarg :neighbour-start :reader neighbour-start :type index-store-vector
		    :documentation "Start index for ids and store.")
   (neighbour-id :initarg :neighbour-id :reader neighbour-id :type index-store-vector
		 :documentation "Row id.")))

(defun compressed-sparse-indexing (subs tensor)
  (declare (type compressed-sparse-matrix tensor)
	   (type (or index-store-vector cons) subs))
  (let-typed ((row 0 :type index-type)
	      (col 0 :type index-type))
     (etypecase subs
       (cons
	(assert (null (cddr subs)) nil 'tensor-index-rank-mismatch)
	(setf row (the index-type (car subs))
	      col (the index-type (cadr subs))))
       (index-store-vector
	(assert (= (length subs) 2) nil 'tensor-index-rank-mismatch)
	(setf row (the index-type (aref subs 0))
	      col (the index-type (aref subs 1)))))
     (when (transpose? tensor)
       (rotatef row col))
    (let*-typed ((nst (neighbour-start tensor) :type index-store-vector)
		 (nid (neighbour-id tensor) :type index-store-vector)
		 (lb (aref nst col) :type index-type)
		 (ub (aref nst (1+ col)) :type index-type))
       (declare (type index-type row col))
       (if (or (= lb ub) (< row (aref nid lb)) (> row (aref nid (1- ub)))) -1
	   (values
	    (very-quickly
	      (loop :with j := (ash (+ lb ub) -1)
		 :repeat 64
		 :do (progn
		       #+nil(format t "~a, ~a, ~a~%" lb j ub)
		       (cond
			 ((= (aref nid j) row) (return j))
			 ((>= lb (1- ub)) (return -1))
			 (t
			  (if (< row (aref nid j))
			      (setf ub j)
			      (setf lb (1+ j)))
			  (setf j (ash (+ lb ub) -1)))))))
	    row col)))))

;;Templates
(deft/method t/store-allocator (cl compressed-sparse-matrix) (size &optional nz)
  (let ((sto-type (store-element-type cl)))
    `(destructuring-bind (nr nc) ,size
       (let ((nz (or ,nz (min (ceiling (* nr nc *default-sparsity*)) *max-sparse-size*))))
	 (list
	  (allocate-index-store nz)
	  (make-array (t/compute-store-size ,cl nz) :element-type ',sto-type :initial-element ,(if (subtypep sto-type 'number) `(t/fid+ ,sto-type) nil)))))))

(deft/method t/compute-store-size (sym compressed-sparse-matrix) (size)
  size)
;;
(deft/method t/store-type (sym compressed-sparse-matrix) (&optional (size '*))
  `(simple-array ,(store-element-type sym) (,size)))

(deft/method t/store-ref (sym compressed-sparse-matrix) (store &rest idx)
   (assert (null (cdr idx)) nil "given more than one index for compressed-store")
  `(aref (the ,(store-type sym) ,store) (the index-type ,(car idx))))

(deft/method t/store-set (sym compressed-sparse-matrix) (value store &rest idx)
   (assert (null (cdr idx)) nil "given more than one index for compressed store")
  `(setf (aref (the ,(store-type sym) ,store) (the index-type ,(car idx))) (the ,(field-type sym) ,value)))

(deft/method t/store-size (sym compressed-sparse-matrix) (ele)
  `(length ,ele))

(deft/method t/store-element-type (sym compressed-sparse-matrix) ()
  (macroexpand `(t/field-type ,sym)))
;;
(defmethod ref ((tensor compressed-sparse-matrix) &rest subscripts)
  (let ((clname (class-name (class-of tensor))))
    (assert (member clname *tensor-type-leaves*) nil 'tensor-abstract-class :tensor-class clname)
    (compile-and-eval
     `(defmethod ref ((tensor ,clname) &rest subscripts)
	(let ((idx (compressed-sparse-indexing (if (numberp (car subscripts)) subscripts (car subscripts)) tensor)))
	  (if (< idx 0)
	      (t/sparse-fill ,clname)
	      (t/store-ref ,clname (store tensor) idx)))))
    (apply #'ref (cons tensor subscripts))))

;; (defmethod (setf ref) (value (tensor compressed-sparse-matrix) &rest subscripts)
;;   (let ((clname (class-name (class-of tensor))))
;;     (assert (member clname *tensor-type-leaves*) nil 'tensor-abstract-class :tensor-class clname)
;;     (compile-and-eval
;;      `(defmethod (setf ref) (value (tensor ,clname) &rest subscripts)
;; 	(multiple-value-bind (idx row col) (compressed-sparse-indexing (if (numberp (car subscripts)) subscripts (car subscripts)) tensor)
;; 	  (if (< idx 0)
;; 	      (let ((ns (neighbour-start tensor))
;; 		    (ni (neighbour-id tensor))
;; 		    (vi (store tensor)))
;; 		(unless (> (store-size tensor) (aref ns (1- (length ns))))
;; 		  (let ((sto-new (make-a-bigger-array)))
;; 		    (move-things forward)
;; 		    copy-back-to-vi..))
;; 		(let ((row-data (merge 'list
;; 				       (cons row (t/coerce ,(field-type clname) value))
;; 				       (loop :for j :from (aref ns col) :to (aref ns (1+ col))
;; 					  :collect (cons (aref ni j) (aref vi j)))
;; 				       #'< :key #'car)))
		  
		
;; 		)
;; 	      (t/store-set ,clname (t/coerce ,(field-type clname) value) (store tensor) idx)))))
;;     (setf (ref tensor (if (numberp (car subscripts)) subscripts (car subscripts))) value)))
;;
