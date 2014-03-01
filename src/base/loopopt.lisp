(in-package #:matlisp)

;;diagonal-copy
;; i, j; must be indices in some tensor.
;; (with-tensors ((a .. :type real-tensor)
;; 	       (b .. :type real-tensor))
;;   :do (forall (i j) :st (= i j) :do (setf (ref a i j) (ref b i j))))

(defparameter *code*
  `((forall (i j) :st (= i j) :do (setf (ref a i j) (ref b i j)))
    (forall (i j) :st (= i j) :do (setf (ref a i j) (ref b i j)))
    (forall (i j) :st (< i j) :do (setf (ref a i j) (ref b i j)))
    ;;Einstein
    (forall (i j) :do (setf (ref a i j) (sum (k) (* (ref b i k) (ref c k i)))))
    ;;+einstein
    ((forall a b c) (i j) :do (setf (ref a i j) (+ (* alpha (sum (k) (* (ref b i k) (ref c k i)))) (* beta (ref a i j)))))
    ;;mod-loop
    (forall (&rest idx) :order :col-major :st (< idx (dimensions a)) :do (setf (ref a idx) (ref b idx)))))

;; `(let (,@stores
;;        ,@dimensions
;;        ,@strides
;;        ,@heads)
   

(defparameter *expr*
  (mapcar #'(lambda (x) (find-tag x :do)) *code*))

(defun prune-set (list &optional (test #'cl:eq))
  (let ((vec (make-array (length list) :initial-contents list))
	(ret nil))
    (loop :for i :from 0 :below (length vec)
       :do (if (loop :for j :from (1+ i) :below (length vec)
		  :do (when (funcall test (aref vec i) (aref vec j))
			(return nil))
		  :finally (return t))
	       (push (aref vec i) ret)))
    ret))

(defun mapadd (func list)
  (let ((ret nil))
    (loop :for ele :in list
       :do (setf ret (setadd ret (funcall func ele))))
    ret))

(defun getinfo (otable sym &optional phi)
  (let ((lst (gethash sym otable)))
    (if phi
	(getf lst phi)
	lst)))

(defun (setf getinfo) (value otable sym &optional phi)
  (if phi
      (let ((lst (gethash sym otable)))
	(setf (getf lst phi) value)
	(setf (gethash sym otable) lst))
      (setf (gethash sym otable) value))
  t)

(defun loop-opt (code)
  ;; (if (not (eql (car code) 'forall)) nil
  (let ((idxs (cadr code))
	(cstr (getf (cddr code) :st))
	(doco (getf (cddr code) :do))
	(order (getf (cddr code) :order :col-major)))
    (if (eql (car idxs) '&rest)
	(let* ((midx (cadr idxs))
	       ;;TODO: Add check for standard-tensor
	       (refs (remove-if-not #'(lambda (x) (and (eql (third x) midx) (null (cdddr x)))) (getcons doco 'ref)))

(defun loopopt (body)
  (destructuring-bind (idx &key st do)
      (cond
	((null st)
	 (generate-loops idx do))
	(t
	 (format t "Not implemented.")))))

(defparameter *table* (make-hash-table))


(defun parse-tensor-ref (otable expr &key multiindex?)
  (let ((ten (second expr))
	(idxs (cddr expr)))
    ;;
    (setf (getinfo ten 'type) 'tensor)
    (loop :for idx :in idxs
       :for i := 0 :then (1+ i)
       :do (progn
	     (setf (getinfo otable idx 'type) 'index)))))
	    
    
		     
(defun optimize-tensor-refs (idx expr &key multiindex?)
  (let* ((refs (remove-if-not #'(lambda (x) (member idx (cddr x))) (getcons expr 'ref))))
    
	 
	 
	
