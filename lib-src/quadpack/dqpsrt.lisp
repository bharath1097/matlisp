;;; Compiled by f2cl version 2.0 beta on 2001/02/23 at 10:09:09
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type 'simple-array)
;;;           (:array-slicing t))

(in-package "QUADPACK")
(use-package :f2cl)

(defun dqpsrt (limit last maxerr ermax elist iord nrmax)
  (declare (type (array integer4 (*)) iord)
           (type (array double-float (*)) elist)
           (type double-float ermax)
           (type integer4 nrmax maxerr last limit))
  (prog ((i 0) (ibeg 0) (ido 0) (isucc 0) (j 0) (jbnd 0) (jupbn 0) (k 0)
         (errmin 0.0d0) (errmax 0.0d0))
    (declare (type double-float errmax errmin)
             (type integer4 k jupbn jbnd j isucc ido ibeg i))
    (if (> last 2) (go label10))
    (fset (fref iord (1) ((1 last))) 1)
    (fset (fref iord (2) ((1 last))) 2)
    (go label90)
   label10
    (setf errmax (fref elist (maxerr) ((1 last))))
    (if (= nrmax 1) (go label30))
    (setf ido (- nrmax 1))
    (fdo (i 1 (+ i 1))
         ((> i ido) nil)
         (tagbody
           (setf isucc (fref iord ((- nrmax 1)) ((1 last))))
           (if (<= errmax (fref elist (isucc) ((1 last)))) (go label30))
           (fset (fref iord (nrmax) ((1 last))) isucc)
           (setf nrmax (- nrmax 1))
          label20))
   label30
    (setf jupbn last)
    (if (> last (+ (truncate limit 2) 2)) (setf jupbn (- (+ limit 3) last)))
    (setf errmin (fref elist (last) ((1 last))))
    (setf jbnd (- jupbn 1))
    (setf ibeg (+ nrmax 1))
    (if (> ibeg jbnd) (go label50))
    (fdo (i ibeg (+ i 1))
         ((> i jbnd) nil)
         (tagbody
           (setf isucc (fref iord (i) ((1 last))))
           (if (>= errmax (fref elist (isucc) ((1 last)))) (go label60))
           (fset (fref iord ((- i 1)) ((1 last))) isucc)
          label40))
   label50
    (fset (fref iord (jbnd) ((1 last))) maxerr)
    (fset (fref iord (jupbn) ((1 last))) last)
    (go label90)
   label60
    (fset (fref iord ((- i 1)) ((1 last))) maxerr)
    (setf k jbnd)
    (fdo (j i (+ j 1))
         ((> j jbnd) nil)
         (tagbody
           (setf isucc (fref iord (k) ((1 last))))
           (if (< errmin (fref elist (isucc) ((1 last)))) (go label80))
           (fset (fref iord ((+ k 1)) ((1 last))) isucc)
           (setf k (- k 1))
          label70))
    (fset (fref iord (i) ((1 last))) last)
    (go label90)
   label80
    (fset (fref iord ((+ k 1)) ((1 last))) last)
   label90
    (setf maxerr (fref iord (nrmax) ((1 last))))
    (setf ermax (fref elist (maxerr) ((1 last))))
    (go end_label)
   end_label
    (return (values limit last maxerr ermax elist iord nrmax))))

