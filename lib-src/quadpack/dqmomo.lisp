;;; Compiled by f2cl version 2.0 beta on 2001/02/23 at 10:09:24
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type 'simple-array)
;;;           (:array-slicing t))


(use-package :f2cl)

(defun dqmomo (alfa beta ri rj rg rh integr)
  (declare (type integer4 integr)
           (type (array double-float (*)) rh rg rj ri)
           (type double-float beta alfa))
  (prog ((i 0) (im1 0) (rbet 0.0d0) (ralf 0.0d0) (betp2 0.0d0) (betp1 0.0d0)
         (anm1 0.0d0) (an 0.0d0) (alfp2 0.0d0) (alfp1 0.0d0))
    (declare (type double-float alfp1 alfp2 an anm1 betp1 betp2 ralf rbet)
             (type integer4 im1 i))
    (setf alfp1 (+ alfa 1.0d0))
    (setf betp1 (+ beta 1.0d0))
    (setf alfp2 (+ alfa 2.0d0))
    (setf betp2 (+ beta 2.0d0))
    (setf ralf (expt 2.0d0 alfp1))
    (setf rbet (expt 2.0d0 betp1))
    (fset (fref ri (1) ((1 25))) (/ ralf alfp1))
    (fset (fref rj (1) ((1 25))) (/ rbet betp1))
    (fset (fref ri (2) ((1 25))) (/ (* (fref ri (1) ((1 25))) alfa) alfp2))
    (fset (fref rj (2) ((1 25))) (/ (* (fref rj (1) ((1 25))) beta) betp2))
    (setf an 2.0d0)
    (setf anm1 1.0d0)
    (fdo (i 3 (+ i 1))
         ((> i 25) nil)
         (tagbody
           (fset (fref ri (i) ((1 25)))
                 (/
                  (- (+ ralf (* an (- an alfp2) (fref ri ((- i 1)) ((1 25))))))
                  (* anm1 (+ an alfp1))))
           (fset (fref rj (i) ((1 25)))
                 (/
                  (- (+ rbet (* an (- an betp2) (fref rj ((- i 1)) ((1 25))))))
                  (* anm1 (+ an betp1))))
           (setf anm1 an)
           (setf an (+ an 1.0d0))
          label20))
    (if (= integr 1) (go label70))
    (if (= integr 3) (go label40))
    (fset (fref rg (1) ((1 25))) (/ (- (fref ri (1) ((1 25)))) alfp1))
    (fset (fref rg (2) ((1 25)))
          (- (/ (- (+ ralf ralf)) (* alfp2 alfp2)) (fref rg (1) ((1 25)))))
    (setf an 2.0d0)
    (setf anm1 1.0d0)
    (setf im1 2)
    (fdo (i 3 (+ i 1))
         ((> i 25) nil)
         (tagbody
           (fset (fref rg (i) ((1 25)))
                 (/
                  (-
                   (+ (* an (- an alfp2) (fref rg (im1) ((1 25))))
                      (* -1 an (fref ri (im1) ((1 25))))
                      (* anm1 (fref ri (i) ((1 25))))))
                  (* anm1 (+ an alfp1))))
           (setf anm1 an)
           (setf an (+ an 1.0d0))
           (setf im1 i)
          label30))
    (if (= integr 2) (go label70))
   label40
    (fset (fref rh (1) ((1 25))) (/ (- (fref rj (1) ((1 25)))) betp1))
    (fset (fref rh (2) ((1 25)))
          (- (/ (- (+ rbet rbet)) (* betp2 betp2)) (fref rh (1) ((1 25)))))
    (setf an 2.0d0)
    (setf anm1 1.0d0)
    (setf im1 2)
    (fdo (i 3 (+ i 1))
         ((> i 25) nil)
         (tagbody
           (fset (fref rh (i) ((1 25)))
                 (/
                  (-
                   (+ (* an (- an betp2) (fref rh (im1) ((1 25))))
                      (* -1 an (fref rj (im1) ((1 25))))
                      (* anm1 (fref rj (i) ((1 25))))))
                  (* anm1 (+ an betp1))))
           (setf anm1 an)
           (setf an (+ an 1.0d0))
           (setf im1 i)
          label50))
    (fdo (i 2 (+ i 2))
         ((> i 25) nil)
         (tagbody
           (fset (fref rh (i) ((1 25))) (- (fref rh (i) ((1 25)))))
          label60))
   label70
    (fdo (i 2 (+ i 2))
         ((> i 25) nil)
         (tagbody
           (fset (fref rj (i) ((1 25))) (- (fref rj (i) ((1 25)))))
          label80))
   label90
    (go end_label)
   end_label
    (return (values alfa beta ri rj rg rh integr))))

