;;;; Implements dual quaternion math.
;;;; This is probably not completely correct, and definitely not tested enough to be exported - use with caution.

(in-package :gamebox-math)

(defun* dquat-identity! ((dquat dquat)) (:result dquat :abbrev dqid!)
  (with-dquat (d dquat)
    (quat-identity! dr)
    (psetf ddw 0.0 ddx 0.0 ddy 0.0 ddz 0.0))
  dquat)

(defun* dquat-identity () (:result dquat :abbrev dqid)
  (dquat-identity! (dquat)))

(defun* dquat-copy! ((out-dquat dquat) (dquat dquat)) (:result dquat :abbrev dqcp!)
  (with-dquats ((o out-dquat) (d dquat))
    (quat-copy! or dr)
    (quat-copy! od dd))
  out-dquat)

(defun* dquat-copy ((dquat dquat)) (:result dquat :abbrev dqcp)
  (dquat-copy! (dquat) dquat))

(defun* dquat+! ((out-dquat dquat) (dquat1 dquat) (dquat2 dquat)) (:result dquat :abbrev dq+!)
  (with-dquats ((o out-dquat) (d1 dquat1) (d2 dquat2))
    (quat+! or d1r d2r)
    (quat+! od d1d d2d))
  out-dquat)

(defun* dquat+ ((dquat1 dquat) (dquat2 dquat)) (:result dquat :abbrev dq+)
  (dquat+! (dquat) dquat1 dquat2))

(defun* dquat*! ((out-dquat dquat) (dquat1 dquat) (dquat2 dquat)) (:result dquat :abbrev dq*!)
  (let ((dual1 (quat))
        (dual2 (quat)))
    (declare (dynamic-extent dual1 dual2))
    (with-dquats ((o out-dquat) (d1 dquat1) (d2 dquat2))
      (quat*! or d1r d2r)
      (quat*! dual1 d1r d2d)
      (quat*! dual2 d1d d2r)
      (quat+! od dual1 dual2)))
  out-dquat)

(defun* dquat* ((dquat1 dquat) (dquat2 dquat)) (:result dquat :abbrev dq*)
  (dquat*! (dquat) dquat1 dquat2))

(defun* dquat-scale! ((out-dquat dquat) (dquat dquat) (scalar single-float)) (:result dquat :abbrev dqscale!)
  (with-dquats ((o out-dquat) (d dquat))
    (quat-scale! or dr scalar)
    (quat-scale! od dd scalar))
  out-dquat)

(defun* dquat-scale ((dquat dquat) (scalar single-float)) (:result dquat :abbrev dqscale)
  (dquat-scale! (dquat) dquat scalar))

(defun* dquat-conjugate! ((out-dquat dquat) (dquat dquat)) (:result dquat :abbrev dqconj!)
  (with-dquats ((o out-dquat) (d dquat))
    (quat-conjugate! or dr)
    (quat-conjugate! od dd))
  out-dquat)

(defun* dquat-conjugate ((dquat dquat)) (:result dquat :abbrev dqconj)
  (dquat-conjugate! (dquat) dquat))

(defun* dquat-normalize! ((out-dquat dquat) (dquat dquat)) (:result dquat :abbrev dqnormalize!)
  (with-dquats ((o out-dquat) (d dquat))
    (quat-normalize! or dr)
    (quat-normalize! od dd))
  out-dquat)

(defun* dquat-normalize ((dquat dquat)) (:result dquat :abbrev dqnormalize)
  (dquat-normalize! (dquat) dquat))

(defun* dquat-negate! ((out-dquat dquat) (dquat dquat)) (:result dquat :abbrev dqneg!)
  (dquat-scale! out-dquat dquat -1.0))

(defun* dquat-negate ((dquat dquat)) (:result dquat :abbrev dqneg)
  (dquat-negate! (dquat) dquat))

(defun* dquat-dot ((dquat1 dquat) (dquat2 dquat)) (:result single-float :abbrev dqdot)
  (with-dquats ((d1 dquat1) (d2 dquat2))
    (quat-dot d1r d2r)))

(defun* dquat-copy-rotation! ((out-quat quat) (dquat dquat)) (:result quat :abbrev dqcprot!)
  (with-dquat (d dquat)
    (quat-copy! out-quat dr))
  out-quat)

(defun* dquat-copy-rotation ((dquat dquat)) (:result quat :abbrev dqcprot)
  (dquat-copy-rotation! (quat) dquat))

(defun* dquat-copy-translation! ((out-vec vec) (dquat dquat)) (:result vec :abbrev dqcptr!)
  (let ((s (quat))
        (c (quat))
        (s*c (quat)))
    (declare (dynamic-extent s c s*c))
    (with-vector (o out-vec)
      (with-dquat (d dquat)
        (quat-scale! s dd 2.0)
        (quat-conjugate! c dr)
        (with-quat (q (quat*! s*c s c))
          (setf ox qx oy qy oz qz)))))
  out-vec)

(defun* dquat-copy-translation ((dquat dquat)) (:result vec :abbrev dqcptr)
  (dquat-copy-translation! (vec) dquat))

(defun* dquat-to-matrix! ((out-matrix matrix) (dquat dquat)) (:result matrix :abbrev dq->m!)
  (let ((translation (vec)))
    (declare (dynamic-extent translation))
    (with-matrix (o out-matrix)
      (with-dquat (d dquat)
        (with-vector (v (dquat-copy-translation! translation dquat))
          (quat-to-matrix! o dr)
          (psetf o03 vx o13 vy o23 vz)))))
  out-matrix)

(defun* dquat-to-matrix ((dquat dquat)) (:result matrix :abbrev dq->m)
  (dquat-to-matrix! (matrix-identity) dquat))

(defun* dquat-rotate! ((out-dquat dquat) (angle float) (axis vec)) (:result dquat :abbrev dqrot!)
  (with-dquat (o (dquat-identity! out-dquat))
    (quat-rotate! or angle axis))
  out-dquat)

(defun* dquat-rotate ((angle float) (axis vec)) (:result dquat :abbrev dqrot)
  (dquat-rotate! (dquat) angle axis))

(defun* make-translation-dquat! ((out-dquat dquat) (vec vec)) (:result dquat :abbrev mktrdq!)
  (with-dquat (o (dquat-identity! out-dquat))
    (quat-from-vec! od vec)
    (quat-scale! od od 0.5))
  out-dquat)

(defun* make-translation-dquat ((vec vec)) (:result dquat :abbrev mktrdq)
  (make-translation-dquat! (dquat) vec))
