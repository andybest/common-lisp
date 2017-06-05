(in-package :gamebox-math)

(eval-when (:compile-toplevel :load-toplevel)
  (defun* dquat-identity! ((dquat dquat)) (:result dquat :abbrev dqid!)
    (with-dquat (d dquat)
      (quat-identity! dr)
      (psetf ddw 0.0 ddx 0.0 ddy 0.0 ddz 0.0))
    dquat)

  (defun* dquat-identity () (:result dquat :abbrev dqid)
    (dquat-identity! (dquat)))

  (define-constant +identity-dual-quaternion+ (dquat-identity) :test #'equalp)
  (define-constant +dqid+ (dquat-identity) :test #'equalp))

(defun* dquat= ((dquat1 dquat) (dquat2 dquat)) (:result boolean :abbrev dq=)
  (with-dquats ((d1 dquat1) (d2 dquat2))
    (and (quat= d1r d2r)
         (quat= d1d d2d))))

(defun* dquat~ ((dquat1 dquat) (dquat2 dquat) &key ((tolerance single-float) +epsilon+)) (:result boolean :abbrev dq~)
  (with-dquats ((d1 dquat1) (d2 dquat2))
    (and (quat~ d1r d2r :tolerance tolerance)
         (quat~ d1d d2d :tolerance tolerance))))

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

(defun* dquat-! ((out-dquat dquat) (dquat1 dquat) (dquat2 dquat)) (:result dquat :abbrev dq-!)
  (with-dquats ((o out-dquat) (d1 dquat1) (d2 dquat2))
    (quat-! or d1r d2r)
    (quat-! od d1d d2d))
  out-dquat)

(defun* dquat- ((dquat1 dquat) (dquat2 dquat)) (:result dquat :abbrev dq-)
  (dquat-! (dquat) dquat1 dquat2))

(defun* dquat*! ((out-dquat dquat) (dquat1 dquat) (dquat2 dquat)) (:result dquat :abbrev dq*!)
  (let ((dual1 (quat))
        (dual2 (quat)))
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

(defun* dquat-conjugate-translation! ((out-dquat dquat) (dquat dquat)) (:result dquat :abbrev dqconjtr!)
  (with-dquats ((o out-dquat) (d dquat))
    (quat-conjugate! or dr)
    (psetf odw (- ddw) odx ddx ody ddy odz ddz))
  out-dquat)

(defun* dquat-conjugate-translation ((dquat dquat)) (:result dquat :abbrev dqconjtr)
  (dquat-conjugate-translation! (dquat) dquat))

(defun* dquat-sandwich! ((out-dquat dquat) (dquat1 dquat) (dquat2 dquat)) (:result dquat :abbrev dqsandwich!)
  (let ((dquat2 (dqnormalize dquat2)))
    (dquat*! out-dquat
             (dquat* dquat2 dquat1)
             (dquat-conjugate-translation dquat2))))

(defun* dquat-sandwich ((dquat1 dquat) (dquat2 dquat)) (:result dquat :abbrev dqsandwich)
  (dquat-sandwich! (dquat) dquat1 dquat2))

(defun* dquat-magnitude-squared ((dquat dquat)) (:result single-float :abbrev dqmagsq)
  (with-dquat (d dquat)
    (quat-magnitude-squared dr)))

(defun* dquat-magnitude ((dquat dquat)) (:result single-float :abbrev dqmag)
  (sqrt (dquat-magnitude-squared dquat)))

(defun* dquat-normalize! ((out-dquat dquat) (dquat dquat)) (:result dquat :abbrev dqnormalize!)
  (let ((magnitude (dquat-magnitude dquat)))
    (unless (zerop magnitude)
      (dquat-scale! out-dquat dquat (/ magnitude))))
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

(defun* dquat-inverse! ((out-dquat dquat) (dquat dquat)) (:result dquat :abbrev dqinv!)
  (with-dquats ((o out-dquat) (d dquat))
    (quat-inverse! or dr)
    (quat-scale! od (quat* or (quat* dd or)) -1.0))
  out-dquat)

(defun* dquat-inverse ((dquat dquat)) (:result dquat :abbrev dqinv)
  (dquat-inverse! (dquat) dquat))

(defun* dquat-translation-to-vec! ((out-vec vec) (dquat dquat)) (:result vec :abbrev dqtr->v!)
  (let ((s (quat))
        (c (quat)))
    (with-vector (o out-vec)
      (with-dquat (d dquat)
        (quat-scale! s dd 2.0)
        (quat-conjugate! c dr)
        (with-quat (q (quat* s c))
          (setf ox qx oy qy oz qz)))))
  out-vec)

(defun* dquat-translation-to-vec ((dquat dquat)) (:result vec :abbrev dqtr->v)
  (dquat-translation-to-vec! (vec) dquat))

(defun* dquat-translation-from-vec! ((out-dquat dquat) (vec vec)) (:result dquat :abbrev v->dqtr!)
  (with-dquat (o (dquat-identity! out-dquat))
    (quat-from-vec! od vec)
    (quat-scale! od od 0.5))
  out-dquat)

(defun* dquat-translation-from-vec ((vec vec)) (:result dquat :abbrev v->dqtr)
  (dquat-translation-from-vec! (dquat) vec))

(defun* dquat-translate! ((out-dquat dquat) (dquat dquat) (vec vec)) (:result dquat :abbrev dqtr!)
  (dquat*! out-dquat (dquat-translation-from-vec vec) dquat))

(defun* dquat-translate ((dquat dquat) (vec vec)) (:result dquat :abbrev dqtr)
  (dquat-translate! (dquat-identity) dquat vec))

(defun* dquat-rotation-to-quat! ((out-quat quat) (dquat dquat)) (:result quat :abbrev dqrot->q!)
  (with-dquat (d dquat)
    (quat-copy! out-quat dr))
  out-quat)

(defun* dquat-rotation-to-quat ((dquat dquat)) (:result quat :abbrev dqrot->q)
  (dquat-rotation-to-quat! (quat) dquat))

(defun* dquat-rotate! ((out-dquat dquat) (dquat dquat) (vec vec)) (:result dquat :abbrev dqrot!)
  (with-dquats ((o out-dquat) (d dquat))
    (quat-rotate! or dr vec))
  out-dquat)

(defun* dquat-rotate ((dquat dquat) (vec vec)) (:result dquat :abbrev dqrot)
  (dquat-rotate! (dquat) dquat vec))

(defun* dquat-to-matrix! ((out-matrix matrix) (dquat dquat)) (:result matrix :abbrev dq->m!)
  (with-matrix (o out-matrix)
    (with-dquat (d dquat)
      (with-vector (v (dquat-translation-to-vec dquat))
        (quat-to-matrix! o dr)
        (psetf o03 vx o13 vy o23 vz))))
  out-matrix)

(defun* dquat-to-matrix ((dquat dquat)) (:result matrix :abbrev dq->m)
  (dquat-to-matrix! (matrix-identity) dquat))

(defun* dquat-to-screw-parameters ((dquat dquat)) (:result (values single-float single-float vec vec) :abbrev dq->screw)
  (with-dquat (d (dquat-normalize dquat))
    (let* ((angle (* 2 (acos (clamp drw -1 1))))
           (dir (vec-normalize (quat-to-vec dr)))
           (tr (dquat-translation-to-vec dquat))
           (pitch (vec-dot tr dir))
           (moment (vec-scale
                    (vec+ (vec-cross tr dir)
                          (vec-scale (vec- (vec-scale tr (vec-dot dir dir))
                                           (vec-scale dir pitch))
                                     (/ (tan (/ angle 2)))))
                    0.5)))
      (values angle pitch dir moment))))

(defun* dquat-from-screw-parameters! ((out-dquat dquat) (angle single-float) (pitch single-float) (direction vec)
                                      (moment vec))
    (:result dquat :abbrev screw->dq!)
  (let* ((half-angle (* angle 0.5))
         (c (cos half-angle))
         (s (sin half-angle)))
    (with-vectors ((r (vscale direction s))
                   (d (v+ (vscale moment s) (vscale direction (* pitch c 0.5)))))
      (setf (dq-real out-dquat) (quat c rx ry rz)
            (dq-dual out-dquat) (quat (- (* pitch s 0.5)) dx dy dz))))
  out-dquat)

(defun* dquat-from-screw-parameters ((angle single-float) (pitch single-float) (direction vec) (moment vec))
    (:result dquat :abbrev screw->dq)
  (dquat-from-screw-parameters! (dquat) angle pitch direction moment))

(defun* dquat-sclerp! ((out-dquat dquat) (dquat1 dquat) (dquat2 dquat) (coeff single-float))
    (:result dquat :abbrev dqsclerp!)
  (let ((diff (dquat* (dquat-inverse dquat1) dquat2)))
    (multiple-value-bind (angle pitch direction moment) (dq->screw diff)
      (dquat*! out-dquat dquat1 (screw->dq (* angle coeff) (* pitch coeff) direction moment))))
  out-dquat)

(defun* dquat-sclerp ((dquat1 dquat) (dquat2 dquat) (coeff single-float)) (:result dquat :abbrev dqsclerp)
  (dquat-sclerp! (dquat) dquat1 dquat2 coeff))

(defun* dquat-nlerp! ((out-dquat dquat) (dquat1 dquat) (dquat2 dquat) (coeff single-float))
    (:result dquat :abbrev dqnlerp!)
  (dquat+! out-dquat dquat1 (dquat-scale (dquat- dquat2 dquat1) coeff)))

(defun* dquat-nlerp ((dquat1 dquat) (dquat2 dquat) (coeff single-float)) (:result dquat :abbrev dqnlerp)
  (dquat-nlerp! (dquat) dquat1 dquat2 coeff))
