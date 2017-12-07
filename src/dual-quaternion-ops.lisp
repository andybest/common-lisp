(in-package :gamebox-math)

(eval-when (:compile-toplevel :load-toplevel)
  (defun* dquat-identity! ((dquat dquat)) (:result dquat :abbrev dqid!)
    "Modify the components of DQUAT to form an identity dual quaternion."
    (with-dquat (d dquat)
      (qid! dr)
      (psetf ddw 0.0 ddx 0.0 ddy 0.0 ddz 0.0))
    dquat)

  (defun* dquat-identity () (:result dquat :abbrev dqid)
    "Create an identity dual quaternion."
    (dqid! (dquat)))

  (define-constant +dqid+ (dqid) :test #'equalp))

(defun* dquat= ((dquat-a dquat) (dquat-b dquat)) (:result boolean :abbrev dq=)
  "Check if the components of DQUAT-A are equal to the components of DQUAT-B."
  (with-dquats ((d1 dquat-a) (d2 dquat-b))
    (and (q= d1r d2r)
         (q= d1d d2d))))

(defun* dquat~ ((dquat-a dquat) (dquat-b dquat)
                &key ((tolerance single-float) +epsilon+))
    (:result boolean :abbrev dq~)
  "Check if the components of DQUAT-A are approximately equal to the components
of DQUAT-B."
  (with-dquats ((d1 dquat-a) (d2 dquat-b))
    (and (q~ d1r d2r :tolerance tolerance)
         (q~ d1d d2d :tolerance tolerance))))

(defun* dquat-copy! ((out-dquat dquat) (dquat dquat))
    (:result dquat :abbrev dqcp!)
  "Copy the components of DQUAT, storing the result in OUT-DQUAT."
  (with-dquats ((o out-dquat) (d dquat))
    (qcp! or dr)
    (qcp! od dd))
  out-dquat)

(defun* dquat-copy ((dquat dquat)) (:result dquat :abbrev dqcp)
   "Copy the components of DQUAT, storing the result in a new dual quaternion."
  (dqcp! (dqid) dquat))

(defun* dquat+! ((out-dquat dquat) (dquat-a dquat) (dquat-b dquat))
    (:result dquat :abbrev dq+!)
   "Dual quaternion addition of DQUAT-A and DQUAT-B, storing the result in
OUT-DQUAT."
  (with-dquats ((o out-dquat) (d1 dquat-a) (d2 dquat-b))
    (q+! or d1r d2r)
    (q+! od d1d d2d))
  out-dquat)

(defun* dquat+ ((dquat-a dquat) (dquat-b dquat)) (:result dquat :abbrev dq+)
  "Dual quaternion addition of DQUAT-A and DQUAT-B, storing the result as a new
dual quaternion."
  (dq+! (dqid) dquat-a dquat-b))

(defun* dquat-! ((out-dquat dquat) (dquat-a dquat) (dquat-b dquat))
    (:result dquat :abbrev dq-!)
  "Dual quaternion subtraction of DQUAT-B from DQUAT-A, storing the result in
OUT-DQUAT."
  (with-dquats ((o out-dquat) (d1 dquat-a) (d2 dquat-b))
    (q-! or d1r d2r)
    (q-! od d1d d2d))
  out-dquat)

(defun* dquat- ((dquat-a dquat) (dquat-b dquat)) (:result dquat :abbrev dq-)
  "Dual quaternion subtraction of DQUAT-B from DQUAT-A, storing the result as a
new dual quaternion."
  (dq-! (dqid) dquat-a dquat-b))

(defun* dquat*! ((out-dquat dquat) (dquat-a dquat) (dquat-b dquat))
    (:result dquat :abbrev dq*!)
  "Dual quaternion multiplication of DQUAT-A and DQUAT-B, storing the result in
OUT-DQUAT."
  (let ((dual1 (quat))
        (dual2 (quat)))
    (with-dquats ((o out-dquat) (d1 dquat-a) (d2 dquat-b))
      (q*! or d1r d2r)
      (q*! dual1 d1r d2d)
      (q*! dual2 d1d d2r)
      (q+! od dual1 dual2)))
  out-dquat)

(defun* dquat* ((dquat-a dquat) (dquat-b dquat)) (:result dquat :abbrev dq*)
  "Dual quaternion multiplication of DQUAT-A and DQUAT-B, storing the result as a
new dual quaternion."
  (dq*! (dqid) dquat-a dquat-b))

(defun* dquat-scale! ((out-dquat dquat) (dquat dquat) (scalar single-float))
    (:result dquat :abbrev dqscale!)
  "Dual quaternion scalar multiplication of DQUAT by SCALAR, storing the result
in OUT-DQUAT."
  (with-dquats ((o out-dquat) (d dquat))
    (qscale! or dr scalar)
    (qscale! od dd scalar))
  out-dquat)

(defun* dquat-scale ((dquat dquat) (scalar single-float))
    (:result dquat :abbrev dqscale)
  "Dual quaternion scalar multiplication of DQUAT by SCALAR, storing the result
as a new dual quaternion."
  (dqscale! (dqid) dquat scalar))

(defun* dquat-conjugate! ((out-dquat dquat) (dquat dquat))
    (:result dquat :abbrev dqconj!)
  "Calculate the conjugate of DQUAT, storing the result in OUT-DQUAT."
  (with-dquats ((o out-dquat) (d dquat))
    (qconj! or dr)
    (qconj! od dd))
  out-dquat)

(defun* dquat-conjugate ((dquat dquat)) (:result dquat :abbrev dqconj)
  "Calculate the conjugate of DQUAT, storing the result as a new dual
quaternion."
  (dqconj! (dqid) dquat))

(defun* dquat-conjugate-full! ((out-dquat dquat) (dquat dquat))
    (:result dquat :abbrev dqconjf!)
  "Calculate the full conjugate of DQUAT, storing the result in OUT-DQUAT."
  (with-dquats ((o out-dquat) (d dquat))
    (qconj! or dr)
    (psetf odw (- ddw) odx ddx ody ddy odz ddz))
  out-dquat)

(defun* dquat-conjugate-full ((dquat dquat)) (:result dquat :abbrev dqconjf)
  "Calculate the full conjugate of DQUAT, storing the result as a new dual
quaternion."
  (dqconjf! (dqid) dquat))

(defun* dquat-apply! ((out-dquat dquat) (dquat-a dquat) (dquat-b dquat))
    (:result dquat :abbrev dqapply!)
  "Apply the sandwich operator to DQUAT-A and DQUAT-B, storing the result
OUT-DQUAT."
  (let ((dquat-b (dqnormalize dquat-b)))
    (dq*! out-dquat
          (dq* dquat-b dquat-a)
          (dqconjf dquat-b))))

(defun* dquat-apply ((dquat-a dquat) (dquat-b dquat))
    (:result dquat :abbrev dqapply)
  "Apply the sandwich operator to DQUAT-A and DQUAT-B, storing the result as a
new dual quaternion."
  (dqapply! (dqid) dquat-a dquat-b))

(defun* dquat-magnitude-squared ((dquat dquat))
    (:result single-float :abbrev dqmagsq)
  "Compute the magnitude (also known as length or Euclidean norm) of the real
part of DQUAT. This results in a squared value, which is cheaper to compute."
  (with-dquat (d dquat)
    (qmagsq dr)))

(defun* dquat-magnitude ((dquat dquat)) (:result single-float :abbrev dqmag)
  "Compute the magnitude (also known as length or Euclidean norm) of the real
part of DQUAT."
  (sqrt (dqmagsq dquat)))

(defun* dquat-normalize! ((out-dquat dquat) (dquat dquat))
    (:result dquat :abbrev dqnormalize!)
  "Normalize a dual quaternion so its real part has a magnitude of 1.0, storing
the result in OUT-DQUAT."
  (let ((magnitude (dqmag dquat)))
    (unless (zerop magnitude)
      (dqscale! out-dquat dquat (/ magnitude))))
  out-dquat)

(defun* dquat-normalize ((dquat dquat)) (:result dquat :abbrev dqnormalize)
  "Normalize a dual quaternion so its real part has a magnitude of 1.0, storing
the result as a new dual quaternion."
  (dqnormalize! (dqid) dquat))

(defun* dquat-negate! ((out-dquat dquat) (dquat dquat))
    (:result dquat :abbrev dqneg!)
  "Negate each component of DQUAT, storing the result in OUT-DQUAT."
  (dqscale! out-dquat dquat -1.0))

(defun* dquat-negate ((dquat dquat)) (:result dquat :abbrev dqneg)
  "Negate each component of DQUAT, storing the result as a new dual quaternion."
  (dqneg! (dqid) dquat))

(defun* dquat-dot ((dquat-a dquat) (dquat-b dquat))
    (:result single-float :abbrev dqdot)
  "Compute the dot product of DQUAT-A and DQUAT-B."
  (with-dquats ((d1 dquat-a) (d2 dquat-b))
    (qdot d1r d2r)))

(defun* dquat-inverse! ((out-dquat dquat) (dquat dquat))
    (:result dquat :abbrev dqinv!)
  "Compute the multiplicative inverse of DQUAT, storing the result in OUT-DQUAT."
  (with-dquats ((o out-dquat) (d dquat))
    (qinv! or dr)
    (qscale! od (q* or (q* dd or)) -1.0))
  out-dquat)

(defun* dquat-inverse ((dquat dquat)) (:result dquat :abbrev dqinv)
  "Compute the multiplicative inverse of DQUAT, storing the result as a new dual
quaternion."
  (dqinv! (dqid) dquat))

(defun* dquat-translation-to-vec3! ((out-vec vec3) (dquat dquat))
    (:result vec3 :abbrev dqtr->v3!)
  "Decode the translation in the dual part of a dual quaternion, storing the
result in OUT-VEC."
  (let ((s (quat))
        (c (quat)))
    (with-vec3 (o out-vec)
      (with-dquat (d dquat)
        (qscale! s dd 2.0)
        (qconj! c dr)
        (with-quat (q (quat* s c))
          (setf ox qx oy qy oz qz)))))
  out-vec)

(defun* dquat-translation-to-vec3 ((dquat dquat)) (:result vec3 :abbrev dqtr->v3)
  "Decode the translation in the dual part of a dual quaternion, storing the
result as a new vector."
  (dqtr->v3! (v3zero) dquat))

(defun* dquat-translation-from-vec3! ((out-dquat dquat) (vec vec3))
    (:result dquat :abbrev v3->dqtr!)
  "Encode a translation vector into a dual quaternion, storing the result in
OUT-DQUAT."
  (with-dquat (o (dqid! out-dquat))
    (v3->q! od vec)
    (qscale! od od 0.5))
  out-dquat)

(defun* dquat-translation-from-vec3 ((vec vec3)) (:result dquat :abbrev v3->dqtr)
  "Encode a translation vector into a dual quaternion, storing the result in a
new dual quaternion."
  (v3->dqtr! (dquat) vec))

(defun* dquat-translate! ((out-dquat dquat) (dquat dquat) (vec vec3))
    (:result dquat :abbrev dqtr!)
  "Translate a quaternion in each of 3 dimensions as specified by VEC, storing
the result in OUT-DQUAT."
  (dq*! out-dquat (v3->dqtr vec) dquat))

(defun* dquat-translate ((dquat dquat) (vec vec3)) (:result dquat :abbrev dqtr)
  "Translate a quaternion in each of 3 dimensions as specified by VEC, storing
the result as a new dual quaternion."
  (dqtr! (dqid) dquat vec))

(defun* dquat-rotation-to-quat! ((out-quat quat) (dquat dquat))
    (:result quat :abbrev dqrot->q!)
  "Get the rotation of a dual quaternion, storing the result in OUT-QUAT."
  (with-dquat (d dquat)
    (qcp! out-quat dr))
  out-quat)

(defun* dquat-rotation-to-quat ((dquat dquat)) (:result quat :abbrev dqrot->q)
   "Get the rotation of a dual quaternion, storing the result as a new
quaternion."
  (dqrot->q! (quat) dquat))

(defun* dquat-rotation-from-quat! ((out-dquat dquat) (quat quat))
    (:result dquat :abbrev q->dqrot!)
  "Set the rotation of a dual quaternion from a quaternion, storing the result
in OUT-DQUAT."
  (with-dquat (o out-dquat)
    (qcp! or quat)
    (qzero! od))
  out-dquat)

(defun* dquat-rotation-from-quat ((quat quat)) (:result dquat :abbrev q->dqrot)
  "Set the rotation of a dual quaternion from a quaternion, storing the result
as a new dual quaternion."
  (q->dqrot! (dqid) quat))

(defun* dquat-rotate! ((out-dquat dquat) (dquat dquat) (vec vec3))
    (:result dquat :abbrev dqrot!)
  "Rotate a dual quaternion in each of 3 dimensions as specified by the vector of
radians VEC, storing the result in OUT-DQUAT."
  (with-dquats ((o out-dquat) (d dquat))
    (qrot! or dr vec))
  out-dquat)

(defun* dquat-rotate ((dquat dquat) (vec vec3)) (:result dquat :abbrev dqrot)
  "Rotate a dual quaternion in each of 3 dimensions as specified by the vector of
radians VEC, storing the result as a new dual quaternion."
  (dqrot! (dqid) dquat vec))

(defun* dquat-to-matrix! ((out-matrix matrix) (dquat dquat))
    (:result matrix :abbrev dq->m!)
  "Convert a dual quaternion to a matrix, storing the result in OUT-MATRIX."
  (with-matrix (o out-matrix)
    (with-dquat (d dquat)
      (with-vec3 (v (dqtr->v3 dquat))
        (q->m! o dr)
        (psetf o03 vx o13 vy o23 vz))))
  out-matrix)

(defun* dquat-to-matrix ((dquat dquat)) (:result matrix :abbrev dq->m)
  "Convert a dual quaternion to a matrix, storing the result as a new matrix."
  (dq->m! (mid) dquat))

(defun* dquat-from-matrix! ((out-dquat dquat) (matrix matrix))
    (:result dquat :abbrev m->dq!)
  "Convert a matrix to a dual quaternion, storing the result in OUT-DQUAT."
  (with-dquat (o out-dquat)
    (let ((rot (q->dqrot (m->q matrix)))
          (tr (v3->dqtr (mtr->v3 matrix))))
      (dq*! out-dquat tr rot)))
  out-dquat)

(defun* dquat-from-matrix ((matrix matrix)) (:result dquat :abbrev m->dq)
  "Convert a matrix to a dual quaternion, storing the result as a new dual
quaternion."
  (m->dq! (dqid) matrix))

(defun* dquat-to-screw-parameters ((dquat dquat))
    (:result (values single-float single-float vec3 vec3) :abbrev dq->screw)
  "Convert a dual quaternion to a set of 6 screw parameters."
  (with-dquat (d (dqnormalize dquat))
    (let* ((angle (* 2 (acos (clamp drw -1 1))))
           (dir (v3normalize (q->v3 dr)))
           (tr (dqtr->v3 dquat))
           (pitch (v3dot tr dir))
           (moment (v3scale
                    (v3+ (v3cross tr dir)
                         (v3scale (v3- (v3scale tr (v3dot dir dir))
                                       (v3scale dir pitch))
                                  (/ (tan (/ angle 2)))))
                    0.5)))
      (values angle pitch dir moment))))

(defun* dquat-from-screw-parameters! ((out-dquat dquat) (angle single-float)
                                      (pitch single-float) (direction vec3)
                                      (moment vec3))
    (:result dquat :abbrev screw->dq!)
  "Convert a set of 6 screw parameters to a dual quaternion, storing the result
in OUT-DQUAT."
  (let* ((half-angle (* angle 0.5))
         (c (cos half-angle))
         (s (sin half-angle)))
    (with-vec3s ((r (v3scale direction s))
                 (d (v3+ (v3scale moment s)
                         (v3scale direction (* pitch c 0.5)))))
      (setf (dq-real out-dquat) (quat c rx ry rz)
            (dq-dual out-dquat) (quat (- (* pitch s 0.5)) dx dy dz))))
  out-dquat)

(defun* dquat-from-screw-parameters ((angle single-float) (pitch single-float)
                                     (direction vec3) (moment vec3))
    (:result dquat :abbrev screw->dq)
  "Convert a set of 6 screw parameters to a dual quaternion, storing the result
as a new dual quaternion."
  (screw->dq! (dqid) angle pitch direction moment))

(defun* dquat-sclerp! ((out-dquat dquat) (dquat-a dquat) (dquat-b dquat)
                       (factor single-float))
    (:result dquat :abbrev dqsclerp!)
  "Perform a screw spherical linear interpolation between DQUAT-A and DQUAT-B by
the interpolation factor FACTOR, storing the result in OUT-DQUAT."
  (let ((diff (dq* (dqinv dquat-a) dquat-b)))
    (multiple-value-bind (angle pitch direction moment) (dq->screw diff)
      (dq*! out-dquat dquat-a (screw->dq (* angle factor)
                                         (* pitch factor)
                                         direction
                                         moment))))
  out-dquat)

(defun* dquat-sclerp ((dquat-a dquat) (dquat-b dquat) (factor single-float))
    (:result dquat :abbrev dqsclerp)
  "Perform a screw spherical linear interpolation between DQUAT-A and DQUAT-B by
the interpolation factor FACTOR, storing the result as a new dual
quaternion."
  (dqsclerp! (dqid) dquat-a dquat-b factor))

(defun* dquat-nlerp! ((out-dquat dquat) (dquat-a dquat) (dquat-b dquat)
                      (factor single-float))
    (:result dquat :abbrev dqnlerp!)
  "Perform a normalized linear interpolation between DQUAT-A and DQUAT-B by the
interpolation factor FACTOR, storing the result in OUT-DQUAT."
  (dq+! out-dquat dquat-a (dqscale (dq- dquat-b dquat-a) factor)))

(defun* dquat-nlerp ((dquat-a dquat) (dquat-b dquat) (factor single-float))
    (:result dquat :abbrev dqnlerp)
  "Perform a normalized linear interpolation between DQUAT-A and DQUAT-B by the
interpolation factor FACTOR, storing the result as a new dual quaternion."
  (dqnlerp! (dqid) dquat-a dquat-b factor))
