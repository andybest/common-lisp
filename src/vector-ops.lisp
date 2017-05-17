(in-package :gamebox-math)

(defun* vec-test () (:result vec :abbrev vtest)
  (with-vector (v (vec))
    (psetf vx 1.0 vy 2.0 vz 3.0)
    v))

(defun* vec-copy! ((out-vec vec) (vec vec)) (:result vec :inline t :abbrev vcp!)
  (with-vectors ((o out-vec) (v vec))
    (psetf ox vx oy vy oz vz))
  out-vec)

(defun* vec-copy ((vec vec)) (:result vec :inline t :abbrev vcp)
  (vec-copy! (vec) vec))

(defun* vec-clamp! ((out-vec vec) (vec vec) &key
                    ((min single-float) least-negative-single-float)
                    ((max single-float) most-positive-single-float))
    (:result vec :inline t :abbrev vclamp!)
  (with-vectors ((o out-vec) (v vec))
    (psetf ox (clamp vx min max)
           oy (clamp vy min max)
           oz (clamp vz min max)))
  out-vec)

(defun* vec-clamp ((vec vec) &key
                   ((min single-float) least-negative-single-float)
                   ((max single-float) most-positive-single-float))
    (:result vec :inline t :abbrev vclamp)
  (vec-clamp! (vec) vec :min min :max max))

(defun* vec-stabilize! ((out-vec vec) (vec vec) &key ((tolerance single-float) +epsilon+))
    (:result vec :inline t :abbrev vstab!)
  (with-vectors ((o out-vec) (v vec))
    (macrolet ((stabilize (place)
                 `(if (< (abs ,place) tolerance)
                      0.0
                      ,place)))
      (psetf ox (stabilize vx)
             oy (stabilize vy)
             oz (stabilize vz))))
  out-vec)

(defun* vec-stabilize ((vec vec) &key ((tolerance single-float) +epsilon+)) (:result vec :inline t :abbrev vstab)
  (vec-stabilize! (vec) vec :tolerance tolerance))

(defun* vec-zero! ((vec vec)) (:result vec :inline t :abbrev vzero!)
  (with-vector (v vec)
    (psetf vx 0.0 vy 0.0 vz 0.0))
  vec)

(defun* vec-zero () (:result vec :inline t :abbrev vzero)
  (vec))

(defun* vec-to-list ((vec vec)) (:result list :inline t :abbrev v->list)
  (with-vector (v vec)
    (list vx vy vz)))

(defun* vec-from-list ((list list)) (:result vec :inline t :abbrev list->v)
  (apply #'vec list))

(defun* vec= ((vec1 vec) (vec2 vec)) (:result boolean :inline t :abbrev v=)
  (with-vectors ((v1 vec1) (v2 vec2))
    (and (= v1x v2x)
         (= v1y v2y)
         (= v1z v2z))))

(defun* vec~ ((vec1 vec) (vec2 vec) &key ((tolerance single-float) +epsilon+)) (:result boolean :inline t :abbrev v~)
  (with-vectors ((v1 vec1) (v2 vec2))
    (and (< (abs (- v1x v2x)) tolerance)
         (< (abs (- v1y v2y)) tolerance)
         (< (abs (- v1z v2z)) tolerance))))

(defun* vec+! ((out-vec vec) (vec1 vec) (vec2 vec)) (:result vec :inline t :abbrev v+!)
  (with-vectors ((o out-vec) (v1 vec1) (v2 vec2))
    (psetf ox (+ v1x v2x)
           oy (+ v1y v2y)
           oz (+ v1z v2z)))
  out-vec)

(defun* vec+ ((vec1 vec) (vec2 vec)) (:result vec :inline t :abbrev v+)
  (vec+! (vec) vec1 vec2))

(defun* vec-! ((out-vec vec) (vec1 vec) (vec2 vec)) (:result vec :inline t :abbrev v-!)
  (with-vectors ((o out-vec) (v1 vec1) (v2 vec2))
    (psetf ox (- v1x v2x)
           oy (- v1y v2y)
           oz (- v1z v2z)))
  out-vec)

(defun* vec- ((vec1 vec) (vec2 vec)) (:result vec :inline t :abbrev v-)
  (vec-! (vec) vec1 vec2))

(defun* vec-hadamard*! ((out-vec vec) (vec1 vec) (vec2 vec)) (:result vec :inline t :abbrev vhad*!)
  (with-vectors ((o out-vec) (v1 vec1) (v2 vec2))
    (psetf ox (* v1x v2x)
           oy (* v1y v2y)
           oz (* v1z v2z)))
  out-vec)

(defun* vec-hadamard* ((vec1 vec) (vec2 vec)) (:result vec :inline t :abbrev vhad*)
  (vec-hadamard*! (vec) vec1 vec2))

(defun* vec-hadamard/! ((out-vec vec) (vec1 vec) (vec2 vec)) (:result vec :inline t :abbrev vhad/!)
  (with-vectors ((o out-vec) (v1 vec1) (v2 vec2))
    (psetf ox (if (zerop v2x) 0.0 (/ v1x v2x))
           oy (if (zerop v2y) 0.0 (/ v1y v2y))
           oz (if (zerop v2z) 0.0 (/ v1z v2z))))
  out-vec)

(defun* vec-hadamard/ ((vec1 vec) (vec2 vec)) (:result vec :inline t :abbrev vhad/)
  (vec-hadamard/! (vec) vec1 vec2))

(defun* vec-scale! ((out-vec vec) (vec vec) (scalar single-float)) (:result vec :inline t :abbrev vscale!)
  (with-vectors ((o out-vec) (v vec))
    (psetf ox (* vx scalar)
           oy (* vy scalar)
           oz (* vz scalar)))
  out-vec)

(defun* vec-scale ((vec vec) (scalar single-float)) (:result vec :inline t :abbrev vscale)
  (vec-scale! (vec) vec scalar))

(defun* vec-dot ((vec1 vec) (vec2 vec)) (:result single-float :inline t :abbrev vdot)
  (with-vectors ((v1 vec1) (v2 vec2))
    (+ (* v1x v2x) (* v1y v2y) (* v1z v2z))))

(defun* vec-magnitude-squared ((vec vec)) (:result single-float :inline t :abbrev vmagsq)
  (vec-dot vec vec))

(defun* vec-magnitude ((vec vec)) (:result single-float :inline t :abbrev vmag)
  (sqrt (vec-magnitude-squared vec)))

(defun* vec-normalize! ((out-vec vec) (vec vec)) (:result vec :inline t :abbrev vnormalize!)
  (let ((magnitude (vec-magnitude vec)))
    (unless (zerop magnitude)
      (vec-scale! out-vec vec (/ magnitude)))
    out-vec))

(defun* vec-normalize ((vec vec)) (:result vec :inline t :abbrev vnormalize)
  (vec-normalize! (vec) vec))

(defun* vec-round! ((out-vec vec) (vec vec)) (:result vec :inline t :abbrev vround!)
  (with-vectors ((o out-vec) (v vec))
    (psetf ox (fround vx)
           oy (fround vy)
           oz (fround vz)))
  out-vec)

(defun* vec-round ((vec vec)) (:result vec :inline t :abbrev vround)
  (vec-round! (vec) vec))

(defun* vec-abs! ((out-vec vec) (vec vec)) (:result vec :inline t :abbrev vabs!)
  (with-vectors ((o out-vec) (v vec))
    (psetf ox (abs vx)
           oy (abs vy)
           oz (abs vz)))
  out-vec)

(defun* vec-abs ((vec vec)) (:result vec :inline t :abbrev vabs)
  (vec-abs! (vec) vec))

(defun* vec-negate! ((out-vec vec) (vec vec)) (:result vec :inline t :abbrev vneg!)
  (vec-scale! out-vec vec -1.0))

(defun* vec-negate ((vec vec)) (:result vec :inline t :abbrev vneg)
  (vec-negate! (vec) vec))

(defun* vec-cross! ((out-vec vec) (vec1 vec) (vec2 vec)) (:result vec :inline t :abbrev vcross!)
  (with-vectors ((o out-vec) (v1 vec1) (v2 vec2))
    (psetf ox (- (* v1y v2z) (* v1z v2y))
           oy (- (* v1z v2x) (* v1x v2z))
           oz (- (* v1x v2y) (* v1y v2x))))
  out-vec)

(defun* vec-cross ((vec1 vec) (vec2 vec)) (:result vec :inline t :abbrev vcross)
  (vec-cross! (vec) vec1 vec2))

(defun* vec-box ((vec1 vec) (vec2 vec) (vec3 vec)) (:result single-float :inline t :abbrev vbox)
  (vec-dot (vec-cross vec1 vec2) vec3))

(defun* vec-angle ((vec1 vec) (vec2 vec)) (:result single-float :inline t :abbrev vangle)
  (let ((dot (vec-dot vec1 vec2))
        (m*m (* (vec-magnitude vec1) (vec-magnitude vec2))))
    (if (zerop m*m) 0.0 (acos (/ dot m*m)))))

(defun* vec-zero-p ((vec vec)) (:result boolean :inline t :abbrev vzerop)
  (with-vector (v vec)
    (and (zerop vx)
         (zerop vy)
         (zerop vz))))

(defun* vec-direction= ((vec1 vec) (vec2 vec)) (:result boolean :inline t :abbrev vdir=)
  (> (vec-dot (vec-normalize vec1) (vec-normalize vec2)) (- 1 +epsilon+)))

(defun* vec-parallel-p ((vec1 vec) (vec2 vec)) (:result boolean :inline t :abbrev vparallelp)
  (vec~ (vec-cross vec1 vec2) +zero-vector+))

(defun* vec-lerp! ((out-vec vec) (vec1 vec) (vec2 vec) (coeff single-float)) (:result vec :inline t :abbrev vlerp!)
  (with-vectors ((o out-vec) (v1 vec1) (v2 vec2))
    (psetf ox (lerp coeff v1x v2x)
           oy (lerp coeff v1y v2y)
           oz (lerp coeff v1z v2z)))
  out-vec)

(defun* vec-lerp ((vec1 vec) (vec2 vec) (coeff single-float)) (:result vec :inline t :abbrev vlerp)
  (vec-lerp! (vec) vec1 vec2 coeff))

(defun* vec-min! ((out-vec vec) (vec1 vec) (vec2 vec)) (:result vec :inline t :abbrev vmin!)
  (with-vectors ((o out-vec) (v1 vec1) (v2 vec2))
    (psetf ox (min v1x v2x)
           oy (min v1y v2y)
           oz (min v1z v2z)))
  out-vec)

(defun* vec-min ((vec1 vec) (vec2 vec)) (:result vec :inline t :abbrev vmin)
  (vec-min! (vec) vec1 vec2))

(defun* vec-max! ((out-vec vec) (vec1 vec) (vec2 vec)) (:result vec :inline t :abbrev vmax!)
  (with-vectors ((o out-vec) (v1 vec1) (v2 vec2))
    (psetf ox (max v1x v2x)
           oy (max v1y v2y)
           oz (max v1z v2z)))
  out-vec)

(defun* vec-max ((vec1 vec) (vec2 vec)) (:result vec :inline t :abbrev vmax)
  (vec-max! (vec) vec1 vec2))
