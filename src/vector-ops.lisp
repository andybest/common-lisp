(in-package :gamebox-math)

(defun* vec-test () (:result vec :abbrev vtest)
  "Create a test vector."
  (with-vector (v (vzero))
    (psetf vx 1.0 vy 2.0 vz 3.0)
    v))

(defun* vec-copy! ((out-vec vec) (vec vec)) (:result vec :abbrev vcp!)
  "Copy the components of VEC, storing the result in OUT-VEC."
  (with-vectors ((o out-vec) (v vec))
    (psetf ox vx oy vy oz vz))
  out-vec)

(defun* vec-copy ((vec vec)) (:result vec :abbrev vcp)
  "Copy the components of VEC, storing the result in a new vector."
  (vec-copy! (vzero) vec))

(defun* vec-clamp! ((out-vec vec) (vec vec) &key
                    ((min single-float) most-negative-single-float)
                    ((max single-float) most-positive-single-float))
    (:result vec :abbrev vclamp!)
  "Clamp each component of VEC within the range of [MIN, MAX], storing the result
in OUT-VEC."
  (with-vectors ((o out-vec) (v vec))
    (psetf ox (clamp vx min max)
           oy (clamp vy min max)
           oz (clamp vz min max)))
  out-vec)

(defun* vec-clamp ((vec vec) &key
                   ((min single-float) most-negative-single-float)
                   ((max single-float) most-positive-single-float))
    (:result vec :abbrev vclamp)
  "Clamp each component of VEC within the range of [MIN, MAX], storing the result
as a new vector."
  (vec-clamp! (vzero) vec :min min :max max))

(defun* vec-stabilize! ((out-vec vec) (vec vec)
                        &key ((tolerance single-float) +epsilon+))
    (:result vec :abbrev vstab!)
  "Adjust any component of VEC to zero if below the epsilon TOLERANCE, storing
the result in OUT-VEC."
  (with-vectors ((o out-vec) (v vec))
    (macrolet ((stabilize (place) `(if (< (abs ,place) tolerance) 0.0 ,place)))
      (psetf ox (stabilize vx)
             oy (stabilize vy)
             oz (stabilize vz))))
  out-vec)

(defun* vec-stabilize ((vec vec) &key ((tolerance single-float) +epsilon+))
    (:result vec :abbrev vstab)
  "Adjust any component of VEC to zero if below the epsilon TOLERANCE, storing
the result as a new vector."
  (vec-stabilize! (vzero) vec :tolerance tolerance))

(defun* vec-zero! ((vec vec)) (:result vec :abbrev vzero!)
  "Set each component of VEC to zero."
  (with-vector (v vec)
    (psetf vx 0.0 vy 0.0 vz 0.0))
  vec)

(defun* vec-zero () (:result vec :abbrev vzero)
  "Create a new zero vector. This is the same as calling #'VEC with no
arguments."
  (vec 0 0 0))

(defun* vec-to-list ((vec vec)) (:result list :abbrev v->list)
  "Convert VEC to a list of components."
  (with-vector (v vec)
    (list vx vy vz)))

(defun* vec-from-list ((list list)) (:result vec :abbrev list->v)
  "Convert LIST to a vector."
  (apply #'vec list))

(defun* vec= ((vec1 vec) (vec2 vec)) (:result boolean :abbrev v=)
  "Check if the components of VEC1 are equal to the components of VEC2."
  (with-vectors ((v1 vec1) (v2 vec2))
    (and (= v1x v2x)
         (= v1y v2y)
         (= v1z v2z))))

(defun* vec~ ((vec1 vec) (vec2 vec) &key ((tolerance single-float) +epsilon+))
    (:result boolean :abbrev v~)
  "Check if the components of VEC1 are approximately equal to the components of
VEC2, according to the epsilon TOLERANCE."
  (with-vectors ((v1 vec1) (v2 vec2))
    (and (~ v1x v2x tolerance)
         (~ v1y v2y tolerance)
         (~ v1z v2z tolerance))))

(defun* vec+! ((out-vec vec) (vec1 vec) (vec2 vec)) (:result vec :abbrev v+!)
  "Vector addition of VEC1 and VEC2, storing the result in OUT-VEC."
  (with-vectors ((o out-vec) (v1 vec1) (v2 vec2))
    (psetf ox (+ v1x v2x)
           oy (+ v1y v2y)
           oz (+ v1z v2z)))
  out-vec)

(defun* vec+ ((vec1 vec) (vec2 vec)) (:result vec :abbrev v+)
  "Vector addition of VEC1 and VEC2, storing the result as a new vector."
  (vec+! (vzero) vec1 vec2))

(defun* vec-! ((out-vec vec) (vec1 vec) (vec2 vec)) (:result vec :abbrev v-!)
  "Vector subtraction of VEC2 from VEC1, storing the result in OUT-VEC."
  (with-vectors ((o out-vec) (v1 vec1) (v2 vec2))
    (psetf ox (- v1x v2x)
           oy (- v1y v2y)
           oz (- v1z v2z)))
  out-vec)

(defun* vec- ((vec1 vec) (vec2 vec)) (:result vec :abbrev v-)
  "Vector subtraction of VEC2 from VEC1, storing the result as a new vector."
  (vec-! (vzero) vec1 vec2))

(defun* vec-hadamard*! ((out-vec vec) (vec1 vec) (vec2 vec))
    (:result vec :abbrev vhad*!)
  "Component-wise multiplication (the Hadamard product) of VEC1 and VEC2, storing
the result in OUT-VEC."
  (with-vectors ((o out-vec) (v1 vec1) (v2 vec2))
    (psetf ox (* v1x v2x)
           oy (* v1y v2y)
           oz (* v1z v2z)))
  out-vec)

(defun* vec-hadamard* ((vec1 vec) (vec2 vec)) (:result vec :abbrev vhad*)
  "Component-wise multiplication (the Hadamard product) of VEC1 and VEC2, storing
the result as a new vector."
  (vec-hadamard*! (vzero) vec1 vec2))

(defun* vec-hadamard/! ((out-vec vec) (vec1 vec) (vec2 vec))
    (:result vec :abbrev vhad/!)
  "Component-wise division (the Hadamard quotient) of VEC1 by VEC2, storing the
result in OUT-VEC."
  (with-vectors ((o out-vec) (v1 vec1) (v2 vec2))
    (psetf ox (if (zerop v2x) 0.0 (/ v1x v2x))
           oy (if (zerop v2y) 0.0 (/ v1y v2y))
           oz (if (zerop v2z) 0.0 (/ v1z v2z))))
  out-vec)

(defun* vec-hadamard/ ((vec1 vec) (vec2 vec)) (:result vec :abbrev vhad/)
  "Component-wise division (the Hadamard quotient) of VEC1 by VEC2, storing the
result as a new vector."
  (vec-hadamard/! (vzero) vec1 vec2))

(defun* vec-scale! ((out-vec vec) (vec vec) (scalar single-float))
    (:result vec :abbrev vscale!)
  "Vector scalar multiplication of VEC by SCALAR, storing the result in OUT-VEC."
  (with-vectors ((o out-vec) (v vec))
    (psetf ox (* vx scalar)
           oy (* vy scalar)
           oz (* vz scalar)))
  out-vec)

(defun* vec-scale ((vec vec) (scalar single-float)) (:result vec :abbrev vscale)
  "Vector scalar multiplication of VEC by SCALAR, storing the result as a new
vector."
  (vec-scale! (vzero) vec scalar))

(defun* vec-dot ((vec1 vec) (vec2 vec)) (:result single-float :abbrev vdot)
  "Compute the dot product of VEC1 and VEC2."
  (with-vectors ((v1 vec1) (v2 vec2))
    (+ (* v1x v2x) (* v1y v2y) (* v1z v2z))))

(defun* vec-magnitude-squared ((vec vec)) (:result single-float :abbrev vmagsq)
  "Compute the magnitude (also known as length or Euclidean norm) of VEC. This
results in a squared value, which is cheaper to compute. It is useful when you
want to compare relative vector lengths, which does not need the expensive square
root function. Use VEC-MAGNITUDE for other cases."
  (vec-dot vec vec))

(defun* vec-magnitude ((vec vec)) (:result single-float :abbrev vmag)
  "Compute the magnitude (also known as length or Euclidean norm) of VEC. If you
only need to compare lengths of vectors, use VEC-MAGNITUDE-SQUARED instead, as it
is cheaper to compute without the square root call of this function."
  (sqrt (vec-magnitude-squared vec)))

(defun* vec-normalize! ((out-vec vec) (vec vec))
    (:result vec :abbrev vnormalize!)
  "Normalize a vector so it has a magnitude of 1.0, storing the result in
OUT-VEC."
  (let ((magnitude (vec-magnitude vec)))
    (unless (zerop magnitude)
      (vec-scale! out-vec vec (/ magnitude))))
  out-vec)

(defun* vec-normalize ((vec vec)) (:result vec :abbrev vnormalize)
  "Normalize a vector so it has a magnitude of 1.0, storing the result as a new
vector."
  (vec-normalize! (vzero) vec))

(defun* vec-round! ((out-vec vec) (vec vec)) (:result vec :abbrev vround!)
  "Round each component of VEC to the nearest integer, storing the result in
OUT-VEC."
  (with-vectors ((o out-vec) (v vec))
    (psetf ox (fround vx)
           oy (fround vy)
           oz (fround vz)))
  out-vec)

(defun* vec-round ((vec vec)) (:result vec :abbrev vround)
  "Round each component of VEC to the nearest integer, storing the result as a
new vector."
  (vec-round! (vzero) vec))

(defun* vec-abs! ((out-vec vec) (vec vec)) (:result vec :abbrev vabs!)
  "Modify VEC to have the absolute value of each component, storing the result in
OUT-VEC."
  (with-vectors ((o out-vec) (v vec))
    (psetf ox (abs vx)
           oy (abs vy)
           oz (abs vz)))
  out-vec)

(defun* vec-abs ((vec vec)) (:result vec :abbrev vabs)
  "Modify VEC to have the absolute value of each component, storing the result as
a new vector."
  (vec-abs! (vzero) vec))

(defun* vec-negate! ((out-vec vec) (vec vec)) (:result vec :abbrev vneg!)
  "Negate each component of VEC, storing the result in OUT-VEC."
  (vec-scale! out-vec vec -1.0))

(defun* vec-negate ((vec vec)) (:result vec :abbrev vneg)
  "Negate each component of VEC, storing the result as a new vector."
  (vec-negate! (vzero) vec))

(defun* vec-cross! ((out-vec vec) (vec1 vec) (vec2 vec))
    (:result vec :abbrev vcross!)
  "Compute the cross product of VEC1 and VEC2, storing the result in OUT-VEC."
  (with-vectors ((o out-vec) (v1 vec1) (v2 vec2))
    (psetf ox (- (* v1y v2z) (* v1z v2y))
           oy (- (* v1z v2x) (* v1x v2z))
           oz (- (* v1x v2y) (* v1y v2x))))
  out-vec)

(defun* vec-cross ((vec1 vec) (vec2 vec)) (:result vec :abbrev vcross)
  "Compute the cross product of VEC1 and VEC2, storing the result as a new
vector."
  (vec-cross! (vzero) vec1 vec2))

(defun* vec-box ((vec1 vec) (vec2 vec) (vec3 vec))
    (:result single-float :abbrev vbox)
  "Compute the box product of VEC1, VEC2, and VEC3."
  (vec-dot (vec-cross vec1 vec2) vec3))

(defun* vec-angle ((vec1 vec) (vec2 vec)) (:result single-float :abbrev vangle)
  "Compute the angle in radians between VEC1 and VEC2."
  (let ((dot (vec-dot vec1 vec2))
        (m*m (* (vec-magnitude vec1) (vec-magnitude vec2))))
    (if (zerop m*m) 0.0 (acos (/ dot m*m)))))

(defun* vec-zero-p ((vec vec)) (:result boolean :abbrev vzerop)
  "Check if all components of VEC are zero."
  (with-vector (v vec)
    (and (zerop vx)
         (zerop vy)
         (zerop vz))))

(defun* vec-direction= ((vec1 vec) (vec2 vec)) (:result boolean :abbrev vdir=)
  "Check if the directions of VEC1 and VEC2 are the same."
  (>= (vec-dot (vec-normalize vec1) (vec-normalize vec2)) (- 1 +epsilon+)))

(defun* vec-parallel-p ((vec1 vec) (vec2 vec))
    (:result boolean :abbrev vparallelp)
  "Check if VEC1 and VEC2 are parallel to each other."
  (vec~ (vec-cross vec1 vec2) +zero-vector+))

(defun* vec-lerp! ((out-vec vec) (vec1 vec) (vec2 vec) (coeff single-float))
    (:result vec :abbrev vlerp!)
  "Perform a linear interpolation between VEC1 and VEC2 by the interpolation
coefficient COEFF, storing the result in OUT-VEC."
  (with-vectors ((o out-vec) (v1 vec1) (v2 vec2))
    (psetf ox (lerp coeff v1x v2x)
           oy (lerp coeff v1y v2y)
           oz (lerp coeff v1z v2z)))
  out-vec)

(defun* vec-lerp ((vec1 vec) (vec2 vec) (coeff single-float))
    (:result vec :abbrev vlerp)
  "Perform a linear interpolation between VEC1 and VEC2 by the interpolation
coefficient COEFF, storing the result as a new vector."
  (vec-lerp! (vzero) vec1 vec2 coeff))

(defun* vec< ((vec1 vec) (vec2 vec)) (:result boolean :abbrev v<)
  "Check if each component of VEC1 is less than that component of VEC2."
  (with-vectors ((v1 vec1) (v2 vec2))
    (and (< v1x v2x)
         (< v1y v2y)
         (< v1z v2z))))

(defun* vec<= ((vec1 vec) (vec2 vec)) (:result boolean :abbrev v<=)
  "Check if each component of VEC1 is less than or equal to that component of
VEC2."
  (with-vectors ((v1 vec1) (v2 vec2))
    (and (<= v1x v2x)
         (<= v1y v2y)
         (<= v1z v2z))))

(defun* vec> ((vec1 vec) (vec2 vec)) (:result boolean :abbrev v>)
  "Check if each component of VEC1 is greater than that component of VEC2."
  (with-vectors ((v1 vec1) (v2 vec2))
    (and (> v1x v2x)
         (> v1y v2y)
         (> v1z v2z))))

(defun* vec>= ((vec1 vec) (vec2 vec)) (:result boolean :abbrev v>=)
  "Check if each component of VEC1 is greater than or equal to that component of
VEC2."
  (with-vectors ((v1 vec1) (v2 vec2))
    (and (>= v1x v2x)
         (>= v1y v2y)
         (>= v1z v2z))))

(defun* vec-min! ((out-vec vec) (vec1 vec) (vec2 vec))
    (:result vec :abbrev vmin!)
  "Component-wise minimum of VEC1 and VEC2, storing the result in OUT-VEC."
  (with-vectors ((o out-vec) (v1 vec1) (v2 vec2))
    (psetf ox (min v1x v2x)
           oy (min v1y v2y)
           oz (min v1z v2z)))
  out-vec)

(defun* vec-min ((vec1 vec) (vec2 vec)) (:result vec :abbrev vmin)
  "Component-wise minimum of VEC1 and VEC2, storing the result as a new vector."
  (vec-min! (vzero) vec1 vec2))

(defun* vec-max! ((out-vec vec) (vec1 vec) (vec2 vec))
    (:result vec :abbrev vmax!)
  "Component-wise maximum of VEC1 and VEC2, storing the result in OUT-VEC."
  (with-vectors ((o out-vec) (v1 vec1) (v2 vec2))
    (psetf ox (max v1x v2x)
           oy (max v1y v2y)
           oz (max v1z v2z)))
  out-vec)

(defun* vec-max ((vec1 vec) (vec2 vec)) (:result vec :abbrev vmax)
  "Component-wise maximum of VEC1 and VEC2, storing the result as a new vector."
  (vec-max! (vzero) vec1 vec2))
