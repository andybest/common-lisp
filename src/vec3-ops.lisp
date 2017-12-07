(in-package :gamebox-math)

(defun* vec3-test () (:result vec3 :abbrev v3test)
  "Create a test vector."
  (with-vec3 (v (v3zero))
    (psetf vx 1.0 vy 2.0 vz 3.0)
    v))

(defun* vec3-copy! ((out-vec vec3) (vec vec3)) (:result vec3 :abbrev v3cp!)
  "Copy the components of VEC, storing the result in OUT-VEC."
  (with-vec3s ((o out-vec) (v vec))
    (psetf ox vx oy vy oz vz))
  out-vec)

(defun* vec3-copy ((vec vec3)) (:result vec3 :abbrev v3cp)
  "Copy the components of VEC, storing the result in a new vector."
  (v3cp! (v3zero) vec))

(defun* vec3-clamp! ((out-vec vec3) (vec vec3) &key
                    ((min single-float) most-negative-single-float)
                    ((max single-float) most-positive-single-float))
    (:result vec3 :abbrev v3clamp!)
  "Clamp each component of VEC within the range of [MIN, MAX], storing the result
in OUT-VEC."
  (with-vec3s ((o out-vec) (v vec))
    (psetf ox (clamp vx min max)
           oy (clamp vy min max)
           oz (clamp vz min max)))
  out-vec)

(defun* vec3-clamp ((vec vec3) &key
                   ((min single-float) most-negative-single-float)
                   ((max single-float) most-positive-single-float))
    (:result vec3 :abbrev v3clamp)
  "Clamp each component of VEC within the range of [MIN, MAX], storing the result
as a new vector."
  (v3clamp! (v3zero) vec :min min :max max))

(defun* vec3-stabilize! ((out-vec vec3) (vec vec3)
                        &key ((tolerance single-float) +epsilon+))
    (:result vec3 :abbrev v3stab!)
  "Adjust any component of VEC to zero if below the epsilon TOLERANCE, storing
the result in OUT-VEC."
  (with-vec3s ((o out-vec) (v vec))
    (macrolet ((stabilize (place) `(if (< (abs ,place) tolerance) 0.0 ,place)))
      (psetf ox (stabilize vx)
             oy (stabilize vy)
             oz (stabilize vz))))
  out-vec)

(defun* vec3-stabilize ((vec vec3) &key ((tolerance single-float) +epsilon+))
    (:result vec3 :abbrev v3stab)
  "Adjust any component of VEC to zero if below the epsilon TOLERANCE, storing
the result as a new vector."
  (v3stab! (v3zero) vec :tolerance tolerance))

(defun* vec3-zero! ((vec vec3)) (:result vec3 :abbrev v3zero!)
  "Set each component of VEC to zero."
  (with-vec3 (v vec)
    (psetf vx 0.0 vy 0.0 vz 0.0))
  vec)

(defun* vec3-zero () (:result vec3 :abbrev v3zero)
  "Create a new zero vector."
  (vec3 0 0 0))

(defun* vec3-to-list ((vec vec3)) (:result list :abbrev v3->list)
  "Convert VEC to a list of components."
  (with-vec3 (v vec)
    (list vx vy vz)))

(defun* vec3-from-list ((list list)) (:result vec3 :abbrev list->v3)
  "Convert LIST to a vector."
  (apply #'vec3 list))

(defun* vec3= ((vec-a vec3) (vec-b vec3)) (:result boolean :abbrev v3=)
  "Check if the components of VEC-A are equal to the components of VEC-B."
  (with-vec3s ((v1 vec-a) (v2 vec-b))
    (and (= v1x v2x)
         (= v1y v2y)
         (= v1z v2z))))

(defun* vec3~ ((vec-a vec3) (vec-b vec3)
               &key ((tolerance single-float) +epsilon+))
    (:result boolean :abbrev v3~)
  "Check if the components of VEC-A are approximately equal to the components of
VEC-B, according to the epsilon TOLERANCE."
  (with-vec3s ((v1 vec-a) (v2 vec-b))
    (and (~ v1x v2x tolerance)
         (~ v1y v2y tolerance)
         (~ v1z v2z tolerance))))

(defun* vec3+! ((out-vec vec3) (vec-a vec3) (vec-b vec3))
    (:result vec3 :abbrev v3+!)
  "Vector addition of VEC-A and VEC-B, storing the result in OUT-VEC."
  (with-vec3s ((o out-vec) (v1 vec-a) (v2 vec-b))
    (psetf ox (+ v1x v2x)
           oy (+ v1y v2y)
           oz (+ v1z v2z)))
  out-vec)

(defun* vec3+ ((vec-a vec3) (vec-b vec3)) (:result vec3 :abbrev v3+)
  "Vector addition of VEC-A and VEC-B, storing the result as a new vector."
  (v3+! (v3zero) vec-a vec-b))

(defun* vec3-! ((out-vec vec3) (vec-a vec3) (vec-b vec3))
    (:result vec3 :abbrev v3-!)
  "Vector subtraction of VEC-A from VEC-B, storing the result in OUT-VEC."
  (with-vec3s ((o out-vec) (v1 vec-a) (v2 vec-b))
    (psetf ox (- v1x v2x)
           oy (- v1y v2y)
           oz (- v1z v2z)))
  out-vec)

(defun* vec3- ((vec-a vec3) (vec-b vec3)) (:result vec3 :abbrev v3-)
  "Vector subtraction of VEC-A from VEC-B, storing the result as a new vector."
  (v3-! (v3zero) vec-a vec-b))

(defun* vec3-hadamard*! ((out-vec vec3) (vec-a vec3) (vec-b vec3))
    (:result vec3 :abbrev v3had*!)
  "Component-wise multiplication (the Hadamard product) of VEC-A and VEC-B,
storing the result in OUT-VEC."
  (with-vec3s ((o out-vec) (v1 vec-a) (v2 vec-b))
    (psetf ox (* v1x v2x)
           oy (* v1y v2y)
           oz (* v1z v2z)))
  out-vec)

(defun* vec3-hadamard* ((vec-a vec3) (vec-b vec3))
    (:result vec3 :abbrev v3had*)
  "Component-wise multiplication (the Hadamard product) of VEC-A and VEC-B,
storing the result as a new vector."
  (v3had*! (v3zero) vec-a vec-b))

(defun* vec3-hadamard/! ((out-vec vec3) (vec-a vec3) (vec-b vec3))
    (:result vec3 :abbrev v3had/!)
  "Component-wise division (the Hadamard quotient) of VEC-A by VEC-B, storing the
result in OUT-VEC."
  (with-vec3s ((o out-vec) (v1 vec-a) (v2 vec-b))
    (psetf ox (if (zerop v2x) 0.0 (/ v1x v2x))
           oy (if (zerop v2y) 0.0 (/ v1y v2y))
           oz (if (zerop v2z) 0.0 (/ v1z v2z))))
  out-vec)

(defun* vec3-hadamard/ ((vec-a vec3) (vec-b vec3)) (:result vec3 :abbrev v3had/)
  "Component-wise division (the Hadamard quotient) of VEC-A by VEC-B, storing the
result as a new vector."
  (v3had/! (v3zero) vec-a vec-b))

(defun* vec3-scale! ((out-vec vec3) (vec vec3) (scalar single-float))
    (:result vec3 :abbrev v3scale!)
  "Vector scalar multiplication of VEC by SCALAR, storing the result in OUT-VEC."
  (with-vec3s ((o out-vec) (v vec))
    (psetf ox (* vx scalar)
           oy (* vy scalar)
           oz (* vz scalar)))
  out-vec)

(defun* vec3-scale ((vec vec3) (scalar single-float))
    (:result vec3 :abbrev v3scale)
  "Vector scalar multiplication of VEC by SCALAR, storing the result as a new
vector."
  (v3scale! (v3zero) vec scalar))

(defun* vec3-dot ((vec-a vec3) (vec-b vec3)) (:result single-float :abbrev v3dot)
  "Compute the dot product of VEC-A and VEC-B."
  (with-vec3s ((v1 vec-a) (v2 vec-b))
    (+ (* v1x v2x) (* v1y v2y) (* v1z v2z))))

(defun* vec3-magnitude-squared ((vec vec3))
    (:result single-float :abbrev v3magsq)
  "Compute the magnitude (also known as length or Euclidean norm) of VEC. This
results in a squared value, which is cheaper to compute. It is useful when you
want to compare relative vector lengths, which does not need the expensive square
root function. Use VEC3-MAGNITUDE for other cases."
  (v3dot vec vec))

(defun* vec3-magnitude ((vec vec3)) (:result single-float :abbrev v3mag)
  "Compute the magnitude (also known as length or Euclidean norm) of VEC. If you
only need to compare lengths of vectors, use VEC3-MAGNITUDE-SQUARED instead, as
it is cheaper to compute without the square root call of this function."
  (sqrt (v3magsq vec)))

(defun* vec3-normalize! ((out-vec vec3) (vec vec3))
    (:result vec3 :abbrev v3normalize!)
  "Normalize a vector so it has a magnitude of 1.0, storing the result in
OUT-VEC."
  (let ((magnitude (v3mag vec)))
    (unless (zerop magnitude)
      (v3scale! out-vec vec (/ magnitude))))
  out-vec)

(defun* vec3-normalize ((vec vec3)) (:result vec3 :abbrev v3normalize)
  "Normalize a vector so it has a magnitude of 1.0, storing the result as a new
vector."
  (v3normalize! (v3zero) vec))

(defun* vec3-round! ((out-vec vec3) (vec vec3)) (:result vec3 :abbrev v3round!)
  "Round each component of VEC to the nearest integer, storing the result in
OUT-VEC."
  (with-vec3s ((o out-vec) (v vec))
    (psetf ox (fround vx)
           oy (fround vy)
           oz (fround vz)))
  out-vec)

(defun* vec3-round ((vec vec3)) (:result vec3 :abbrev v3round)
  "Round each component of VEC to the nearest integer, storing the result as a
new vector."
  (v3round! (v3zero) vec))

(defun* vec3-abs! ((out-vec vec3) (vec vec3)) (:result vec3 :abbrev v3abs!)
  "Modify VEC to have the absolute value of each component, storing the result in
OUT-VEC."
  (with-vec3s ((o out-vec) (v vec))
    (psetf ox (abs vx)
           oy (abs vy)
           oz (abs vz)))
  out-vec)

(defun* vec3-abs ((vec vec3)) (:result vec3 :abbrev v3abs)
  "Modify VEC to have the absolute value of each component, storing the result as
a new vector."
  (v3abs! (v3zero) vec))

(defun* vec3-negate! ((out-vec vec3) (vec vec3)) (:result vec3 :abbrev v3neg!)
  "Negate each component of VEC, storing the result in OUT-VEC."
  (v3scale! out-vec vec -1.0))

(defun* vec3-negate ((vec vec3)) (:result vec3 :abbrev v3neg)
  "Negate each component of VEC, storing the result as a new vector."
  (v3neg! (v3zero) vec))

(defun* vec3-cross! ((out-vec vec3) (vec-a vec3) (vec-b vec3))
    (:result vec3 :abbrev v3cross!)
  "Compute the cross product of VEC-A and VEC-B, storing the result in OUT-VEC."
  (with-vec3s ((o out-vec) (v1 vec-a) (v2 vec-b))
    (psetf ox (- (* v1y v2z) (* v1z v2y))
           oy (- (* v1z v2x) (* v1x v2z))
           oz (- (* v1x v2y) (* v1y v2x))))
  out-vec)

(defun* vec3-cross ((vec-a vec3) (vec-b vec3)) (:result vec3 :abbrev v3cross)
  "Compute the cross product of VEC-A and VEC-B, storing the result as a new
vector."
  (v3cross! (v3zero) vec-a vec-b))

(defun* vec3-box ((vec-a vec3) (vec-b vec3) (vec-c vec3))
    (:result single-float :abbrev v3box)
  "Compute the box product of VEC-A, VEC-B, and VEC-C."
  (v3dot (v3cross vec-a vec-b) vec-c))

(defun* vec3-angle ((vec-a vec3) (vec-b vec3))
    (:result single-float :abbrev v3angle)
  "Compute the angle in radians between VEC-A and VEC-B."
  (let ((dot (v3dot vec-a vec-b))
        (m*m (* (v3mag vec-a) (v3mag vec-b))))
    (if (zerop m*m) 0.0 (acos (/ dot m*m)))))

(defun* vec3-zero-p ((vec vec3)) (:result boolean :abbrev v3zerop)
  "Check if all components of VEC are zero."
  (with-vec3 (v vec)
    (and (zerop vx)
         (zerop vy)
         (zerop vz))))

(defun* vec3-direction= ((vec-a vec3) (vec-b vec3))
    (:result boolean :abbrev v3dir=)
  "Check if the directions of VEC-A and VEC-B are the same."
  (>= (v3dot (v3normalize vec-a) (v3normalize vec-b)) (- 1 +epsilon+)))

(defun* vec3-parallel-p ((vec-a vec3) (vec-b vec3))
    (:result boolean :abbrev v3parallelp)
  "Check if VEC-A and VEC-B are parallel to each other."
  (v3~ (v3cross vec-a vec-b) +zero-vec3+))

(defun* vec3-lerp! ((out-vec vec3) (vec-a vec3) (vec-b vec3)
                    (factor single-float))
    (:result vec3 :abbrev v3lerp!)
  "Perform a linear interpolation between VEC-A and VEC-B by the interpolation
factor FACTOR, storing the result in OUT-VEC."
  (with-vec3s ((o out-vec) (v1 vec-a) (v2 vec-b))
    (psetf ox (lerp factor v1x v2x)
           oy (lerp factor v1y v2y)
           oz (lerp factor v1z v2z)))
  out-vec)

(defun* vec3-lerp ((vec-a vec3) (vec-b vec3) (factor single-float))
    (:result vec3 :abbrev v3lerp)
  "Perform a linear interpolation between VEC-A and VEC-B by the interpolation
factor FACTOR, storing the result as a new vector."
  (v3lerp! (v3zero) vec-a vec-b factor))

(defun* vec3< ((vec-a vec3) (vec-b vec3)) (:result boolean :abbrev v3<)
  "Check if each component of VEC-A is less than that component of VEC-B."
  (with-vec3s ((v1 vec-a) (v2 vec-b))
    (and (< v1x v2x)
         (< v1y v2y)
         (< v1z v2z))))

(defun* vec3<= ((vec-a vec3) (vec-b vec3)) (:result boolean :abbrev v3<=)
  "Check if each component of VEC-A is less than or equal to that component of
VEC-B."
  (with-vec3s ((v1 vec-a) (v2 vec-b))
    (and (<= v1x v2x)
         (<= v1y v2y)
         (<= v1z v2z))))

(defun* vec3> ((vec-a vec3) (vec-b vec3)) (:result boolean :abbrev v3>)
  "Check if each component of VEC-A is greater than that component of VEC-B."
  (with-vec3s ((v1 vec-a) (v2 vec-b))
    (and (> v1x v2x)
         (> v1y v2y)
         (> v1z v2z))))

(defun* vec3>= ((vec-a vec3) (vec-b vec3)) (:result boolean :abbrev v3>=)
  "Check if each component of VEC-A is greater than or equal to that component of
VEC-B."
  (with-vec3s ((v1 vec-a) (v2 vec-b))
    (and (>= v1x v2x)
         (>= v1y v2y)
         (>= v1z v2z))))

(defun* vec3-min! ((out-vec vec3) (vec-a vec3) (vec-b vec3))
    (:result vec3 :abbrev v3min!)
  "Component-wise minimum of VEC-A and VEC-B, storing the result in OUT-VEC."
  (with-vec3s ((o out-vec) (v1 vec-a) (v2 vec-b))
    (psetf ox (min v1x v2x)
           oy (min v1y v2y)
           oz (min v1z v2z)))
  out-vec)

(defun* vec3-min ((vec-a vec3) (vec-b vec3)) (:result vec3 :abbrev v3min)
  "Component-wise minimum of VEC-A and VEC-B, storing the result as a new
vector."
  (v3min! (v3zero) vec-a vec-b))

(defun* vec3-max! ((out-vec vec3) (vec-a vec3) (vec-b vec3))
    (:result vec3 :abbrev v3max!)
  "Component-wise maximum of VEC-A and VEC-B, storing the result in OUT-VEC."
  (with-vec3s ((o out-vec) (v1 vec-a) (v2 vec-b))
    (psetf ox (max v1x v2x)
           oy (max v1y v2y)
           oz (max v1z v2z)))
  out-vec)

(defun* vec3-max ((vec-a vec3) (vec-b vec3)) (:result vec3 :abbrev v3max)
  "Component-wise maximum of VEC-A and VEC-B, storing the result as a new
vector."
  (v3max! (v3zero) vec-a vec-b))
