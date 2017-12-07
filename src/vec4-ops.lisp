(in-package :gamebox-math)

(defun* vec4-test () (:result vec4 :abbrev v4test)
  "Create a test vector."
  (with-vec4 (v (v4zero))
    (psetf vx 1.0 vy 2.0 vz 3.0 vw 4.0)
    v))

(defun* vec4-copy! ((out-vec vec4) (vec vec4)) (:result vec4 :abbrev v4cp!)
  "Copy the components of VEC, storing the result in OUT-VEC."
  (with-vec4s ((o out-vec) (v vec))
    (psetf ox vx oy vy oz vz ow vw))
  out-vec)

(defun* vec4-copy ((vec vec4)) (:result vec4 :abbrev v4cp)
  "Copy the components of VEC, storing the result in a new vector."
  (v4cp! (v4zero) vec))

(defun* vec4-clamp! ((out-vec vec4) (vec vec4) &key
                    ((min single-float) most-negative-single-float)
                    ((max single-float) most-positive-single-float))
    (:result vec4 :abbrev v4clamp!)
  "Clamp each component of VEC within the range of [MIN, MAX], storing the result
in OUT-VEC."
  (with-vec4s ((o out-vec) (v vec))
    (psetf ox (clamp vx min max)
           oy (clamp vy min max)
           oz (clamp vz min max)
           ow (clamp vw min max)))
  out-vec)

(defun* vec4-clamp ((vec vec4) &key
                   ((min single-float) most-negative-single-float)
                   ((max single-float) most-positive-single-float))
    (:result vec4 :abbrev v4clamp)
  "Clamp each component of VEC within the range of [MIN, MAX], storing the result
as a new vector."
  (v4clamp! (v4zero) vec :min min :max max))

(defun* vec4-stabilize! ((out-vec vec4) (vec vec4)
                        &key ((tolerance single-float) +epsilon+))
    (:result vec4 :abbrev v4stab!)
  "Adjust any component of VEC to zero if below the epsilon TOLERANCE, storing
the result in OUT-VEC."
  (with-vec4s ((o out-vec) (v vec))
    (macrolet ((stabilize (place) `(if (< (abs ,place) tolerance) 0.0 ,place)))
      (psetf ox (stabilize vx)
             oy (stabilize vy)
             oz (stabilize vz)
             ow (stabilize vw))))
  out-vec)

(defun* vec4-stabilize ((vec vec4) &key ((tolerance single-float) +epsilon+))
    (:result vec4 :abbrev v4stab)
  "Adjust any component of VEC to zero if below the epsilon TOLERANCE, storing
the result as a new vector."
  (v4stab! (v4zero) vec :tolerance tolerance))

(defun* vec4-zero! ((vec vec4)) (:result vec4 :abbrev v4zero!)
  "Set each component of VEC to zero."
  (with-vec4 (v vec)
    (psetf vx 0.0 vy 0.0 vz 0.0 vw 0.0))
  vec)

(defun* vec4-zero () (:result vec4 :abbrev v4zero)
  "Create a new zero vector."
  (vec4 0 0 0 0))

(defun* vec4-to-list ((vec vec4)) (:result list :abbrev v4->list)
  "Convert VEC to a list of components."
  (with-vec4 (v vec)
    (list vx vy vz vw)))

(defun* vec4-from-list ((list list)) (:result vec4 :abbrev list->v4)
  "Convert LIST to a vector."
  (apply #'vec4 list))

(defun* vec4= ((vec-a vec4) (vec-b vec4)) (:result boolean :abbrev v4=)
  "Check if the components of VEC-A are equal to the components of VEC-B."
  (with-vec4s ((v1 vec-a) (v2 vec-b))
    (and (= v1x v2x)
         (= v1y v2y)
         (= v1z v2z)
         (= v1w v2w))))

(defun* vec4~ ((vec-a vec4) (vec-b vec4)
               &key ((tolerance single-float) +epsilon+))
    (:result boolean :abbrev v4~)
  "Check if the components of VEC-A are approximately equal to the components of
VEC-B, according to the epsilon TOLERANCE."
  (with-vec4s ((v1 vec-a) (v2 vec-b))
    (and (~ v1x v2x tolerance)
         (~ v1y v2y tolerance)
         (~ v1z v2z tolerance)
         (~ v1w v2w tolerance))))

(defun* vec4+! ((out-vec vec4) (vec-a vec4) (vec-b vec4))
    (:result vec4 :abbrev v4+!)
  "Vector addition of VEC-A and VEC-B, storing the result in OUT-VEC."
  (with-vec4s ((o out-vec) (v1 vec-a) (v2 vec-b))
    (psetf ox (+ v1x v2x)
           oy (+ v1y v2y)
           oz (+ v1z v2z)
           ow (+ v1w v2w)))
  out-vec)

(defun* vec4+ ((vec-a vec4) (vec-b vec4)) (:result vec4 :abbrev v4+)
  "Vector addition of VEC-A and VEC-B, storing the result as a new vector."
  (v4+! (v4zero) vec-a vec-b))

(defun* vec4-! ((out-vec vec4) (vec-a vec4) (vec-b vec4))
    (:result vec4 :abbrev v4-!)
  "Vector subtraction of VEC-A from VEC-B, storing the result in OUT-VEC."
  (with-vec4s ((o out-vec) (v1 vec-a) (v2 vec-b))
    (psetf ox (- v1x v2x)
           oy (- v1y v2y)
           oz (- v1z v2z)
           ow (- v1w v2w)))
  out-vec)

(defun* vec4- ((vec-a vec4) (vec-b vec4)) (:result vec4 :abbrev v4-)
  "Vector subtraction of VEC-A from VEC-B, storing the result as a new vector."
  (v4-! (v4zero) vec-a vec-b))

(defun* vec4-hadamard*! ((out-vec vec4) (vec-a vec4) (vec-b vec4))
    (:result vec4 :abbrev v4had*!)
  "Component-wise multiplication (the Hadamard product) of VEC-A and VEC-B,
storing the result in OUT-VEC."
  (with-vec4s ((o out-vec) (v1 vec-a) (v2 vec-b))
    (psetf ox (* v1x v2x)
           oy (* v1y v2y)
           oz (* v1z v2z)
           ow (* v1w v2w)))
  out-vec)

(defun* vec4-hadamard* ((vec-a vec4) (vec-b vec4))
    (:result vec4 :abbrev v4had*)
  "Component-wise multiplication (the Hadamard product) of VEC-A and VEC-B,
storing the result as a new vector."
  (v4had*! (v4zero) vec-a vec-b))

(defun* vec4-hadamard/! ((out-vec vec4) (vec-a vec4) (vec-b vec4))
    (:result vec4 :abbrev v4had/!)
  "Component-wise division (the Hadamard quotient) of VEC-A by VEC-B, storing the
result in OUT-VEC."
  (with-vec4s ((o out-vec) (v1 vec-a) (v2 vec-b))
    (psetf ox (if (zerop v2x) 0.0 (/ v1x v2x))
           oy (if (zerop v2y) 0.0 (/ v1y v2y))
           oz (if (zerop v2z) 0.0 (/ v1z v2z))
           ow (if (zerop v2w) 0.0 (/ v1w v2w))))
  out-vec)

(defun* vec4-hadamard/ ((vec-a vec4) (vec-b vec4)) (:result vec4 :abbrev v4had/)
  "Component-wise division (the Hadamard quotient) of VEC-A by VEC-B, storing the
result as a new vector."
  (v4had/! (v4zero) vec-a vec-b))

(defun* vec4-scale! ((out-vec vec4) (vec vec4) (scalar single-float))
    (:result vec4 :abbrev v4scale!)
  "Vector scalar multiplication of VEC by SCALAR, storing the result in OUT-VEC."
  (with-vec4s ((o out-vec) (v vec))
    (psetf ox (* vx scalar)
           oy (* vy scalar)
           oz (* vz scalar)
           ow (* vw scalar)))
  out-vec)

(defun* vec4-scale ((vec vec4) (scalar single-float))
    (:result vec4 :abbrev v4scale)
  "Vector scalar multiplication of VEC by SCALAR, storing the result as a new
vector."
  (v4scale! (v4zero) vec scalar))

(defun* vec4-dot ((vec-a vec4) (vec-b vec4)) (:result single-float :abbrev v4dot)
  "Compute the dot product of VEC-A and VEC-B."
  (with-vec4s ((v1 vec-a) (v2 vec-b))
    (+ (* v1x v2x) (* v1y v2y) (* v1z v2z) (* v1w v2w))))

(defun* vec4-magnitude-squared ((vec vec4))
    (:result single-float :abbrev v4magsq)
  "Compute the magnitude (also known as length or Euclidean norm) of VEC. This
results in a squared value, which is cheaper to compute. It is useful when you
want to compare relative vector lengths, which does not need the expensive square
root function. Use VEC4-MAGNITUDE for other cases."
  (v4dot vec vec))

(defun* vec4-magnitude ((vec vec4)) (:result single-float :abbrev v4mag)
  "Compute the magnitude (also known as length or Euclidean norm) of VEC. If you
only need to compare lengths of vectors, use VEC4-MAGNITUDE-SQUARED instead, as
it is cheaper to compute without the square root call of this function."
  (sqrt (v4magsq vec)))

(defun* vec4-normalize! ((out-vec vec4) (vec vec4))
    (:result vec4 :abbrev v4normalize!)
  "Normalize a vector so it has a magnitude of 1.0, storing the result in
OUT-VEC."
  (let ((magnitude (v4mag vec)))
    (unless (zerop magnitude)
      (v4scale! out-vec vec (/ magnitude))))
  out-vec)

(defun* vec4-normalize ((vec vec4)) (:result vec4 :abbrev v4normalize)
  "Normalize a vector so it has a magnitude of 1.0, storing the result as a new
vector."
  (v4normalize! (v4zero) vec))

(defun* vec4-round! ((out-vec vec4) (vec vec4)) (:result vec4 :abbrev v4round!)
  "Round each component of VEC to the nearest integer, storing the result in
OUT-VEC."
  (with-vec4s ((o out-vec) (v vec))
    (psetf ox (fround vx)
           oy (fround vy)
           oz (fround vz)
           ow (fround vw)))
  out-vec)

(defun* vec4-round ((vec vec4)) (:result vec4 :abbrev v4round)
  "Round each component of VEC to the nearest integer, storing the result as a
new vector."
  (v4round! (v4zero) vec))

(defun* vec4-abs! ((out-vec vec4) (vec vec4)) (:result vec4 :abbrev v4abs!)
  "Modify VEC to have the absolute value of each component, storing the result in
OUT-VEC."
  (with-vec4s ((o out-vec) (v vec))
    (psetf ox (abs vx)
           oy (abs vy)
           oz (abs vz)
           ow (abs vw)))
  out-vec)

(defun* vec4-abs ((vec vec4)) (:result vec4 :abbrev v4abs)
  "Modify VEC to have the absolute value of each component, storing the result as
a new vector."
  (v4abs! (v4zero) vec))

(defun* vec4-negate! ((out-vec vec4) (vec vec4)) (:result vec4 :abbrev v4neg!)
  "Negate each component of VEC, storing the result in OUT-VEC."
  (v4scale! out-vec vec -1.0))

(defun* vec4-negate ((vec vec4)) (:result vec4 :abbrev v4neg)
  "Negate each component of VEC, storing the result as a new vector."
  (v4neg! (v4zero) vec))

(defun* vec4-zero-p ((vec vec4)) (:result boolean :abbrev v4zerop)
  "Check if all components of VEC are zero."
  (with-vec4 (v vec)
    (and (zerop vx)
         (zerop vy)
         (zerop vz)
         (zerop vw))))

(defun* vec4-lerp! ((out-vec vec4) (vec-a vec4) (vec-b vec4)
                    (factor single-float))
    (:result vec4 :abbrev v4lerp!)
  "Perform a linear interpolation between VEC-A and VEC-B by the interpolation
factor FACTOR, storing the result in OUT-VEC."
  (with-vec4s ((o out-vec) (v1 vec-a) (v2 vec-b))
    (psetf ox (lerp factor v1x v2x)
           oy (lerp factor v1y v2y)
           oz (lerp factor v1z v2z)
           ow (lerp factor v1w v2w)))
  out-vec)

(defun* vec4-lerp ((vec-a vec4) (vec-b vec4) (factor single-float))
    (:result vec4 :abbrev v4lerp)
  "Perform a linear interpolation between VEC-A and VEC-B by the interpolation
factor FACTOR, storing the result as a new vector."
  (v4lerp! (v4zero) vec-a vec-b factor))

(defun* vec4< ((vec-a vec4) (vec-b vec4)) (:result boolean :abbrev v4<)
  "Check if each component of VEC-A is less than that component of VEC-B."
  (with-vec4s ((v1 vec-a) (v2 vec-b))
    (and (< v1x v2x)
         (< v1y v2y)
         (< v1z v2z)
         (< v1w v2w))))

(defun* vec4<= ((vec-a vec4) (vec-b vec4)) (:result boolean :abbrev v4<=)
  "Check if each component of VEC-A is less than or equal to that component of
VEC-B."
  (with-vec4s ((v1 vec-a) (v2 vec-b))
    (and (<= v1x v2x)
         (<= v1y v2y)
         (<= v1z v2z)
         (<= v1w v2w))))

(defun* vec4> ((vec-a vec4) (vec-b vec4)) (:result boolean :abbrev v4>)
  "Check if each component of VEC-A is greater than that component of VEC-B."
  (with-vec4s ((v1 vec-a) (v2 vec-b))
    (and (> v1x v2x)
         (> v1y v2y)
         (> v1z v2z)
         (> v1w v2w))))

(defun* vec4>= ((vec-a vec4) (vec-b vec4)) (:result boolean :abbrev v4>=)
  "Check if each component of VEC-A is greater than or equal to that component of
VEC-B."
  (with-vec4s ((v1 vec-a) (v2 vec-b))
    (and (>= v1x v2x)
         (>= v1y v2y)
         (>= v1z v2z)
         (>= v1w v2w))))

(defun* vec4-min! ((out-vec vec4) (vec-a vec4) (vec-b vec4))
    (:result vec4 :abbrev v4min!)
  "Component-wise minimum of VEC-A and VEC-B, storing the result in OUT-VEC."
  (with-vec4s ((o out-vec) (v1 vec-a) (v2 vec-b))
    (psetf ox (min v1x v2x)
           oy (min v1y v2y)
           oz (min v1z v2z)
           ow (min v1w v2w)))
  out-vec)

(defun* vec4-min ((vec-a vec4) (vec-b vec4)) (:result vec4 :abbrev v4min)
  "Component-wise minimum of VEC-A and VEC-B, storing the result as a new
vector."
  (v4min! (v4zero) vec-a vec-b))

(defun* vec4-max! ((out-vec vec4) (vec-a vec4) (vec-b vec4))
    (:result vec4 :abbrev v4max!)
  "Component-wise maximum of VEC-A and VEC-B, storing the result in OUT-VEC."
  (with-vec4s ((o out-vec) (v1 vec-a) (v2 vec-b))
    (psetf ox (max v1x v2x)
           oy (max v1y v2y)
           oz (max v1z v2z)
           ow (max v1w v2w)))
  out-vec)

(defun* vec4-max ((vec-a vec4) (vec-b vec4)) (:result vec4 :abbrev v4max)
  "Component-wise maximum of VEC-A and VEC-B, storing the result as a new
vector."
  (v4max! (v4zero) vec-a vec-b))
