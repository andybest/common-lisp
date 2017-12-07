(in-package :gamebox-math)

(defun* vec2-test () (:result vec2 :abbrev v2test)
  "Create a test vector."
  (with-vec2 (v (v2zero))
    (psetf vx 1.0 vy 2.0)
    v))

(defun* vec2-copy! ((out-vec vec2) (vec vec2)) (:result vec2 :abbrev v2cp!)
  "Copy the components of VEC, storing the result in OUT-VEC."
  (with-vec2s ((o out-vec) (v vec))
    (psetf ox vx oy vy))
  out-vec)

(defun* vec2-copy ((vec vec2)) (:result vec2 :abbrev v2cp)
  "Copy the components of VEC, storing the result in a new vector."
  (v2cp! (v2zero) vec))

(defun* vec2-clamp! ((out-vec vec2) (vec vec2) &key
                    ((min single-float) most-negative-single-float)
                    ((max single-float) most-positive-single-float))
    (:result vec2 :abbrev v2clamp!)
  "Clamp each component of VEC within the range of [MIN, MAX], storing the result
in OUT-VEC."
  (with-vec2s ((o out-vec) (v vec))
    (psetf ox (clamp vx min max)
           oy (clamp vy min max)))
  out-vec)

(defun* vec2-clamp ((vec vec2) &key
                   ((min single-float) most-negative-single-float)
                   ((max single-float) most-positive-single-float))
    (:result vec2 :abbrev v2clamp)
  "Clamp each component of VEC within the range of [MIN, MAX], storing the result
as a new vector."
  (v2clamp! (v2zero) vec :min min :max max))

(defun* vec2-stabilize! ((out-vec vec2) (vec vec2)
                        &key ((tolerance single-float) +epsilon+))
    (:result vec2 :abbrev v2stab!)
  "Adjust any component of VEC to zero if below the epsilon TOLERANCE, storing
the result in OUT-VEC."
  (with-vec2s ((o out-vec) (v vec))
    (macrolet ((stabilize (place) `(if (< (abs ,place) tolerance) 0.0 ,place)))
      (psetf ox (stabilize vx)
             oy (stabilize vy))))
  out-vec)

(defun* vec2-stabilize ((vec vec2) &key ((tolerance single-float) +epsilon+))
    (:result vec2 :abbrev v2stab)
  "Adjust any component of VEC to zero if below the epsilon TOLERANCE, storing
the result as a new vector."
  (v2stab! (v2zero) vec :tolerance tolerance))

(defun* vec2-zero! ((vec vec2)) (:result vec2 :abbrev v2zero!)
  "Set each component of VEC to zero."
  (with-vec2 (v vec)
    (psetf vx 0.0 vy 0.0))
  vec)

(defun* vec2-zero () (:result vec2 :abbrev v2zero)
  "Create a new zero vector."
  (vec2 0 0))

(defun* vec2-to-list ((vec vec2)) (:result list :abbrev v2->list)
  "Convert VEC to a list of components."
  (with-vec2 (v vec)
    (list vx vy)))

(defun* vec2-from-list ((list list)) (:result vec2 :abbrev list->v2)
  "Convert LIST to a vector."
  (apply #'vec2 list))

(defun* vec2= ((vec-a vec2) (vec-b vec2)) (:result boolean :abbrev v2=)
  "Check if the components of VEC-A are equal to the components of VEC-B."
  (with-vec2s ((v1 vec-a) (v2 vec-b))
    (and (= v1x v2x)
         (= v1y v2y))))

(defun* vec2~ ((vec-a vec2) (vec-b vec2)
               &key ((tolerance single-float) +epsilon+))
    (:result boolean :abbrev v2~)
  "Check if the components of VEC-A are approximately equal to the components of
VEC-B, according to the epsilon TOLERANCE."
  (with-vec2s ((v1 vec-a) (v2 vec-b))
    (and (~ v1x v2x tolerance)
         (~ v1y v2y tolerance))))

(defun* vec2+! ((out-vec vec2) (vec-a vec2) (vec-b vec2))
    (:result vec2 :abbrev v2+!)
  "Vector addition of VEC-A and VEC-B, storing the result in OUT-VEC."
  (with-vec2s ((o out-vec) (v1 vec-a) (v2 vec-b))
    (psetf ox (+ v1x v2x)
           oy (+ v1y v2y)))
  out-vec)

(defun* vec2+ ((vec-a vec2) (vec-b vec2)) (:result vec2 :abbrev v2+)
  "Vector addition of VEC-A and VEC-B, storing the result as a new vector."
  (v2+! (v2zero) vec-a vec-b))

(defun* vec2-! ((out-vec vec2) (vec-a vec2) (vec-b vec2))
    (:result vec2 :abbrev v2-!)
  "Vector subtraction of VEC-A from VEC-B, storing the result in OUT-VEC."
  (with-vec2s ((o out-vec) (v1 vec-a) (v2 vec-b))
    (psetf ox (- v1x v2x)
           oy (- v1y v2y)))
  out-vec)

(defun* vec2- ((vec-a vec2) (vec-b vec2)) (:result vec2 :abbrev v2-)
  "Vector subtraction of VEC-A from VEC-B, storing the result as a new vector."
  (v2-! (v2zero) vec-a vec-b))

(defun* vec2-hadamard*! ((out-vec vec2) (vec-a vec2) (vec-b vec2))
    (:result vec2 :abbrev v2had*!)
  "Component-wise multiplication (the Hadamard product) of VEC-A and VEC-B,
storing the result in OUT-VEC."
  (with-vec2s ((o out-vec) (v1 vec-a) (v2 vec-b))
    (psetf ox (* v1x v2x)
           oy (* v1y v2y)))
  out-vec)

(defun* vec2-hadamard* ((vec-a vec2) (vec-b vec2))
    (:result vec2 :abbrev v2had*)
  "Component-wise multiplication (the Hadamard product) of VEC-A and VEC-B,
storing the result as a new vector."
  (v2had*! (v2zero) vec-a vec-b))

(defun* vec2-hadamard/! ((out-vec vec2) (vec-a vec2) (vec-b vec2))
    (:result vec2 :abbrev v2had/!)
  "Component-wise division (the Hadamard quotient) of VEC-A by VEC-B, storing the
result in OUT-VEC."
  (with-vec2s ((o out-vec) (v1 vec-a) (v2 vec-b))
    (psetf ox (if (zerop v2x) 0.0 (/ v1x v2x))
           oy (if (zerop v2y) 0.0 (/ v1y v2y))))
  out-vec)

(defun* vec2-hadamard/ ((vec-a vec2) (vec-b vec2)) (:result vec2 :abbrev v2had/)
  "Component-wise division (the Hadamard quotient) of VEC-A by VEC-B, storing the
result as a new vector."
  (v2had/! (v2zero) vec-a vec-b))

(defun* vec2-scale! ((out-vec vec2) (vec vec2) (scalar single-float))
    (:result vec2 :abbrev v2scale!)
  "Vector scalar multiplication of VEC by SCALAR, storing the result in OUT-VEC."
  (with-vec2s ((o out-vec) (v vec))
    (psetf ox (* vx scalar)
           oy (* vy scalar)))
  out-vec)

(defun* vec2-scale ((vec vec2) (scalar single-float))
    (:result vec2 :abbrev v2scale)
  "Vector scalar multiplication of VEC by SCALAR, storing the result as a new
vector."
  (v2scale! (v2zero) vec scalar))

(defun* vec2-dot ((vec-a vec2) (vec-b vec2)) (:result single-float :abbrev v2dot)
  "Compute the dot product of VEC-A and VEC-B."
  (with-vec2s ((v1 vec-a) (v2 vec-b))
    (+ (* v1x v2x) (* v1y v2y))))

(defun* vec2-magnitude-squared ((vec vec2))
  (:result single-float :abbrev v2magsq)
  "Compute the magnitude (also known as length or Euclidean norm) of VEC. This
results in a squared value, which is cheaper to compute. It is useful when you
want to compare relative vector lengths, which does not need the expensive square
root function. Use VEC2-MAGNITUDE for other cases."
  (v2dot vec vec))

(defun* vec2-magnitude ((vec vec2)) (:result single-float :abbrev v2mag)
  "Compute the magnitude (also known as length or Euclidean norm) of VEC. If you
only need to compare lengths of vectors, use VEC2-MAGNITUDE-SQUARED instead, as
it is cheaper to compute without the square root call of this function."
  (sqrt (v2magsq vec)))

(defun* vec2-normalize! ((out-vec vec2) (vec vec2))
    (:result vec2 :abbrev v2normalize!)
  "Normalize a vector so it has a magnitude of 1.0, storing the result in
OUT-VEC."
  (let ((magnitude (v2mag vec)))
    (unless (zerop magnitude)
      (v2scale! out-vec vec (/ magnitude))))
  out-vec)

(defun* vec2-normalize ((vec vec2)) (:result vec2 :abbrev v2normalize)
  "Normalize a vector so it has a magnitude of 1.0, storing the result as a new
vector."
  (v2normalize! (v2zero) vec))

(defun* vec2-round! ((out-vec vec2) (vec vec2)) (:result vec2 :abbrev v2round!)
  "Round each component of VEC to the nearest integer, storing the result in
OUT-VEC."
  (with-vec2s ((o out-vec) (v vec))
    (psetf ox (fround vx)
           oy (fround vy)))
  out-vec)

(defun* vec2-round ((vec vec2)) (:result vec2 :abbrev v2round)
  "Round each component of VEC to the nearest integer, storing the result as a
new vector."
  (v2round! (v2zero) vec))

(defun* vec2-abs! ((out-vec vec2) (vec vec2)) (:result vec2 :abbrev v2abs!)
  "Modify VEC to have the absolute value of each component, storing the result in
OUT-VEC."
  (with-vec2s ((o out-vec) (v vec))
    (psetf ox (abs vx)
           oy (abs vy)))
  out-vec)

(defun* vec2-abs ((vec vec2)) (:result vec2 :abbrev v2abs)
  "Modify VEC to have the absolute value of each component, storing the result as
a new vector."
  (v2abs! (v2zero) vec))

(defun* vec2-negate! ((out-vec vec2) (vec vec2)) (:result vec2 :abbrev v2neg!)
  "Negate each component of VEC, storing the result in OUT-VEC."
  (v2scale! out-vec vec -1.0))

(defun* vec2-negate ((vec vec2)) (:result vec2 :abbrev v2neg)
  "Negate each component of VEC, storing the result as a new vector."
  (v2neg! (v2zero) vec))

(defun* vec2-angle ((vec-a vec2) (vec-b vec2))
    (:result single-float :abbrev v2angle)
  "Compute the angle in radians between VEC-A and VEC-B."
  (let ((dot (v2dot vec-a vec-b))
        (m*m (* (v2mag vec-a) (v2mag vec-b))))
    (if (zerop m*m) 0.0 (acos (/ dot m*m)))))

(defun* vec2-zero-p ((vec vec2)) (:result boolean :abbrev v2zerop)
  "Check if all components of VEC are zero."
  (with-vec2 (v vec)
    (and (zerop vx)
         (zerop vy))))

(defun* vec2-direction= ((vec-a vec2) (vec-b vec2))
    (:result boolean :abbrev v2dir=)
  "Check if the directions of VEC-A and VEC-B are the same."
  (>= (v2dot (v2normalize vec-a) (v2normalize vec-b)) (- 1 +epsilon+)))

(defun* vec2-lerp! ((out-vec vec2) (vec-a vec2) (vec-b vec2)
                    (factor single-float))
    (:result vec2 :abbrev v2lerp!)
  "Perform a linear interpolation between VEC-A and VEC-B by the interpolation
factor FACTOR, storing the result in OUT-VEC."
  (with-vec2s ((o out-vec) (v1 vec-a) (v2 vec-b))
    (psetf ox (lerp factor v1x v2x)
           oy (lerp factor v1y v2y)))
  out-vec)

(defun* vec2-lerp ((vec-a vec2) (vec-b vec2) (factor single-float))
    (:result vec2 :abbrev v2lerp)
  "Perform a linear interpolation between VEC-A and VEC-B by the interpolation
factor FACTOR, storing the result as a new vector."
  (v2lerp! (v2zero) vec-a vec-b factor))

(defun* vec2< ((vec-a vec2) (vec-b vec2)) (:result boolean :abbrev v2<)
  "Check if each component of VEC-A is less than that component of VEC-B."
  (with-vec2s ((v1 vec-a) (v2 vec-b))
    (and (< v1x v2x)
         (< v1y v2y))))

(defun* vec2<= ((vec-a vec2) (vec-b vec2)) (:result boolean :abbrev v2<=)
  "Check if each component of VEC-A is less than or equal to that component of
VEC-B."
  (with-vec2s ((v1 vec-a) (v2 vec-b))
    (and (<= v1x v2x)
         (<= v1y v2y))))

(defun* vec2> ((vec-a vec2) (vec-b vec2)) (:result boolean :abbrev v2>)
  "Check if each component of VEC-A is greater than that component of VEC-B."
  (with-vec2s ((v1 vec-a) (v2 vec-b))
    (and (> v1x v2x)
         (> v1y v2y))))

(defun* vec2>= ((vec-a vec2) (vec-b vec2)) (:result boolean :abbrev v2>=)
  "Check if each component of VEC-A is greater than or equal to that component of
VEC-B."
  (with-vec2s ((v1 vec-a) (v2 vec-b))
    (and (>= v1x v2x)
         (>= v1y v2y))))

(defun* vec2-min! ((out-vec vec2) (vec-a vec2) (vec-b vec2))
    (:result vec2 :abbrev v2min!)
  "Component-wise minimum of VEC-A and VEC-B, storing the result in OUT-VEC."
  (with-vec2s ((o out-vec) (v1 vec-a) (v2 vec-b))
    (psetf ox (min v1x v2x)
           oy (min v1y v2y)))
  out-vec)

(defun* vec2-min ((vec-a vec2) (vec-b vec2)) (:result vec2 :abbrev v2min)
  "Component-wise minimum of VEC-A and VEC-B, storing the result as a new
vector."
  (v2min! (v2zero) vec-a vec-b))

(defun* vec2-max! ((out-vec vec2) (vec-a vec2) (vec-b vec2))
    (:result vec2 :abbrev v2max!)
  "Component-wise maximum of VEC-A and VEC-B, storing the result in OUT-VEC."
  (with-vec2s ((o out-vec) (v1 vec-a) (v2 vec-b))
    (psetf ox (max v1x v2x)
           oy (max v1y v2y)))
  out-vec)

(defun* vec2-max ((vec-a vec2) (vec-b vec2)) (:result vec2 :abbrev v2max)
  "Component-wise maximum of VEC-A and VEC-B, storing the result as a new
vector."
  (v2max! (v2zero) vec-a vec-b))
