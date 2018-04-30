(in-package :box.math.vec4f)

;;; Structure

(deftype vec () '(simple-array single-float (4)))

(defstruct (vec (:type (vector single-float))
                (:constructor %make (x y z w))
                (:conc-name nil)
                (:copier nil))
  "A Euclidean vector of 4 single-float components."
  (x 0.0f0 :type single-float)
  (y 0.0f0 :type single-float)
  (z 0.0f0 :type single-float)
  (w 0.0f0 :type single-float))

(defmacro with-components (((prefix vec) &rest rest) &body body)
  "A convenience macro for concisely accessing the components of vectors."
  `(with-accessors ((,prefix identity)
                    (,(box.math.common::%make-accessor-symbol prefix 'x) x)
                    (,(box.math.common::%make-accessor-symbol prefix 'y) y)
                    (,(box.math.common::%make-accessor-symbol prefix 'z) z)
                    (,(box.math.common::%make-accessor-symbol prefix 'w) w))
       ,vec
     ,(if rest
          `(with-components ,rest ,@body)
          `(progn ,@body))))

;;; Constants

(au:define-constant +zero+
    (make-array 4 :element-type 'single-float :initial-contents '(0.0f0 0.0f0 0.0f0 0.0f0))
  :test #'equalp
  :documentation "A vector with each component as zero.")

;;; Swizzling

(box.math.common::%generate-swizzle-functions 4)

;;; Operations

(declaim (inline vec))
(declaim (ftype (function (real real real real) vec) make))
(defun make (x y z w)
  "Create a new vector."
  (%make (float x 1.0f0) (float y 1.0f0) (float z 1.0f0) (float w 1.0f0)))

(declaim (inline zero!))
(declaim (ftype (function (vec) vec) zero!))
(defun zero! (vec)
  "Set each component of VEC to zero."
  (with-components ((v vec))
    (psetf vx 0.0f0 vy 0.0f0 vz 0.0f0 vw 0.0f0))
  vec)

(declaim (inline zero))
(declaim (ftype (function () vec) zero))
(defun zero ()
  "Create a new vector with all components initialized to zero."
  (make 0 0 0 0))

(declaim (inline one!))
(declaim (ftype (function (vec) vec) one!))
(defun one! (vec)
  "Set each component of VEC to one."
  (with-components ((v vec))
    (psetf vx 1.0 vy 1.0 vz 1.0 vw 1.0))
  vec)

(declaim (inline one))
(declaim (ftype (function () vec) one))
(defun one ()
  "Create a new vector with all components initialized to one."
  (make 1 1 1 1))

(declaim (inline zerop))
(declaim (ftype (function (vec) boolean) zerop))
(defun zerop (vec)
  "Check if each component of VEC is zero."
  (with-components ((v vec))
    (and (cl:zerop vx)
         (cl:zerop vy)
         (cl:zerop vz)
         (cl:zerop vw))))

(declaim (inline copy!))
(declaim (ftype (function (vec vec) vec) copy!))
(defun copy! (out vec)
  "Copy each component of VEC to the existing vector, OUT."
  (with-components ((o out) (v vec))
    (psetf ox vx oy vy oz vz ow vw))
  out)

(declaim (inline copy))
(declaim (ftype (function (vec) vec) copy))
(defun copy (vec)
  "Copy each component of VEC to a freshly allocated vector."
  (copy! (zero) vec))

(declaim (inline clamp!))
(declaim (ftype (function (vec vec &key (:min single-float) (:max single-float)) vec) clamp!))
(defun clamp! (out vec &key (min most-negative-single-float) (max most-positive-single-float))
  "Clamp each component of VEC within the range of [MIN, MAX], storing the result in the existing
vector, OUT."
  (with-components ((o out) (v vec))
    (psetf ox (au:clamp vx min max)
           oy (au:clamp vy min max)
           oz (au:clamp vz min max)
           ow (au:clamp vw min max)))
  out)

(declaim (inline clamp))
(declaim (ftype (function (vec &key (:min single-float) (:max single-float)) vec) clamp))
(defun clamp (vec &key (min most-negative-single-float) (max most-positive-single-float))
  "Clamp each component of VEC within the range of [MIN, MAX], storing the result in a freshly
allocated vector."
  (clamp! (zero) vec :min min :max max))

(declaim (inline stabilize!))
(declaim (ftype (function (vec vec &key (:tolerance single-float)) vec) stabilize!))
(defun stabilize! (out vec &key (tolerance +epsilon+))
  "Adjust each component of VEC to zero if it's below TOLERANCE, storing the result in the existing
vector, OUT."
  (with-components ((o out) (v vec))
    (macrolet ((stabilize (place)
                 `(if (cl:< (cl:abs ,place) tolerance) 0.0f0 ,place)))
      (psetf ox (stabilize vx)
             oy (stabilize vy)
             oz (stabilize vz)
             ow (stabilize vw))))
  out)

(declaim (inline stabilize))
(declaim (ftype (function (vec &key (:tolerance single-float)) vec) stabilize))
(defun stabilize (vec &key (tolerance +epsilon+))
  "Adjust each component of VEC to zero if it's below TOLERANCE, storing the result in a freshly
allocated vector."
  (stabilize! (zero) vec :tolerance tolerance))

(declaim (inline to-list))
(declaim (ftype (function (vec) list) to-list))
(defun to-list (vec)
  "Convert VEC to a list of its components."
  (with-components ((v vec))
    (list vx vy vz vw)))

(declaim (inline from-list))
(declaim (ftype (function (list) vec) from-list))
(defun from-list (list)
  "Create a vector from a list of components."
  (apply #'make list))

(declaim (inline =))
(declaim (ftype (function (vec vec) boolean) =))
(defun = (vec1 vec2)
  "Check if all components of VEC1 are numerically equal to the components of VEC2."
  (with-components ((v1 vec1) (v2 vec2))
    (and (cl:= v1x v2x)
         (cl:= v1y v2y)
         (cl:= v1z v2z)
         (cl:= v1w v2w))))

(declaim (inline ~))
(declaim (ftype (function (vec vec &key (:tolerance single-float)) boolean) ~))
(defun ~ (vec1 vec2 &key (tolerance +epsilon+))
  "Check if all components of VEC1 are approximately equal to the components of VEC2, according to
TOLERANCE."
  (with-components ((v1 vec1) (v2 vec2))
    (and (box.math.common::%~ v1x v2x tolerance)
         (box.math.common::%~ v1y v2y tolerance)
         (box.math.common::%~ v1z v2z tolerance)
         (box.math.common::%~ v1w v2w tolerance))))

(declaim (inline +!))
(declaim (ftype (function (vec vec vec) vec) +!))
(defun +! (out vec1 vec2)
  "Calculate the sum of VEC1 and VEC2, storing the result in the existing vector, OUT."
  (with-components ((o out) (v1 vec1) (v2 vec2))
    (psetf ox (cl:+ v1x v2x)
           oy (cl:+ v1y v2y)
           oz (cl:+ v1z v2z)
           ow (cl:+ v1w v2w)))
  out)

(declaim (inline +))
(declaim (ftype (function (vec vec) vec) +))
(defun + (vec1 vec2)
  "Calculate the sum of VEC1 and VEC2, storing the result in a freshly allocated vector."
  (+! (zero) vec1 vec2))

(declaim (inline -!))
(declaim (ftype (function (vec vec vec) vec) -!))
(defun -! (out vec1 vec2)
  "Calculate the difference of VEC2 from VEC1, storing the result in the existing vector, OUT."
  (with-components ((o out) (v1 vec1) (v2 vec2))
    (psetf ox (cl:- v1x v2x)
           oy (cl:- v1y v2y)
           oz (cl:- v1z v2z)
           ow (cl:- v1w v2w)))
  out)

(declaim (inline -))
(declaim (ftype (function (vec vec) vec) -))
(defun - (vec1 vec2)
  "Calculate the difference of VEC2 from VEC1, storing the result in a freshly allocated vector."
  (-! (zero) vec1 vec2))

(declaim (inline *!))
(declaim (ftype (function (vec vec vec) vec) *!))
(defun *! (out vec1 vec2)
  "Calculate the Hadamard (component-wise) product of VEC1 and VEC2, storing the result in the
existing vector, OUT."
  (with-components ((o out) (v1 vec1) (v2 vec2))
    (psetf ox (cl:* v1x v2x)
           oy (cl:* v1y v2y)
           oz (cl:* v1z v2z)
           ow (cl:* v1w v2w)))
  out)

(declaim (inline *))
(declaim (ftype (function (vec vec) vec) *))
(defun * (vec1 vec2)
  "Calculate the Hadamard (component-wise) product of VEC1 and VEC2, storing the result in a freshly
allocated vector."
  (*! (zero) vec1 vec2))

(declaim (inline /!))
(declaim (ftype (function (vec vec vec) vec) /!))
(defun /! (out vec1 vec2)
  "Calculate the Hadamard (component-wise) quotient of VEC1 by VEC2, storing the result in the
existing vector, OUT."
  (with-components ((o out) (v1 vec1) (v2 vec2))
    (psetf ox (if (cl:zerop v2x) 0.0f0 (cl:/ v1x v2x))
           oy (if (cl:zerop v2y) 0.0f0 (cl:/ v1y v2y))
           oz (if (cl:zerop v2z) 0.0f0 (cl:/ v1z v2z))
           ow (if (cl:zerop v2w) 0.0f0 (cl:/ v1w v2w))))
  out)

(declaim (inline /))
(declaim (ftype (function (vec vec) vec) /))
(defun / (vec1 vec2)
  "Calculate the Hadamard (component-wise) quotient of VEC1 by VEC2, storing the result in a freshly
allocated vector."
  (/! (zero) vec1 vec2))

(declaim (inline scale!))
(declaim (ftype (function (vec vec single-float) vec) scale!))
(defun scale! (out vec scalar)
  "Scale VEC by SCALAR, storing the result in the existing vector, OUT."
  (with-components ((o out) (v vec))
    (psetf ox (cl:* vx scalar)
           oy (cl:* vy scalar)
           oz (cl:* vz scalar)
           ow (cl:* vw scalar)))
  out)

(declaim (inline scale))
(declaim (ftype (function (vec single-float) vec) scale))
(defun scale (vec scalar)
  "Scale VEC by SCALAR, storing the result in a freshly allocated vector."
  (scale! (zero) vec scalar))

(declaim (inline dot))
(declaim (ftype (function (vec vec) single-float) dot))
(defun dot (vec1 vec2)
  "Calculate the dot product of VEC1 and VEC2. Returns a scalar."
  (with-components ((v1 vec1) (v2 vec2))
    (cl:+ (cl:* v1x v2x) (cl:* v1y v2y) (cl:* v1z v2z) (cl:* v1w v2w))))

(declaim (inline magnitude-squared))
(declaim (ftype (function (vec) single-float) magnitude-squared))
(defun magnitude-squared (vec)
  "Calculate the magnitude (also known as length or Euclidean norm) of VEC. This results in a
squared value, which is cheaper to compute. It is useful when you want to compare relative lengths,
which does not need the expensive square root function.

See MAGNITUDE for other cases."
  (dot vec vec))

(declaim (inline magnitude))
(declaim (ftype (function (vec) single-float) magnitude))
(defun magnitude (vec)
  "Compute the magnitude (also known as length or Euclidean norm) of VEC.

See MAGNITUDE-SQUARED if you only need to compare lengths, as it is cheaper to compute without the
square root call of this function."
  (sqrt (magnitude-squared vec)))

(declaim (inline normalize!))
(declaim (ftype (function (vec vec) vec) normalize!))
(defun normalize! (out vec)
  "Convert VEC to be of unit length, storing the result in the existing vector, OUT."
  (let ((magnitude (magnitude vec)))
    (unless (cl:zerop magnitude)
      (scale! out vec (cl:/ magnitude))))
  out)

(declaim (inline normalize))
(declaim (ftype (function (vec) vec) normalize))
(defun normalize (vec)
  "Convert VEC to be of unit length, storing the result in a freshly allocated vector."
  (normalize! (zero) vec))

(declaim (inline round!))
(declaim (ftype (function (vec vec) vec) round!))
(defun round! (out vec)
  "Round each component of VEC to the nearest integer, storing the result in the existing vector,
OUT."
  (with-components ((o out) (v vec))
    (psetf ox (fround vx)
           oy (fround vy)
           oz (fround vz)
           ow (fround vw)))
  out)

(declaim (inline round))
(declaim (ftype (function (vec) vec) round))
(defun round (vec)
  "Round each component of VEC to the nearest integer, storing the result in a freshly allocated
vector."
  (round! (zero) vec))

(declaim (inline abs!))
(declaim (ftype (function (vec vec) vec) abs!))
(defun abs! (out vec)
  "Convert each component of VEC to its absolute value, storing the result in the existing vector,
OUT."
  (with-components ((o out) (v vec))
    (psetf ox (cl:abs vx)
           oy (cl:abs vy)
           oz (cl:abs vz)
           ow (cl:abs vw)))
  out)

(declaim (inline abs))
(declaim (ftype (function (vec) vec) abs))
(defun abs (vec)
  "Convert each component of VEC to its absolute value, storing the result in a freshly allocated
vector."
  (abs! (zero) vec))

(declaim (inline negate!))
(declaim (ftype (function (vec vec) vec) negate!))
(defun negate! (out vec)
  "Negate each component of VEC, storing the result in the existing vector, OUT."
  (scale! out vec -1.0f0))

(declaim (inline negate))
(declaim (ftype (function (vec) vec) negate))
(defun negate (vec)
  "Negate each component of VEC, storing the result in a freshly allocated vector."
  (negate! (zero) vec))

(declaim (inline angle))
(declaim (ftype (function (vec vec) single-float) angle))
(defun angle (vec1 vec2)
  "Calculate the angle in radians between VEC1 and VEC2."
  (let ((dot (dot vec1 vec2))
        (m*m (cl:* (magnitude vec1) (magnitude vec2))))
    (if (cl:zerop m*m) 0.0f0 (acos (cl:/ dot m*m)))))

(declaim (inline lerp!))
(declaim (ftype (function (vec vec vec single-float) vec) lerp!))
(defun lerp! (out vec1 vec2 factor)
  "Linearly interpolate between VEC1 and VEC2 by FACTOR, storing the result in the existing vector,
OUT."
  (with-components ((o out) (v1 vec1) (v2 vec2))
    (psetf ox (au:lerp factor v1x v2x)
           oy (au:lerp factor v1y v2y)
           oz (au:lerp factor v1z v2z)
           ow (au:lerp factor v1w v2w)))
  out)

(declaim (inline lerp))
(declaim (ftype (function (vec vec single-float) vec) lerp))
(defun lerp (vec1 vec2 factor)
  "Linearly interpolate between VEC1 and VEC2 by FACTOR, storing the result in a freshly allocated
vector."
  (lerp! (zero) vec1 vec2 factor))

(declaim (inline <))
(declaim (ftype (function (vec vec) boolean) <))
(defun < (vec1 vec2)
  "Check if each component of VEC1 is less than the same component of VEC2."
  (with-components ((v1 vec1) (v2 vec2))
    (and (cl:< v1x v2x)
         (cl:< v1y v2y)
         (cl:< v1z v2z)
         (cl:< v1w v2w))))

(declaim (inline <=))
(declaim (ftype (function (vec vec) boolean) <=))
(defun <= (vec1 vec2)
  "Check if each component of VEC1 is less than or equal to the same component of VEC2."
  (with-components ((v1 vec1) (v2 vec2))
    (and (cl:<= v1x v2x)
         (cl:<= v1y v2y)
         (cl:<= v1z v2z)
         (cl:<= v1w v2w))))

(declaim (inline >))
(declaim (ftype (function (vec vec) boolean) >))
(defun > (vec1 vec2)
  "Check if each component of VEC1 is greater than the same component of VEC2."
  (with-components ((v1 vec1) (v2 vec2))
    (and (cl:> v1x v2x)
         (cl:> v1y v2y)
         (cl:> v1z v2z)
         (cl:> v1w v2w))))

(declaim (inline >=))
(declaim (ftype (function (vec vec) boolean) >=))
(defun >= (vec1 vec2)
  "Check if each component of VEC1 is greater than or equal to the same component of VEC2."
  (with-components ((v1 vec1) (v2 vec2))
    (and (cl:>= v1x v2x)
         (cl:>= v1y v2y)
         (cl:>= v1z v2z)
         (cl:>= v1w v2w))))

(declaim (inline min!))
(declaim (ftype (function (vec vec vec) vec) min!))
(defun min! (out vec1 vec2)
  "Return the minimum of each component in VEC1 and VEC2, into the existing vector, OUT."
  (with-components ((o out) (v1 vec1) (v2 vec2))
    (psetf ox (cl:min v1x v2x)
           oy (cl:min v1y v2y)
           oz (cl:min v1z v2z)
           ow (cl:min v1w v2w)))
  out)

(declaim (inline min))
(declaim (ftype (function (vec vec) vec) min))
(defun min (vec1 vec2)
  "Return the minimum each component in VEC1 and VEC2, into a freshly allocated vector."
  (min! (zero) vec1 vec2))

(declaim (inline max!))
(declaim (ftype (function (vec vec vec) vec) max!))
(defun max! (out vec1 vec2)
  "Return the maximum of each component in VEC1 and VEC2, into the existing vector, OUT."
  (with-components ((o out) (v1 vec1) (v2 vec2))
    (psetf ox (cl:max v1x v2x)
           oy (cl:max v1y v2y)
           oz (cl:max v1z v2z)
           ow (cl:max v1w v2w)))
  out)

(declaim (inline max))
(declaim (ftype (function (vec vec) vec) max))
(defun max (vec1 vec2)
  "Return the maximum of each component in VEC1 and VEC2, into a freshly allocated vector."
  (max! (zero) vec1 vec2))
