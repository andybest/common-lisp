(in-package #:cl-user)

(defpackage #:origin.vec4
  (:local-nicknames (#:% #:origin.internal))
  (:use #:cl)
  (:shadow
   #:=
   #:+
   #:-
   #:*
   #:/
   #:random
   #:length
   #:round
   #:abs
   #:<
   #:<=
   #:>
   #:>=
   #:min
   #:max)
  (:export
   #:vec
   #:x
   #:y
   #:z
   #:w
   #:with-vectors
   #:make
   #:+zero+
   #:zero!
   #:zero
   #:one!
   #:one
   #:zero-p
   #:random!
   #:random
   #:copy!
   #:copy
   #:sign!
   #:sign
   #:fract!
   #:fract
   #:clamp!
   #:clamp
   #:to-list
   #:from-list
   #:=
   #:~
   #:+!
   #:+
   #:-!
   #:-
   #:*!
   #:*
   #:/!
   #:/
   #:scale!
   #:scale
   #:dot
   #:length-squared
   #:length
   #:distance-squared
   #:distance
   #:normalize!
   #:normalize
   #:round!
   #:round
   #:abs!
   #:abs
   #:negate!
   #:negate
   #:angle
   #:lerp!
   #:lerp
   #:<
   #:<=
   #:>
   #:>=
   #:min!
   #:min
   #:max!
   #:max
   #:radians!
   #:radians
   #:degrees!
   #:degrees))


(in-package #:origin.vec4)

;;; Structure

(deftype vec () '(simple-array single-float (4)))

(defstruct (vec (:type (vector single-float))
                (:constructor %make (x y z w))
                (:conc-name nil)
                (:copier nil))
  "A Euclidean vector of 4 single-float vectors."
  (x 0f0 :type single-float)
  (y 0f0 :type single-float)
  (z 0f0 :type single-float)
  (w 0f0 :type single-float))

(defmacro with-vectors (((prefix vec) &rest rest) &body body)
  "A convenience macro for concisely accessing the vectors of vectors."
  `(with-accessors ((,prefix identity)
                    (,(%::make-accessor-symbol prefix 'x) x)
                    (,(%::make-accessor-symbol prefix 'y) y)
                    (,(%::make-accessor-symbol prefix 'z) z)
                    (,(%::make-accessor-symbol prefix 'w) w)
                    (,(%::make-accessor-symbol prefix 'r) x)
                    (,(%::make-accessor-symbol prefix 'g) y)
                    (,(%::make-accessor-symbol prefix 'b) z)
                    (,(%::make-accessor-symbol prefix 'a) w)
                    (,(%::make-accessor-symbol prefix 's) x)
                    (,(%::make-accessor-symbol prefix 't) y)
                    (,(%::make-accessor-symbol prefix 'p) z)
                    (,(%::make-accessor-symbol prefix 'q) w))
       ,vec
     ,(if rest
          `(with-vectors ,rest ,@body)
          `(progn ,@body))))

;;; Constants

(au:define-constant +zero+
    (make-array 4 :element-type 'single-float
                  :initial-contents '(0f0 0f0 0f0 0f0))
  :test #'equalp
  :documentation "A vector with each component as zero.")

;;; Operations

(declaim (inline vec))
(declaim (ftype (function (real real real real) vec) make))
(defun make (x y z w)
  "Create a new vector."
  (%make (float x 1f0) (float y 1f0) (float z 1f0) (float w 1f0)))

(declaim (inline zero!))
(declaim (ftype (function (vec) vec) zero!))
(defun zero! (vec)
  "Set each component of VEC to zero."
  (with-vectors ((v vec))
    (psetf vx 0f0 vy 0f0 vz 0f0 vw 0f0))
  vec)

(declaim (inline zero))
(declaim (ftype (function () vec) zero))
(defun zero ()
  "Create a new vector with all vectors initialized to zero."
  (make 0f0 0f0 0f0 0f0))

(declaim (inline one!))
(declaim (ftype (function (vec) vec) one!))
(defun one! (vec)
  "Set each component of VEC to one."
  (with-vectors ((v vec))
    (psetf vx 1f0 vy 1f0 vz 1f0 vw 1f0))
  vec)

(declaim (inline one))
(declaim (ftype (function () vec) one))
(defun one ()
  "Create a new vector with all vectors initialized to one."
  (one! (zero)))

(declaim (inline zero-p))
(declaim (ftype (function (vec) boolean) zero-p))
(defun zero-p (vec)
  "Check if each component of VEC is zero."
  (with-vectors ((v vec))
    (and (zerop vx)
         (zerop vy)
         (zerop vz)
         (zerop vw))))

(declaim (inline random!))
(declaim (ftype (function (vec &key (:min real) (:max real)) vec) random!))
(defun random! (out &key (min 0.0) (max 1.0))
  (with-vectors ((o out))
    (psetf ox (cl:+ min (cl:random (cl:- max min)))
           oy (cl:+ min (cl:random (cl:- max min)))
           oz (cl:+ min (cl:random (cl:- max min)))
           ow (cl:+ min (cl:random (cl:- max min)))))
  out)

(declaim (inline random))
(declaim (ftype (function (&key (:min real) (:max real)) vec) random))
(defun random (&key (min 0.0) (max 1.0))
  (random! (zero) :min min :max max))

(declaim (inline copy!))
(declaim (ftype (function (vec vec) vec) copy!))
(defun copy! (out vec)
  "Copy each component of VEC to the existing vector, OUT."
  (with-vectors ((o out)
                 (v vec))
    (psetf ox vx oy vy oz vz ow vw))
  out)

(declaim (inline copy))
(declaim (ftype (function (vec) vec) copy))
(defun copy (vec)
  "Copy each component of VEC to a freshly allocated vector."
  (copy! (zero) vec))

(declaim (inline sign!))
(declaim (ftype (function (vec vec) vec) sign!))
(defun sign! (out vec)
  (with-vectors ((o out)
                 (v vec))
    (psetf ox (signum vx)
           oy (signum vy)
           oz (signum vz)
           ow (signum vw)))
  out)

(declaim (inline sign))
(declaim (ftype (function (vec) vec) sign))
(defun sign (vec)
  (sign! (zero) vec))

(declaim (inline fract!))
(declaim (ftype (function (vec vec) vec) fract!))
(defun fract! (out vec)
  (with-vectors ((o out)
                 (v vec))
    (psetf ox (cl:- vx (cl:floor vx))
           oy (cl:- vy (cl:floor vy))
           oz (cl:- vz (cl:floor vz))
           ow (cl:- vw (cl:floor vw))))
  out)

(declaim (inline fract))
(declaim (ftype (function (vec) vec) fract))
(defun fract (vec)
  (fract! (zero) vec))

(declaim (inline clamp!))
(declaim (ftype (function (vec vec &key (:min single-float) (:max single-float))
                          vec)
                clamp!))
(defun clamp! (out vec
               &key (min most-negative-single-float)
                 (max most-positive-single-float))
  "Clamp each component of VEC within the range of [MIN, MAX], storing the
result in the existing vector, OUT."
  (with-vectors ((o out)
                 (v vec))
    (psetf ox (au:clamp vx min max)
           oy (au:clamp vy min max)
           oz (au:clamp vz min max)
           ow (au:clamp vw min max)))
  out)

(declaim (inline clamp))
(declaim (ftype (function (vec &key (:min single-float) (:max single-float))
                          vec)
                clamp))
(defun clamp (vec
              &key (min most-negative-single-float)
                (max most-positive-single-float))
  "Clamp each component of VEC within the range of [MIN, MAX], storing the
result in a freshly allocated vector."
  (clamp! (zero) vec :min min :max max))

(declaim (inline to-list))
(declaim (ftype (function (vec) list) to-list))
(defun to-list (vec)
  "Convert VEC to a list of its vectors."
  (with-vectors ((v vec))
    (list vx vy vz vw)))

(declaim (inline from-list))
(declaim (ftype (function (list) vec) from-list))
(defun from-list (list)
  "Create a vector from a list of vectors."
  (apply #'make list))

(declaim (inline =))
(declaim (ftype (function (vec vec) boolean) =))
(defun = (vec1 vec2)
  "Check if all vectors of VEC1 are numerically equal to the vectors of
VEC2."
  (with-vectors ((v1 vec1)
                 (v2 vec2))
    (and (cl:= v1x v2x)
         (cl:= v1y v2y)
         (cl:= v1z v2z)
         (cl:= v1w v2w))))

(declaim (inline ~))
(declaim (ftype (function (vec vec &key (:tolerance single-float)) boolean) ~))
(defun ~ (vec1 vec2 &key (tolerance 1e-7))
  "Check if all vectors of VEC1 are approximately equal to the vectors of
VEC2, according to TOLERANCE."
  (with-vectors ((v1 vec1)
                 (v2 vec2))
    (and (%::~ v1x v2x tolerance)
         (%::~ v1y v2y tolerance)
         (%::~ v1z v2z tolerance)
         (%::~ v1w v2w tolerance))))

(declaim (inline +!))
(declaim (ftype (function (vec vec vec) vec) +!))
(defun +! (out vec1 vec2)
  "Calculate the sum of VEC1 and VEC2, storing the result in the existing
vector, OUT."
  (declare (optimize speed (space 0) (debug 0)))
  (with-vectors ((o out)
                 (v1 vec1)
                 (v2 vec2))
    (psetf ox (cl:+ v1x v2x)
           oy (cl:+ v1y v2y)
           oz (cl:+ v1z v2z)
           ow (cl:+ v1w v2w)))
  out)

(declaim (inline +))
(declaim (ftype (function (vec vec) vec) +))
(defun + (vec1 vec2)
  "Calculate the sum of VEC1 and VEC2, storing the result in a freshly allocated
vector."
  (+! (zero) vec1 vec2))

(declaim (inline -!))
(declaim (ftype (function (vec vec vec) vec) -!))
(defun -! (out vec1 vec2)
  "Calculate the difference of VEC2 from VEC1, storing the result in the
existing vector, OUT."
  (with-vectors ((o out)
                 (v1 vec1)
                 (v2 vec2))
    (psetf ox (cl:- v1x v2x)
           oy (cl:- v1y v2y)
           oz (cl:- v1z v2z)
           ow (cl:- v1w v2w)))
  out)

(declaim (inline -))
(declaim (ftype (function (vec vec) vec) -))
(defun - (vec1 vec2)
  "Calculate the difference of VEC2 from VEC1, storing the result in a freshly
allocated vector."
  (-! (zero) vec1 vec2))

(declaim (inline *!))
(declaim (ftype (function (vec vec vec) vec) *!))
(defun *! (out vec1 vec2)
  "Calculate the Hadamard (component-wise) product of VEC1 and VEC2, storing the
result in the existing vector, OUT."
  (declare (optimize speed (space 0) (debug 0) (safety 0)))
  (with-vectors ((o out)
                 (v1 vec1)
                 (v2 vec2))
    (psetf ox (cl:* v1x v2x)
           oy (cl:* v1y v2y)
           oz (cl:* v1z v2z)
           ow (cl:* v1w v2w)))
  out)

(declaim (inline *))
(declaim (ftype (function (vec vec) vec) *))
(defun * (vec1 vec2)
  "Calculate the Hadamard (component-wise) product of VEC1 and VEC2, storing the
result in a freshly allocated vector."
  (*! (zero) vec1 vec2))

(declaim (inline /!))
(declaim (ftype (function (vec vec vec) vec) /!))
(defun /! (out vec1 vec2)
  "Calculate the Hadamard (component-wise) quotient of VEC1 by VEC2, storing the
result in the existing vector, OUT."
  (with-vectors ((o out)
                 (v1 vec1)
                 (v2 vec2))
    (psetf ox (if (zerop v2x) 0f0 (cl:/ v1x v2x))
           oy (if (zerop v2y) 0f0 (cl:/ v1y v2y))
           oz (if (zerop v2z) 0f0 (cl:/ v1z v2z))
           ow (if (zerop v2w) 0f0 (cl:/ v1w v2w))))
  out)

(declaim (inline /))
(declaim (ftype (function (vec vec) vec) /))
(defun / (vec1 vec2)
  "Calculate the Hadamard (component-wise) quotient of VEC1 by VEC2, storing the
result in a freshly allocated vector."
  (/! (zero) vec1 vec2))

(declaim (inline scale!))
(declaim (ftype (function (vec vec single-float) vec) scale!))
(defun scale! (out vec scalar)
  "Scale VEC by SCALAR, storing the result in the existing vector, OUT."
  (with-vectors ((o out)
                 (v vec))
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
  (with-vectors ((v1 vec1)
                 (v2 vec2))
    (cl:+ (cl:* v1x v2x) (cl:* v1y v2y) (cl:* v1z v2z) (cl:* v1w v2w))))

(declaim (inline length-squared))
(declaim (ftype (function (vec) single-float) length-squared))
(defun length-squared (vec)
  "Calculate the magnitude (also known as length or Euclidean norm) of VEC. This
results in a squared value, which is cheaper to compute. It is useful when you
want to compare relative lengths, which does not need the expensive square root
function.

See LENGTH for other cases."
  (dot vec vec))

(declaim (inline length))
(declaim (ftype (function (vec) single-float) length))
(defun length (vec)
  "Compute the magnitude (also known as length or Euclidean norm) of VEC.

See LENGTH-SQUARED if you only need to compare lengths, as it is cheaper to
compute without the square root call of this function."
  (sqrt (length-squared vec)))

(declaim (inline distance-squared))
(declaim (ftype (function (vec vec) single-float) distance-squared))
(defun distance-squared (vec1 vec2)
  (length-squared (- vec2 vec1)))

(declaim (inline distance))
(declaim (ftype (function (vec vec) single-float) distance))
(defun distance (vec1 vec2)
  (sqrt (distance-squared vec1 vec2)))

(declaim (inline normalize!))
(declaim (ftype (function (vec vec) vec) normalize!))
(defun normalize! (out vec)
  "Convert VEC to be of unit length, storing the result in the existing vector,
OUT."
  (let ((length (length vec)))
    (unless (zerop length)
      (scale! out vec (cl:/ length))))
  out)

(declaim (inline normalize))
(declaim (ftype (function (vec) vec) normalize))
(defun normalize (vec)
  "Convert VEC to be of unit length, storing the result in a freshly allocated
vector."
  (normalize! (zero) vec))

(declaim (inline round!))
(declaim (ftype (function (vec vec) vec) round!))
(defun round! (out vec)
  "Round each component of VEC to the nearest integer, storing the result in the
existing vector, OUT."
  (with-vectors ((o out)
                 (v vec))
    (psetf ox (fround vx)
           oy (fround vy)
           oz (fround vz)
           ow (fround vw)))
  out)

(declaim (inline round))
(declaim (ftype (function (vec) vec) round))
(defun round (vec)
  "Round each component of VEC to the nearest integer, storing the result in a
freshly allocated vector."
  (round! (zero) vec))

(declaim (inline abs!))
(declaim (ftype (function (vec vec) vec) abs!))
(defun abs! (out vec)
  "Convert each component of VEC to its absolute value, storing the result in
the existing vector, OUT."
  (with-vectors ((o out)
                 (v vec))
    (psetf ox (cl:abs vx)
           oy (cl:abs vy)
           oz (cl:abs vz)
           ow (cl:abs vw)))
  out)

(declaim (inline abs))
(declaim (ftype (function (vec) vec) abs))
(defun abs (vec)
  "Convert each component of VEC to its absolute value, storing the result in a
freshly allocated vector."
  (abs! (zero) vec))

(declaim (inline negate!))
(declaim (ftype (function (vec vec) vec) negate!))
(defun negate! (out vec)
  "Negate each component of VEC, storing the result in the existing vector,
OUT."
  (scale! out vec -1f0))

(declaim (inline negate))
(declaim (ftype (function (vec) vec) negate))
(defun negate (vec)
  "Negate each component of VEC, storing the result in a freshly allocated
vector."
  (negate! (zero) vec))

(declaim (inline angle))
(declaim (ftype (function (vec vec) single-float) angle))
(defun angle (vec1 vec2)
  "Calculate the angle in radians between VEC1 and VEC2."
  (let ((dot (dot vec1 vec2))
        (m*m (cl:* (length vec1) (length vec2))))
    (if (zerop m*m) 0f0 (acos (cl:/ dot m*m)))))

(declaim (inline lerp!))
(declaim (ftype (function (vec vec vec single-float) vec) lerp!))
(defun lerp! (out vec1 vec2 factor)
  "Linearly interpolate between VEC1 and VEC2 by FACTOR, storing the result in
the existing vector, OUT."
  (with-vectors ((o out)
                 (v1 vec1)
                 (v2 vec2))
    (psetf ox (au:lerp factor v1x v2x)
           oy (au:lerp factor v1y v2y)
           oz (au:lerp factor v1z v2z)
           ow (au:lerp factor v1w v2w)))
  out)

(declaim (inline lerp))
(declaim (ftype (function (vec vec single-float) vec) lerp))
(defun lerp (vec1 vec2 factor)
  "Linearly interpolate between VEC1 and VEC2 by FACTOR, storing the result in a
freshly allocated vector."
  (lerp! (zero) vec1 vec2 factor))

(declaim (inline <))
(declaim (ftype (function (vec vec) boolean) <))
(defun < (vec1 vec2)
  "Check if each component of VEC1 is less than the same component of VEC2."
  (with-vectors ((v1 vec1)
                 (v2 vec2))
    (and (cl:< v1x v2x)
         (cl:< v1y v2y)
         (cl:< v1z v2z)
         (cl:< v1w v2w))))

(declaim (inline <=))
(declaim (ftype (function (vec vec) boolean) <=))
(defun <= (vec1 vec2)
  "Check if each component of VEC1 is less than or equal to the same component
of VEC2."
  (with-vectors ((v1 vec1)
                 (v2 vec2))
    (and (cl:<= v1x v2x)
         (cl:<= v1y v2y)
         (cl:<= v1z v2z)
         (cl:<= v1w v2w))))

(declaim (inline >))
(declaim (ftype (function (vec vec) boolean) >))
(defun > (vec1 vec2)
  "Check if each component of VEC1 is greater than the same component of VEC2."
  (with-vectors ((v1 vec1)
                 (v2 vec2))
    (and (cl:> v1x v2x)
         (cl:> v1y v2y)
         (cl:> v1z v2z)
         (cl:> v1w v2w))))

(declaim (inline >=))
(declaim (ftype (function (vec vec) boolean) >=))
(defun >= (vec1 vec2)
  "Check if each component of VEC1 is greater than or equal to the same
component of VEC2."
  (with-vectors ((v1 vec1)
                 (v2 vec2))
    (and (cl:>= v1x v2x)
         (cl:>= v1y v2y)
         (cl:>= v1z v2z)
         (cl:>= v1w v2w))))

(declaim (inline min!))
(declaim (ftype (function (vec vec vec) vec) min!))
(defun min! (out vec1 vec2)
  "Return the minimum of each component in VEC1 and VEC2, into the existing
vector, OUT."
  (with-vectors ((o out)
                 (v1 vec1)
                 (v2 vec2))
    (psetf ox (cl:min v1x v2x)
           oy (cl:min v1y v2y)
           oz (cl:min v1z v2z)
           ow (cl:min v1w v2w)))
  out)

(declaim (inline min))
(declaim (ftype (function (vec vec) vec) min))
(defun min (vec1 vec2)
  "Return the minimum each component in VEC1 and VEC2, into a freshly allocated
vector."
  (min! (zero) vec1 vec2))

(declaim (inline max!))
(declaim (ftype (function (vec vec vec) vec) max!))
(defun max! (out vec1 vec2)
  "Return the maximum of each component in VEC1 and VEC2, into the existing
vector, OUT."
  (with-vectors ((o out)
                 (v1 vec1)
                 (v2 vec2))
    (psetf ox (cl:max v1x v2x)
           oy (cl:max v1y v2y)
           oz (cl:max v1z v2z)
           ow (cl:max v1w v2w)))
  out)

(declaim (inline max))
(declaim (ftype (function (vec vec) vec) max))
(defun max (vec1 vec2)
  "Return the maximum of each component in VEC1 and VEC2, into a freshly
allocated vector."
  (max! (zero) vec1 vec2))

(declaim (inline radians!))
(declaim (ftype (function (vec vec) vec) radians!))
(defun radians! (out vec)
  (with-vectors ((o out)
                 (v vec))
    (let ((x (float (cl:/ pi 180) 1f0)))
      (psetf ox (cl:* vx x)
             oy (cl:* vy x)
             oz (cl:* vz x)
             ow (cl:* vw x)))
    out))

(declaim (inline radians))
(declaim (ftype (function (vec) vec) radians))
(defun radians (vec)
  (radians! (zero) vec))

(declaim (inline degrees!))
(declaim (ftype (function (vec vec) vec) degrees!))
(defun degrees! (out vec)
  (with-vectors ((o out)
                 (v vec))
    (let ((x (float (cl:/ 180 pi) 1f0)))
      (psetf ox (cl:* vx x)
             oy (cl:* vy x)
             oz (cl:* vz x)
             ow (cl:* vw x))))
  out)

(declaim (inline degrees))
(declaim (ftype (function (vec) vec) degrees))
(defun degrees (vec)
  (degrees! (zero) vec))
