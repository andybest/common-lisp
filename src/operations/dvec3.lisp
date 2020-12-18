(in-package #:net.mfiano.lisp.origin.dvec3)

(u:fn-> = (vec vec &key (:rel u:f64) (:abs u:f64)) boolean)
(declaim (inline =))
(defun = (vec1 vec2 &key (rel 1d-7) (abs rel))
  (declare (optimize speed))
  (com:cwcmp 3 (vec1 vec2) (com:= vec1 vec2 rel abs)))

(u:fn-> zero! (vec) vec)
(declaim (inline zero!))
(defun zero! (vec)
  "Modify the vector VEC by setting each of its components to zero."
  (declare (optimize speed))
  (com:cwset 3 vec nil 0d0)
  vec)

(u:fn-> zero () vec)
(declaim (inline zero))
(defun zero ()
  "Construct a fresh vector with each component set to zero."
  (declare (optimize speed))
  (%vec 0d0 0d0 0d0))

(u:fn-> zero-p (vec) boolean)
(declaim (inline zero-p))
(defun zero-p (vec)
  "Check whether or not the input vector is a zero vector."
  (declare (optimize speed))
  (= vec +zero+))

(u:fn-> random! (vec u:f64 u:f64) vec)
(declaim (inline random!))
(defun random! (out min max)
  "Modify vector VEC to have a random value for each of its components, The
range of each component is bounded by MIN and MAX."
  (declare (optimize speed))
  (let ((diff (cl:- max min)))
    (com:cwset 3 out nil (cl:+ min (cl:random diff))))
  out)

(u:fn-> random (u:f64 u:f64) vec)
(declaim (inline random))
(defun random (min max)
  "Construct a fresh vector with random elements. The range of each component is
bounded by MIN and MAX."
  (declare (optimize speed))
  (random! (zero) min max))

(u:fn-> copy! (vec vec) vec)
(declaim (inline copy!))
(defun copy! (out vec)
  "Modify vector OUT by copying the components of vector VEC into it."
  (declare (optimize speed))
  (com:cwset 3 out vec vec)
  out)

(u:fn-> copy (vec) vec)
(declaim (inline copy))
(defun copy (vec)
  "Construct a fresh vector that is a copy of vector VEC."
  (declare (optimize speed))
  (copy! (zero) vec))

(u:fn-> sign! (vec vec) vec)
(declaim (inline sign!))
(defun sign! (out vec)
  "Modify vector OUT to have its components represent the sign of each component
of vector VEC."
  (declare (optimize speed))
  (com:cwset 3 out vec (signum vec))
  out)

(u:fn-> sign (vec) vec)
(declaim (inline sign))
(defun sign (vec)
  "Construct a fresh vector that has its components represent the sign of each
component of vector VEC."
  (declare (optimize speed))
  (sign! (zero) vec))

(u:fn-> fract! (vec vec) vec)
(declaim (inline fract!))
(defun fract! (out vec)
  "Modify vector OUT to have its components contain the fractional portion of
the components in vector VEC."
  (com:cwset 3 out vec (cl:- vec (ffloor vec)))
  out)

(u:fn-> fract (vec) vec)
(declaim (inline fract))
(defun fract (vec)
  "Construct a fresh vector that has its components contain the fractional
portion of the components in vector VEC."
  (fract! (zero) vec))

(u:fn-> clamp! (vec vec u:f64 u:f64) vec)
(declaim (inline clamp!))
(defun clamp! (out vec min max)
  "Modify vector OUT to have its components represent the components of vector
VEC, bounded by MIN and MAX."
  (declare (optimize speed))
  (com:cwset 3 out vec (u:clamp vec min max))
  out)

(u:fn-> clamp (vec u:f64 u:f64) vec)
(declaim (inline clamp))
(defun clamp (vec min max)
  "Construct a fresh vector that has the components of vector VEC bounded by MIN
and MAX."
  (declare (optimize speed))
  (clamp! (zero) vec min max))

(u:fn-> +! (vec vec vec) vec)
(declaim (inline +!))
(defun +! (out vec1 vec2)
  "Modify vector OUT by performing component-wise addition of vectors VEC1 and
VEC2."
  (declare (optimize speed))
  (com:cwset 3 out (vec1 vec2) (cl:+ vec1 vec2))
  out)

(u:fn-> + (vec vec) vec)
(declaim (inline +))
(defun + (vec1 vec2)
  "Construct a fresh vector by performing component-wise addition of vectors
VEC1 and VEC2."
  (declare (optimize speed))
  (+! (zero) vec1 vec2))

(u:fn-> -! (vec vec vec) vec)
(declaim (inline -!))
(defun -! (out vec1 vec2)
  "Modify vector OUT by performing component-wise subtraction of vectors VEC1
and VEC2."
  (declare (optimize speed))
  (com:cwset 3 out (vec1 vec2) (cl:- vec1 vec2))
  out)

(u:fn-> - (vec vec) vec)
(declaim (inline -))
(defun - (vec1 vec2)
  "Construct a fresh vector by performing component-wise substraction of vectors
VEC1 and VEC2."
  (declare (optimize speed))
  (-! (zero) vec1 vec2))

(u:fn-> *! (vec vec vec) vec)
(declaim (inline *!))
(defun *! (out vec1 vec2)
  "Modify vector OUT by performing component-wise multiplication of vectors VEC1
and VEC2."
  (declare (optimize speed))
  (com:cwset 3 out (vec1 vec2) (cl:* vec1 vec2))
  out)

(u:fn-> * (vec vec) vec)
(declaim (inline *))
(defun * (vec1 vec2)
  "Construct a fresh vector by performing component-wise multiplication of
vectors VEC1 and VEC2."
  (declare (optimize speed))
  (*! (zero) vec1 vec2))

(u:fn-> /! (vec vec vec) vec)
(declaim (inline /!))
(defun /! (out vec1 vec2)
  "Modify vector OUT by performing component-wise division of vectors VEC1 and
VEC2."
  (declare (optimize speed))
  (com:cwset 3 out (vec1 vec2) (if (zerop vec2) 0d0 (cl:/ vec1 vec2)))
  out)

(u:fn-> / (vec vec) vec)
(declaim (inline /))
(defun / (vec1 vec2)
  "Construct a fresh vector by performing component-wise division of vectors
VEC1 and VEC2."
  (declare (optimize speed))
  (/! (zero) vec1 vec2))

(u:fn-> scale! (vec vec u:f64) vec)
(declaim (inline scale!))
(defun scale! (out vec scalar)
  "Modify vector OUT by adding the scalar SCALAR to each component of vector
VEC."
  (declare (optimize speed))
  (with-components ((o out) (v vec))
    (psetf ox (cl:* vx scalar)
           oy (cl:* vy scalar)
           oz (cl:* vz scalar)))
  out)

(u:fn-> scale (vec u:f64) vec)
(declaim (inline scale))
(defun scale (vec scalar)
  "Construct a fresh vector by adding the scalar SCALAR to each component of
vector VEC."
  (declare (optimize speed))
  (scale! (zero) vec scalar))

(u:fn-> invert! (vec vec) vec)
(declaim (inline invert!))
(defun invert! (out vec)
  "Modify vector OUT to have each component be the inverted component of vector
VEC."
  (declare (optimize speed))
  (com:cwset 3 out vec (if (zerop vec) 0d0 (cl:/ vec)))
  out)

(u:fn-> invert (vec) vec)
(declaim (inline invert))
(defun invert (vec)
  "Construct a fresh vector with each component being the inverted component of
vector VEC."
  (declare (optimize speed))
  (invert! (zero) vec))

(u:fn-> dot (vec vec) u:f64)
(declaim (inline dot))
(defun dot (vec1 vec2)
  "Compute the dot product of vectors VEC1 and VEC2. Returns a scalar."
  (with-components ((v1 vec1) (v2 vec2))
    (cl:+ (cl:* v1x v2x) (cl:* v1y v2y) (cl:* v1z v2z))))

(u:fn-> length-squared (vec) u:f64)
(declaim (inline length-squared))
(defun length-squared (vec)
  "Compute the squared length of vector VEC."
  (with-components ((v vec))
    (cl:+ (cl:expt vx 2) (cl:expt vy 2) (cl:expt vz 2))))

(u:fn-> length (vec) u:f64)
(declaim (inline length))
(defun length (vec)
  "Compute the length of vector VEC. Returns a scalar."
  (cl:sqrt (length-squared vec)))

(u:fn-> distance-squared (vec vec) u:f64)
(declaim (inline distance-squared))
(defun distance-squared (vec1 vec2)
  (length-squared (- vec2 vec1)))

(u:fn-> distance (vec vec) u:f64)
(declaim (inline distance))
(defun distance (vec1 vec2)
  (cl:sqrt (distance-squared vec1 vec2)))

(u:fn-> normalize! (vec vec) vec)
(declaim (inline normalize!))
(defun normalize! (out vec)
  (declare (optimize speed))
  (let ((length (length vec)))
    (unless (zerop length)
      (scale! out vec (cl:/ length))))
  out)

(u:fn-> normalize (vec) vec)
(declaim (inline normalize))
(defun normalize (vec)
  (declare (optimize speed))
  (normalize! (zero) vec))

(u:fn-> round! (vec vec) vec)
(declaim (inline round!))
(defun round! (out vec)
  (com:cwset 3 out vec (fround vec))
  out)

(u:fn-> round (vec) vec)
(declaim (inline round))
(defun round (vec)
  (round! (zero) vec))

(u:fn-> abs! (vec vec) vec)
(declaim (inline abs!))
(defun abs! (out vec)
  (declare (optimize speed))
  (com:cwset 3 out vec (cl:abs vec))
  out)

(u:fn-> abs (vec) vec)
(declaim (inline abs))
(defun abs (vec)
  (declare (optimize speed))
  (abs! (zero) vec))

(u:fn-> negate! (vec vec) vec)
(declaim (inline negate!))
(defun negate! (out vec)
  (declare (optimize speed))
  (scale! out vec -1d0))

(u:fn-> negate (vec) vec)
(declaim (inline negate))
(defun negate (vec)
  (declare (optimize speed))
  (negate! (zero) vec))

(u:fn-> cross! (vec vec vec) vec)
(declaim (inline cross!))
(defun cross! (out vec1 vec2)
  (declare (optimize speed))
  (with-components ((o out) (v1 vec1) (v2 vec2))
    (psetf ox (cl:- (cl:* v1y v2z) (cl:* v1z v2y))
           oy (cl:- (cl:* v1z v2x) (cl:* v1x v2z))
           oz (cl:- (cl:* v1x v2y) (cl:* v1y v2x))))
  out)

(u:fn-> cross (vec vec) vec)
(declaim (inline cross))
(defun cross (vec1 vec2)
  (declare (optimize speed))
  (cross! (zero) vec1 vec2))

(u:fn-> angle (vec vec) u:f64)
(declaim (inline angle))
(defun angle (vec1 vec2)
  (let ((dot (dot vec1 vec2))
        (m*m (cl:* (length vec1) (length vec2))))
    (if (zerop m*m)
        0d0
        (cl:acos (the (double-float -1d0 1d0) (cl:/ dot m*m))))))

(u:fn-> direction= (vec vec) boolean)
(declaim (inline direction=))
(defun direction= (vec1 vec2)
  (declare (optimize speed))
  (cl:>= (dot (normalize vec1) (normalize vec2)) (cl:- 1 1d-7)))

(u:fn-> parallel-p (vec vec) boolean)
(declaim (inline parallel-p))
(defun parallel-p (vec1 vec2)
  (= (cross vec1 vec2) +zero+))

(u:fn-> lerp! (vec vec vec u:f64) vec)
(declaim (inline lerp!))
(defun lerp! (out vec1 vec2 factor)
  (declare (optimize speed))
  (com:cwset 3 out (vec1 vec2) (u:lerp factor vec1 vec2))
  out)

(u:fn-> lerp (vec vec u:f64) vec)
(declaim (inline lerp))
(defun lerp (vec1 vec2 factor)
  (declare (optimize speed))
  (lerp! (zero) vec1 vec2 factor))

(u:fn-> < (vec vec) boolean)
(declaim (inline <))
(defun < (vec1 vec2)
  (declare (optimize speed))
  (com:cwcmp 3 (vec1 vec2) (cl:< vec1 vec2)))

(u:fn-> <= (vec vec) boolean)
(declaim (inline <=))
(defun <= (vec1 vec2)
  (declare (optimize speed))
  (com:cwcmp 3 (vec1 vec2) (cl:<= vec1 vec2)))

(u:fn-> > (vec vec) boolean)
(declaim (inline >))
(defun > (vec1 vec2)
  (declare (optimize speed))
  (com:cwcmp 3 (vec1 vec2) (cl:> vec1 vec2)))

(u:fn-> >= (vec vec) boolean)
(declaim (inline >=))
(defun >= (vec1 vec2)
  (declare (optimize speed))
  (com:cwcmp 3 (vec1 vec2) (cl:>= vec1 vec2)))

(u:fn-> min! (vec vec vec) vec)
(declaim (inline min!))
(defun min! (out vec1 vec2)
  (declare (optimize speed))
  (com:cwset 3 out (vec1 vec2) (cl:min vec1 vec2))
  out)

(u:fn-> min (vec vec) vec)
(declaim (inline min))
(defun min (vec1 vec2)
  (declare (optimize speed))
  (min! (zero) vec1 vec2))

(u:fn-> max! (vec vec vec) vec)
(declaim (inline max!))
(defun max! (out vec1 vec2)
  (declare (optimize speed))
  (com:cwset 3 out (vec1 vec2) (cl:max vec1 vec2))
  out)

(u:fn-> max (vec vec) vec)
(declaim (inline max))
(defun max (vec1 vec2)
  (declare (optimize speed))
  (max! (zero) vec1 vec2))

(u:fn-> radians! (vec vec) vec)
(declaim (inline radians!))
(defun radians! (out vec)
  (declare (optimize speed))
  (com:cwset 3 out vec (cl:* vec com:+deg/double+))
  out)

(u:fn-> radians (vec) vec)
(declaim (inline radians))
(defun radians (vec)
  (declare (optimize speed))
  (radians! (zero) vec))

(u:fn-> degrees! (vec vec) vec)
(declaim (inline degrees!))
(defun degrees! (out vec)
  (declare (optimize speed))
  (com:cwset 3 out vec (cl:* vec com:+rad/double+))
  out)

(u:fn-> degrees (vec) vec)
(declaim (inline degrees))
(defun degrees (vec)
  (declare (optimize speed))
  (degrees! (zero) vec))

(u:fn-> expt! (vec vec real) vec)
(declaim (inline expt!))
(defun expt! (out vec power)
  (com:cwset 3 out vec (cl:expt vec power))
  out)

(u:fn-> expt (vec real) vec)
(declaim (inline expt))
(defun expt (vec power)
  (expt! (zero) vec power))

(u:fn-> sqrt! (vec vec) vec)
(declaim (inline sqrt!))
(defun sqrt! (out vec)
  (declare (optimize speed))
  (com:cwset 3 out vec (cl:sqrt (the (double-float 0d0) vec)))
  out)

(u:fn-> sqrt (vec) vec)
(declaim (inline sqrt))
(defun sqrt (vec)
  (declare (optimize speed))
  (sqrt! (zero) vec))

(u:fn-> floor! (vec vec) vec)
(declaim (inline floor!))
(defun floor! (out vec)
  (com:cwset 3 out vec (ffloor vec))
  out)

(u:fn-> floor (vec) vec)
(declaim (inline floor))
(defun floor (vec)
  (floor! (zero) vec))

(u:fn-> ceiling! (vec vec) vec)
(declaim (inline ceiling!))
(defun ceiling! (out vec)
  (com:cwset 3 out vec (fceiling vec))
  out)

(u:fn-> ceiling (vec) vec)
(declaim (inline ceiling))
(defun ceiling (vec)
  (ceiling! (zero) vec))

(u:fn-> mod! (vec vec u:f64) vec)
(declaim (inline mod!))
(defun mod! (out vec divisor)
  (com:cwset 3 out vec (nth-value 1 (ffloor vec divisor)))
  out)

(u:fn-> mod (vec real) vec)
(declaim (inline mod))
(defun mod (vec divisor)
  (mod! (zero) vec divisor))

(u:fn-> sin! (vec vec) vec)
(declaim (inline sin!))
(defun sin! (out vec)
  (declare (optimize speed))
  (com:cwset 3 out vec (cl:sin vec))
  out)

(u:fn-> sin (vec) vec)
(declaim (inline sin))
(defun sin (vec)
  (declare (optimize speed))
  (sin! (zero) vec))

(u:fn-> cos! (vec vec) vec)
(declaim (inline cos!))
(defun cos! (out vec)
  (declare (optimize speed))
  (com:cwset 3 out vec (cl:cos vec))
  out)

(u:fn-> cos (vec) vec)
(declaim (inline cos))
(defun cos (vec)
  (declare (optimize speed))
  (cos! (zero) vec))

(u:fn-> tan! (vec vec) vec)
(declaim (inline tan!))
(defun tan! (out vec)
  (declare (optimize speed))
  (com:cwset 3 out vec (cl:tan vec))
  out)

(u:fn-> tan (vec) vec)
(declaim (inline tan))
(defun tan (vec)
  (declare (optimize speed))
  (tan! (zero) vec))

(u:fn-> asin! (vec vec) vec)
(declaim (inline asin!))
(defun asin! (out vec)
  (declare (optimize speed))
  (com:cwset 3 out vec (cl:asin (the (double-float -1d0 1d0) vec)))
  out)

(u:fn-> asin (vec) vec)
(declaim (inline asin))
(defun asin (vec)
  (declare (optimize speed))
  (asin! (zero) vec))

(u:fn-> acos! (vec vec) vec)
(declaim (inline acos!))
(defun acos! (out vec)
  (declare (optimize speed))
  (com:cwset 3 out vec (cl:acos (the (double-float -1d0 1d0) vec)))
  out)

(u:fn-> acos (vec) vec)
(declaim (inline acos))
(defun acos (vec)
  (declare (optimize speed))
  (acos! (zero) vec))

(u:fn-> atan! (vec vec) vec)
(declaim (inline atan!))
(defun atan! (out vec)
  (declare (optimize speed))
  (com:cwset 3 out vec (cl:atan vec))
  out)

(u:fn-> atan (vec) vec)
(declaim (inline atan))
(defun atan (vec)
  (declare (optimize speed))
  (atan! (zero) vec))

(u:fn-> velocity! (vec vec u:f64) vec)
(declaim (inline velocity!))
(defun velocity! (vec axis rate)
  (declare (optimize speed))
  (copy! vec axis)
  (normalize! vec vec)
  (scale! vec vec rate))

(u:fn-> velocity (vec u:f64) vec)
(declaim (inline velocity))
(defun velocity (axis rate)
  (declare (optimize speed))
  (velocity! (zero) axis rate))
