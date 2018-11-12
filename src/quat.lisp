(in-package :box.math.quat)

;;; Structure

(deftype quat () '(simple-array single-float (4)))

(defstruct (quat (:type (vector single-float))
                 (:constructor %make (w x y z))
                 (:conc-name nil)
                 (:copier nil))
  "A 4-dimensional complex number that describes a 3-dimensional rotation."
  (w 0.0f0 :type single-float)
  (x 0.0f0 :type single-float)
  (y 0.0f0 :type single-float)
  (z 0.0f0 :type single-float))

(defmacro with-components (((prefix quat) &rest rest) &body body)
  "A convenience macro for concisely accessing the components of quaternions."
  `(with-accessors ((,prefix identity)
                    (,(box.math.common::%make-accessor-symbol prefix 'w) w)
                    (,(box.math.common::%make-accessor-symbol prefix 'x) x)
                    (,(box.math.common::%make-accessor-symbol prefix 'y) y)
                    (,(box.math.common::%make-accessor-symbol prefix 'z) z))
       ,quat
     ,(if rest
          `(with-components ,rest ,@body)
          `(progn ,@body))))

;;; Constants

(au:define-constant +zero+
    (make-array 4 :element-type 'single-float :initial-contents '(0.0f0 0.0f0 0.0f0 0.0f0))
  :test #'equalp
  :documentation "A quaternion with each component as zero.")

(au:define-constant +id+
    (make-array 4 :element-type 'single-float :initial-contents '(1.0f0 0.0f0 0.0f0 0.0f0))
  :test #'equalp
  :documentation "An identity quaternion.")

;;; Operations

(declaim (inline make))
(declaim (ftype (function (real real real real) quat) make))
(defun make (w x y z)
  "Create a new quaternion."
  (%make (float w 1.0f0) (float x 1.0f0) (float y 1.0f0) (float z 1.0f0)))

(declaim (ftype (function (quat) quat) id!))
(defun id! (quat)
  "Modify QUAT to be an identity quaternion."
  (with-components ((q quat))
    (psetf qw 1.0f0 qx 0.0f0 qy 0.0f0 qz 0.0f0))
  quat)

(declaim (ftype (function () quat) id))
(defun id ()
  "Create an identity quaternion."
  (id! (make 0 0 0 0)))

(declaim (inline zero!))
(declaim (ftype (function (quat) quat) zero!))
(defun zero! (quat)
  "Set each component of QUAT to zero."
  (with-components ((q quat))
    (psetf qw 0.0f0 qx 0.0f0 qy 0.0f0 qz 0.0f0))
  quat)

(declaim (inline zero))
(declaim (ftype (function () quat) zero))
(defun zero ()
  "Create a new quaternion with all components initialized to zero."
  (make 0 0 0 0))

(declaim (inline =))
(declaim (ftype (function (quat quat) boolean) =))
(defun = (quat1 quat2)
  "Check if all components of QUAT1 are numerically equal to the components of QUAT2."
  (with-components ((q1 quat1) (q2 quat2))
    (and (cl:= q1w q2w)
         (cl:= q1x q2x)
         (cl:= q1y q2y)
         (cl:= q1z q2z))))

(declaim (inline ~))
(declaim (ftype (function (quat quat &key (:tolerance single-float)) boolean) ~))
(defun ~ (quat1 quat2 &key (tolerance +epsilon+))
  "Check if all components of QUAT1 are approximately equal to the components of QUAT2, according to
TOLERANCE."
  (with-components ((q1 quat1) (q2 quat2))
    (and (box.math.common::%~ q1w q2w tolerance)
         (box.math.common::%~ q1x q2x tolerance)
         (box.math.common::%~ q1y q2y tolerance)
         (box.math.common::%~ q1z q2z tolerance))))

(declaim (inline copy!))
(declaim (ftype (function (quat quat) quat) copy!))
(defun copy! (out quat)
  "Copy each component of QUAT to the existing quaternion, OUT."
  (with-components ((o out) (q quat))
    (psetf ow qw ox qx oy qy oz qz))
  out)

(declaim (inline copy))
(declaim (ftype (function (quat) quat) copy))
(defun copy (quat)
  "Copy each component of QUAT to a freshly allocated quaternion."
  (copy! (id) quat))

(declaim (inline +!))
(declaim (ftype (function (quat quat quat) quat) +!))
(defun +! (out quat1 quat2)
  "Calculate the sum of QUAT1 and QUAT2, storing the result in the existing quaternion, OUT."
  (with-components ((o out) (q1 quat1) (q2 quat2))
    (psetf ow (cl:+ q1w q2w)
           ox (cl:+ q1x q2x)
           oy (cl:+ q1y q2y)
           oz (cl:+ q1z q2z)))
  out)

(declaim (inline +))
(declaim (ftype (function (quat quat) quat) +))
(defun + (quat1 quat2)
  "Calculate the sum of QUAT1 and QUAT2, storing the result in a freshly allocated quaternion."
  (+! (id) quat1 quat2))

(declaim (inline -!))
(declaim (ftype (function (quat quat quat) quat) -!))
(defun -! (out quat1 quat2)
  "Calculate the difference of QUAT2 from QUAT1, storing the result in the existing quaternion,
OUT."
  (with-components ((o out) (q1 quat1) (q2 quat2))
    (psetf ow (cl:- q1w q2w)
           ox (cl:- q1x q2x)
           oy (cl:- q1y q2y)
           oz (cl:- q1z q2z)))
  out)

(declaim (inline -))
(declaim (ftype (function (quat quat) quat) -))
(defun - (quat1 quat2)
  "Calculate the difference of QUAT2 from QUAT1, storing the result in a freshly allocated
quaternion."
  (-! (id) quat1 quat2))

(declaim (inline *!))
(declaim (ftype (function (quat quat quat) quat) *!))
(defun *! (out quat1 quat2)
  "Calculate the product of QUAT1 and QUAT2, storing the result in the existing quaternion, OUT."
  (with-components ((o out) (q1 quat1) (q2 quat2))
    (psetf ow (cl:- (cl:* q1w q2w) (cl:* q1x q2x) (cl:* q1y q2y) (cl:* q1z q2z))
           ox (cl:- (cl:+ (cl:* q1w q2x) (cl:* q1x q2w) (cl:* q1y q2z)) (cl:* q1z q2y))
           oy (cl:- (cl:+ (cl:* q1w q2y) (cl:* q1y q2w) (cl:* q1z q2x)) (cl:* q1x q2z))
           oz (cl:- (cl:+ (cl:* q1w q2z) (cl:* q1z q2w) (cl:* q1x q2y)) (cl:* q1y q2x))))
  out)

(declaim (inline *))
(declaim (ftype (function (quat quat) quat) *))
(defun * (quat1 quat2)
  "Calculate the product of QUAT1 and QUAT2, storing the result in a freshly allocated quaternion."
  (*! (id) quat1 quat2))

(declaim (inline scale!))
(declaim (ftype (function (quat quat single-float) quat) scale!))
(defun scale! (out quat scalar)
  "Scale QUAT by SCALAR, storing the result in the existing quaternion, OUT."
  (with-components ((o out) (q quat))
    (psetf ow (cl:* qw scalar)
           ox (cl:* qx scalar)
           oy (cl:* qy scalar)
           oz (cl:* qz scalar)))
  out)

(declaim (inline scale))
(declaim (ftype (function (quat single-float) quat) scale))
(defun scale (quat scalar)
  "Scale QUAT by SCALAR, storing the result in a freshly allocated quaternion."
  (scale! (id) quat scalar))

(declaim (inline conjugate!))
(declaim (ftype (function (quat quat) quat) conjugate!))
(defun conjugate! (out quat)
  "Calculate the conjugate of QUAT, storing the result in the existing quaternion, OUT."
  (with-components ((o out) (q quat))
    (psetf ow qw
           ox (cl:- qx)
           oy (cl:- qy)
           oz (cl:- qz)))
  out)

(declaim (inline conjugate))
(declaim (ftype (function (quat) quat) conjugate))
(defun conjugate (quat)
  "Calculate the conjugate of QUAT, storing the result in a freshly allocated quaternion."
  (conjugate! (id) quat))

(declaim (inline cross!))
(declaim (ftype (function (quat quat quat) quat) cross!))
(defun cross! (out quat1 quat2)
  "Calculate the cross product of QUAT1 and QUAT2, storing the result in the existing quaternion,
OUT."
  (scale! out (+ (* quat2 (conjugate quat1)) (* quat1 quat2)) 0.5f0))

(declaim (inline cross))
(declaim (ftype (function (quat quat) quat) cross))
(defun cross (quat1 quat2)
  "Calculate the cross product of QUAT1 and QUAT2, storing the result in a freshly allocated
quaternion."
  (cross! (id) quat1 quat2))

(declaim (inline magnitude-squared))
(declaim (ftype (function (quat) single-float) magnitude-squared))
(defun magnitude-squared (quat)
  "Calculate the magnitude (also known as length or Euclidean norm) of QUAT. This results in a
squared value, which is cheaper to compute. It is useful when you want to compare relative lengths,
which does not need the expensive square root function.

See MAGNITUDE for other cases."
  (with-components ((q quat))
    (cl:+ (cl:* qw qw) (cl:* qx qx) (cl:* qy qy) (cl:* qz qz))))

(declaim (inline magnitude))
(declaim (ftype (function (quat) single-float) magnitude))
(defun magnitude (quat)
  "Compute the magnitude (also known as length or Euclidean norm) of QUAT.

See MAGNITUDE-SQUARED if you only need to compare lengths, as it is cheaper to compute without the
square root call of this function."
  (sqrt (magnitude-squared quat)))

(declaim (inline normalize!))
(declaim (ftype (function (quat quat) quat) normalize!))
(defun normalize! (out quat)
  "Convert QUAT to be of unit length, storing the result in the existing quaternion, OUT."
  (let ((magnitude (magnitude quat)))
    (unless (zerop magnitude)
      (scale! out quat (/ magnitude))))
  out)

(declaim (inline normalize))
(declaim (ftype (function (quat) quat) normalize))
(defun normalize (quat)
  "Convert QUAT to be of unit length, storing the result in a freshly allocated quaternion."
  (normalize! (id) quat))

(declaim (inline negate!))
(declaim (ftype (function (quat quat) quat) negate!))
(defun negate! (out quat)
  "Negate each component of QUAT, storing the result in the existing quaternion, OUT."
  (scale! out quat -1.0f0))

(declaim (inline negate))
(declaim (ftype (function (quat) quat) negate))
(defun negate (quat)
  "Negate each component of QUAT, storing the result in a freshly allocated quaternion."
  (negate! (id) quat))

(declaim (inline dot))
(declaim (ftype (function (quat quat) single-float) dot))
(defun dot (quat1 quat2)
  "Calculate the dot product of QUAT1 and QUAT2. Returns a scalar."
  (with-components ((q1 quat1) (q2 quat2))
    (cl:+ (cl:* q1w q2w) (cl:* q1x q2x) (cl:* q1y q2y) (cl:* q1z q2z))))

(declaim (inline inverse!))
(declaim (ftype (function (quat quat) quat) inverse!))
(defun inverse! (out quat)
  "Calculate the inverse of QUAT, storing the result in the existing quaternion, OUT."
  (conjugate! out quat)
  (scale! out out (/ (magnitude-squared quat)))
  out)

(declaim (inline inverse))
(declaim (ftype (function (quat) quat) inverse))
(defun inverse (quat)
  "Calculate the inverse of QUAT, storing the result in a freshly allocated quaternion."
  (inverse! (id) quat))

(declaim (inline rotate!))
(declaim (ftype (function (quat quat v3:vec) quat) rotate!))
(defun rotate! (out quat vec)
  "Rotate QUAT by the vector of Euler angles, VEC, storing the result in the existing quaternion,
OUT."
  (with-components ((o out) (q (copy quat)))
    (v3:with-components ((v (v3:scale vec 0.5f0))
                         (c (v3:make (cos vx) (cos vy) (cos vz)))
                         (s (v3:make (sin vx) (sin vy) (sin vz))))
      (psetf ow (cl:- (cl:* cx cy cz) (cl:* sx sy sz))
             ox (cl:+ (cl:* sx cy cz) (cl:* cx sy sz))
             oy (cl:- (cl:* cx sy cz) (cl:* sx cy sz))
             oz (cl:+ (cl:* sx sy cz) (cl:* cx cy sz)))
      (*! out out q)))
  out)

(declaim (ftype (function (quat v3:vec) quat) rotate))
(defun rotate (quat vec)
  "Rotate QUAT by the vector of Euler angles, VEC, storing the result in a freshly allocated
quaternion."
  (rotate! (id) quat vec))

(declaim (inline to-vec3!))
(declaim (ftype (function (v3:vec quat) v3:vec) to-vec3!))
(defun to-vec3! (out quat)
  "Copy the imaginary components of QUAT to the existing vector, OUT."
  (v3:with-components ((v out))
    (with-components ((q quat))
      (setf vx qx vy qy vz qz)))
  out)

(declaim (inline to-vec3))
(declaim (ftype (function (quat) v3:vec) to-vec3))
(defun to-vec3 (quat)
  "Copy the imaginary components of QUAT to a freshly allocated vector."
  (to-vec3! (v3:zero) quat))

(declaim (inline to-vec4!))
(declaim (ftype (function (v4:vec quat) v4:vec) to-vec4!))
(defun to-vec4! (out quat)
  "Copy the components of QUAT to the existing vector, OUT."
  (v4:with-components ((v out))
    (with-components ((q quat))
      (setf vx qw vy qx vz qy vw qz)))
  out)

(declaim (inline to-vec4))
(declaim (ftype (function (quat) v4:vec) to-vec4))
(defun to-vec4 (quat)
  "Copy the components of QUAT to a freshly allocated vector."
  (to-vec4! (v4:zero) quat))

(declaim (inline from-vec3!))
(declaim (ftype (function (quat v3:vec) quat) from-vec3!))
(defun from-vec3! (out vec)
  "Copy the components of VEC to the imaginary components of the existing quaternion, OUT."
  (with-components ((q out))
    (v3:with-components ((v vec))
      (setf qw 0.0f0 qx vx qy vy qz vz)))
  out)

(declaim (inline from-vec3))
(declaim (ftype (function (v3:vec) quat) from-vec3))
(defun from-vec3 (vec)
  "Copy the components of VEC to the imaginary components of a freshly allocated quaternion."
  (from-vec3! (zero) vec))

(declaim (inline from-vec4!))
(declaim (ftype (function (quat v4:vec) quat) from-vec4!))
(defun from-vec4! (out vec)
  "Copy the components of VEC to the existing quaternion, OUT."
  (with-components ((q out))
    (v4:with-components ((v vec))
      (setf qw vx qx vy qy vz qz vw)))
  out)

(declaim (inline v4->q))
(declaim (ftype (function (v4:vec) quat) from-vec4))
(defun from-vec4 (vec)
  "Copy the components of VEC to a freshly allocated quaternion."
  (from-vec4! (zero) vec))

(declaim (inline to-mat4!))
(declaim (ftype (function (m4:matrix quat) m4:matrix) to-mat4!))
(defun to-mat4! (out quat)
  "Convert QUAT to a 3-dimensional rotation matrix, storing the result in the existing matrix, OUT."
  (m4:with-components ((o out))
    (with-components ((q quat))
      (let* ((s (/ 2 (magnitude-squared quat)))
             (xs (cl:* qx s))
             (ys (cl:* qy s))
             (zs (cl:* qz s))
             (xx (cl:* qx xs))
             (xy (cl:* qx ys))
             (xz (cl:* qx zs))
             (yy (cl:* qy ys))
             (yz (cl:* qy zs))
             (zz (cl:* qz zs))
             (wx (cl:* qw xs))
             (wy (cl:* qw ys))
             (wz (cl:* qw zs)))
        (psetf o00 (cl:- 1 (cl:+ yy zz))
               o01 (cl:- xy wz)
               o02 (cl:+ xz wy)
               o03 0.0f0
               o10 (cl:+ xy wz)
               o11 (cl:- 1 (cl:+ xx zz))
               o12 (cl:- yz wx)
               o13 0.0f0
               o20 (cl:- xz wy)
               o21 (cl:+ yz wx)
               o22 (cl:- 1 (cl:+ xx yy))
               o23 0.0f0
               o30 0.0f0
               o31 0.0f0
               o32 0.0f0
               o33 1.0f0))))
  out)

(declaim (inline to-mat4))
(declaim (ftype (function (quat) m4:matrix) to-mat4))
(defun to-mat4 (quat)
  "Convert QUAT to a 3-dimensional rotation matrix, storing the result in a freshly allocated
matrix."
  (to-mat4! (m4:id) quat))

(declaim (ftype (function (quat m4:matrix) quat) from-mat4!))
(defun from-mat4! (out matrix)
  "Convert MATRIX to a quaternion, storing the result in the existing quaternion, OUT."
  (with-components ((q out))
    (m4:with-components ((m matrix))
      (let ((trace (m4:trace matrix))
            (col1 (1+ (cl:- m00 m11 m22)))
            (col2 (1+ (cl:- m11 m00 m22)))
            (col3 (1+ (cl:- m22 m00 m11)))
            (s 0.0f0))
        (cond
          ((plusp trace)
           (setf s (/ 0.5f0 (sqrt trace))
                 qw (/ 0.25f0 s)
                 qx (cl:* (cl:- m21 m12) s)
                 qy (cl:* (cl:- m02 m20) s)
                 qz (cl:* (cl:- m10 m01) s)))
          ((and (>= col1 col2) (>= col1 col3))
           (setf s (/ 0.5f0 (sqrt col1))
                 qw (cl:* (cl:- m21 m12) s)
                 qx (/ 0.25f0 s)
                 qy (cl:* (cl:+ m10 m01) s)
                 qz (cl:* (cl:+ m02 m20) s)))
          ((and (>= col2 col1) (>= col2 col3))
           (setf s (/ 0.5f0 (sqrt col2))
                 qw (cl:* (cl:- m02 m20) s)
                 qx (cl:* (cl:+ m01 m10) s)
                 qy (/ 0.25f0 s)
                 qz (cl:* (cl:+ m12 m21) s)))
          (t
           (setf s (/ 0.5f0 (sqrt col3))
                 qw (cl:* (cl:- m10 m01) s)
                 qx (cl:* (cl:+ m02 m20) s)
                 qy (cl:* (cl:+ m12 m21) s)
                 qz (/ 0.25f0 s)))))))
  out)

(declaim (inline from-mat4))
(declaim (ftype (function (m4:matrix) quat) from-mat4))
(defun from-mat4 (matrix)
  "Convert MATRIX to a quaternion, storing the result in a freshly allocated quaternion."
  (from-mat4! (id) matrix))

(declaim (ftype (function (quat quat quat single-float) quat) slerp!))
(defun slerp! (out quat1 quat2 factor)
  "Perform a spherical linear interpolation between QUAT1 and QUAT2 by FACTOR, storing the result in
the existing quaternion, OUT."
  (with-components ((o out) (q1 quat1) (q2 quat2))
    (let ((dot (dot q1 q2))
          (q2 q2))
      (when (minusp dot)
        (negate! q2 q2)
        (setf dot (cl:- dot)))
      (if (> (abs dot) 0.9995f0)
          (psetf ow (au:lerp factor q1w q2w)
                 ox (au:lerp factor q1x q2x)
                 oy (au:lerp factor q1y q2y)
                 oz (au:lerp factor q1z q2z))
          (let* ((angle (acos (au:clamp dot 0 1)))
                 (sin-angle (sin angle))
                 (scale1 (/ (sin (cl:* angle (cl:- 1 factor))) sin-angle))
                 (scale2 (/ (sin (cl:* factor angle)) sin-angle)))
            (psetf ow (cl:+ (cl:* q1w scale1) (cl:* q2w scale2))
                   ox (cl:+ (cl:* q1x scale1) (cl:* q2x scale2))
                   oy (cl:+ (cl:* q1y scale1) (cl:* q2y scale2))
                   oz (cl:+ (cl:* q1z scale1) (cl:* q2z scale2)))))))
  out)

(declaim (inline slerp))
(declaim (ftype (function (quat quat single-float) quat) slerp))
(defun slerp (quat1 quat2 factor)
  "Perform a spherical linear interpolation between QUAT1 and QUAT2 by FACTOR, storing the result in
a freshly allocated quaternion."
  (slerp! (id) quat1 quat2 factor))
