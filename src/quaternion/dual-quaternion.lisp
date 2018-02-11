(in-package :box.math.dquat)

;;; Structure

(deftype dquat () '(simple-array q:quat (2)))

(defstruct (dquat (:type vector)
                  (:constructor %make (real dual))
                  (:conc-name dq-)
                  (:copier nil))
  "A set of quaternions that can describe both a rotation and translation in 3 dimensions."
  (real (q:zero) :type q:quat)
  (dual (q:zero) :type q:quat))

(defmacro with-components (((prefix dquat) . rest) &body body)
  "A convenience macro for concisely accessing the components of dual quaternions."
  `(q:with-components ((,prefix ,dquat)
                       (,(box.math.base::%make-accessor-symbol prefix '.r) (dq-real ,dquat))
                       (,(box.math.base::%make-accessor-symbol prefix '.d) (dq-dual ,dquat)))
     ,dquat
     ,(if rest
          `(with-components ,rest ,@body)
          `(progn ,@body))))

;;; Operations

(declaim (inline make))
(declaim (ftype (function (q:quat q:quat) dquat) make))
(defun make (real dual)
  "Create a dual quaternion"
  (%make real dual))

(declaim (inline id!))
(declaim (ftype (function (dquat) dquat) id!))
(defun id! (dquat)
  "Modify DQUAT to be an identity dual quaternion."
  (with-components ((d dquat))
    (q:id! d.r)
    (psetf d.dw 0.0f0 d.dx 0.0f0 d.dy 0.0f0 d.dz 0.0f0))
  dquat)

(declaim (inline id))
(declaim (ftype (function () dquat) id))
(defun id ()
  "Create an identity dual quaternion."
  (id! (make (q:id) (q:zero))))

(declaim (inline zero!))
(declaim (ftype (function (dquat) dquat) zero!))
(defun zero! (dquat)
  "Set each component of DQUAT to zero."
  (with-components ((d dquat))
    (q:zero! d.r)
    (q:zero! d.d))
  dquat)

(declaim (inline zero))
(declaim (ftype (function () dquat) zero))
(defun zero ()
  "Create a new dual quaternion with all components initialized to zero."
  (make (q:zero) (q:zero)))

(declaim (inline =))
(declaim (ftype (function (dquat dquat) boolean) =))
(defun = (dquat1 dquat2)
  "Check if all components of QUAT1 are numerically equal to the components of QUAT2."
  (with-components ((d1 dquat1) (d2 dquat2))
    (and (q:= d1.r d2.r)
         (q:= d1.d d2.d))))

(declaim (inline ~))
(declaim (ftype (function (dquat dquat &key (:tolerance single-float)) boolean) ~))
(defun ~ (dquat1 dquat2 &key (tolerance +epsilon+))
  "Check if all components of DQUAT1 are approximately equal to the components of QUAT2, according
to TOLERANCE."
  (with-components ((d1 dquat1) (d2 dquat2))
    (and (q:~ d1.r d2.r :tolerance tolerance)
         (q:~ d1.d d2.d :tolerance tolerance))))

(declaim (inline copy!))
(declaim (ftype (function (dquat dquat) dquat) copy!))
(defun copy! (out dquat)
  "Copy each component of DQUAT to the existing dual quaternion, OUT."
  (with-components ((o out) (d dquat))
    (q:copy! o.r d.r)
    (q:copy! o.d d.d))
  out)

(declaim (inline copy))
(declaim (ftype (function (dquat) dquat) copy))
(defun copy (dquat)
  "Copy each component of QUAT to a freshly allocated dual quaternion."
  (copy! (id) dquat))

(declaim (inline +!))
(declaim (ftype (function (dquat dquat dquat) dquat) +!))
(defun +! (out dquat1 dquat2)
  "Calculate the sum of DQUAT1 and DQUAT2, storing the result in the existing dual quaternion, OUT."
  (with-components ((o out) (d1 dquat1) (d2 dquat2))
    (q:+! o.r d1.r d2.r)
    (q:+! o.d d1.d d2.d))
  out)

(declaim (inline +))
(declaim (ftype (function (dquat dquat) dquat) +))
(defun + (dquat1 dquat2)
  "Calculate the sum of DQUAT1 and DQUAT2, storing the result in a freshly allocated dual
quaternion."
  (+! (id) dquat1 dquat2))

(declaim (inline -!))
(declaim (ftype (function (dquat dquat dquat) dquat) -!))
(defun -! (out dquat1 dquat2)
  "Calculate the difference of DQUAT2 from DQUAT1, storing the result in the existing dual
quaternion, OUT."
  (with-components ((o out) (d1 dquat1) (d2 dquat2))
    (q:-! o.r d1.r d2.r)
    (q:-! o.d d1.d d2.d))
  out)

(declaim (inline -))
(declaim (ftype (function (dquat dquat) dquat) -))
(defun - (dquat1 dquat2)
  "Calculate the difference of DQUAT2 from DQUAT1, storing the result in a freshly allocated dual
quaternion."
  (-! (id) dquat1 dquat2))

(declaim (ftype (function (dquat dquat dquat) dquat) *!))
(defun *! (out dquat1 dquat2)
  "Calculate the product of DQUAT1 and DQUAT2, storing the result in the existing dual quaternion,
OUT."
  (let ((dual1 (q:zero))
        (dual2 (q:zero)))
    (with-components ((o out) (d1 dquat1) (d2 dquat2))
      (q:*! o.r d1.r d2.r)
      (q:*! dual1 d1.r d2.d)
      (q:*! dual2 d1.d d2.r)
      (q:+! o.d dual1 dual2)))
  out)

(declaim (inline *))
(declaim (ftype (function (dquat dquat) dquat) *))
(defun * (dquat1 dquat2)
  "Calculate the product of DQUAT1 and DQUAT2, storing the result in a freshly allocated dual
quaternion."
  (*! (id) dquat1 dquat2))

(declaim (inline scale!))
(declaim (ftype (function (dquat dquat single-float) dquat) scale!))
(defun scale! (out dquat scalar)
  "Scale DQUAT by SCALAR, storing the result in the existing dual quaternion, OUT."
  (with-components ((o out) (d dquat))
    (q:scale! o.r d.r scalar)
    (q:scale! o.d d.d scalar))
  out)

(declaim (inline dqscale))
(declaim (ftype (function (dquat single-float) dquat) scale))
(defun scale (dquat scalar)
  "Scale DQUAT by SCALAR, storing the result in a freshly allocated dual quaternion."
  (scale! (id) dquat scalar))

(declaim (inline conjugate!))
(declaim (ftype (function (dquat dquat) dquat) conjugate!))
(defun conjugate! (out dquat)
  "Calculate the conjugate of DQUAT, storing the result in the existing dual quaternion, OUT."
  (with-components ((o out) (d dquat))
    (q:conjugate! o.r d.r)
    (q:conjugate! o.d d.d))
  out)

(declaim (inline conjugate))
(declaim (ftype (function (dquat) dquat) conjugate))
(defun conjugate (dquat)
  "Calculate the conjugate of DQUAT, storing the result in a freshly allocated dual quaternion."
  (conjugate! (id) dquat))

(declaim (inline conjugate-full!))
(declaim (ftype (function (dquat dquat) dquat) conjugate-full!))
(defun conjugate-full! (out dquat)
  "Calculate the full conjugate of DQUAT, storing the result in the existing dual quaternion, OUT."
  (with-components ((o out) (d dquat))
    (q:conjugate! o.r d.r)
    (psetf o.dw (cl:- d.dw) o.dx d.dx o.dy d.dy o.dz d.dz))
  out)

(declaim (inline conjugate-full))
(declaim (ftype (function (dquat) dquat) conjugate-full))
(defun conjugate-full (dquat)
  "Calculate the full conjugate of DQUAT, storing the result in a freshly allocated dual
quaternion."
  (conjugate-full! (id) dquat))

(declaim (inline magnitude-squared))
(declaim (ftype (function (dquat) single-float) magnitude-squared))
(defun magnitude-squared (dquat)
  "Calculate the magnitude (also known as length or Euclidean norm) of DQUAT. This results in a
squared value, which is cheaper to compute. It is useful when you want to compare relative lengths,
which does not need the expensive square root function.

See MAGNITUDE for other cases."
  (with-components ((d dquat))
    (q:magnitude-squared d.r)))

(declaim (inline magnitude))
(declaim (ftype (function (dquat) single-float) magnitude))
(defun magnitude (dquat)
  "Compute the magnitude (also known as length or Euclidean norm) of DQUAT.

See MAGNITUDE-SQUARED if you only need to compare lengths, as it is cheaper to compute without the
square root call of this function."
  (sqrt (magnitude-squared dquat)))

(declaim (inline normalize!))
(declaim (ftype (function (dquat dquat) dquat) normalize!))
(defun normalize! (out dquat)
  "Convert DQUAT to be of unit length, storing the result in the existing dual quaternion, OUT."
  (let ((magnitude (magnitude dquat)))
    (unless (zerop magnitude)
      (scale! out dquat (/ magnitude))))
  out)

(declaim (inline normalize))
(declaim (ftype (function (dquat) dquat) normalize))
(defun normalize (dquat)
  "Convert DQUAT to be of unit length, storing the result in a freshly allocated dual quaternion."
  (normalize! (id) dquat))

(declaim (inline negate!))
(declaim (ftype (function (dquat dquat) dquat) negate!))
(defun negate! (out dquat)
  "Negate each component of DQUAT, storing the result in the existing dual quaternion, OUT."
  (scale! out dquat -1.0f0))

(declaim (inline negate))
(declaim (ftype (function (dquat) dquat) negate))
(defun negate (dquat)
  "Negate each component of DQUAT, storing the result in a freshly allocated dual quaternion."
  (negate! (id) dquat))

(declaim (ftype (function (dquat dquat dquat) dquat) apply!))
(defun apply! (out dquat1 dquat2)
  "Apply the sandwich operator to DQUAT1 and DQUAT2, storing the result in the existing dual
quaternion, OUT."
  (let ((dquat2 (normalize dquat2)))
    (*! out (* dquat2 dquat1) (conjugate-full dquat2))))

(declaim (inline apply))
(declaim (ftype (function (dquat dquat) dquat) apply))
(defun apply (dquat1 dquat2)
  "Apply the sandwich operator to DQUAT1 and DQUAT2, storing the result in a freshly allocated dual
quaternion."
  (apply! (id) dquat1 dquat2))

(declaim (inline dot))
(declaim (ftype (function (dquat dquat) single-float) dot))
(defun dot (dquat1 dquat2)
  "Calculate the dot product of DQUAT1 and DQUAT2. Returns a scalar."
  (with-components ((d1 dquat1) (d2 dquat2))
    (q:dot d1.r d2.r)))

(declaim (ftype (function (dquat dquat) dquat) inverse!))
(defun inverse! (out dquat)
  "Calculate the inverse of DQUAT, storing the result in the existing dual quaternion, OUT."
  (with-components ((o out) (d dquat))
    (q:inverse! o.r d.r)
    (q:scale! o.d (q:* o.r (q:* d.d o.r)) -1.0f0))
  out)

(declaim (inline inverse))
(declaim (ftype (function (dquat) dquat) inverse))
(defun inverse (dquat)
  "Calculate the inverse of DQUAT, storing the result in a freshly allocated dual quaternion."
  (inverse! (id) dquat))

(declaim (inline translation-to-vec3!))
(declaim (ftype (function (v3:vec dquat) v3:vec) translation-to-vec3!))
(defun translation-to-vec3! (out dquat)
  "Copy the translation part of DQUAT, storing the result in the existing vector, OUT."
  (let ((s (q:zero))
        (c (q:zero)))
    (v3:with-components ((o out))
      (with-components ((d dquat))
        (q:scale! s d.d 2.0f0)
        (q:conjugate! c d.r)
        (q:with-components ((q (q:* s c)))
          (setf ox qx oy qy oz qz)))))
  out)

(declaim (inline translation-to-vec3))
(declaim (ftype (function (dquat) v3:vec) translation-to-vec3))
(defun translation-to-vec3 (dquat)
  "Copy the translation part of DQUAT, storing the result in a freshly allocated vector."
  (translation-to-vec3! (v3:zero) dquat))

(declaim (inline translation-from-vec3!))
(declaim (ftype (function (dquat v3:vec) dquat) translation-from-vec3!))
(defun translation-from-vec3! (out vec)
  "Copy the components of VEC, storing the result in the existing dual quaternion, OUT."
  (with-components ((o (id! out)))
    (q:from-vec3! o.d vec)
    (q:scale! o.d o.d 0.5f0))
  out)

(declaim (inline translation-from-vec3))
(declaim (ftype (function (v3:vec) dquat) translation-from-vec3))
(defun translation-from-vec3 (vec)
  "Copy the components of VEC, storing the result in a freshly allocated dual quaternion."
  (translation-from-vec3! (zero) vec))

(declaim (inline translate!))
(declaim (ftype (function (dquat dquat v3:vec) dquat) translate!))
(defun translate! (out dquat vec)
  "Translate DQUAT by VEC, storing the result in the existing dual quaternion, OUT."
  (*! out (translation-from-vec3 vec) dquat))

(declaim (inline translate))
(declaim (ftype (function (dquat v3:vec) dquat) translate))
(defun translate (dquat vec)
  "Translate DQUAT by VEC, storing the result in a freshly allocated dual quaternion."
  (translate! (id) dquat vec))

(declaim (inline rotation-to-quat!))
(declaim (ftype (function (q:quat dquat) q:quat) rotation-to-quat!))
(defun rotation-to-quat! (out dquat)
  "Copy the rotation part of DQUAT, storing the result in the existing quaternion, OUT."
  (with-components ((d dquat))
    (q:copy! out d.r))
  out)

(declaim (inline rotation-to-quat))
(declaim (ftype (function (dquat) q:quat) rotation-to-quat))
(defun rotation-to-quat (dquat)
  "Copy the rotation part of DQUAT, storing the result in a freshly allocated quaternion."
  (rotation-to-quat! (q:zero) dquat))

(declaim (inline rotation-from-quat!))
(declaim (ftype (function (dquat q:quat) dquat) rotation-from-quat!))
(defun rotation-from-quat! (out quat)
  "Copy the components of QUAT, storing the result in the existing dual quaternion, OUT."
  (with-components ((o out))
    (q:copy! o.r quat)
    (q:zero! o.d))
  out)

(declaim (inline rotation-from-quat))
(declaim (ftype (function (q:quat) dquat) rotation-from-quat))
(defun rotation-from-quat (quat)
  "Copy the components of QUAT, storing the result in a freshly allocated dual quaternion."
  (rotation-from-quat! (id) quat))

(declaim (ftype (function (dquat dquat v3:vec) dquat) rotate!))
(defun rotate! (out dquat vec)
  "Rotate DQUAT by the vector of Euler angles, VEC, storing the result in the existing dual
quaternion, OUT."
  (with-components ((o out) (d dquat))
    (q:rotate! o.r d.r vec))
  out)

(declaim (ftype (function (dquat v3:vec) dquat) rotate))
(declaim (inline rotate))
(defun rotate (dquat vec)
  "Rotate DQUAT by the vector of Euler angles, VEC, storing the result in a freshly allocated dual
quaternion."
  (rotate! (id) dquat vec))

(declaim (ftype (function (m4:matrix dquat) m4:matrix) to-mat4!))
(defun to-mat4! (out dquat)
  "Convert DQUAT to a 3-dimensional transformation matrix, storing the result in the existing
matrix, OUT."
  (m4:with-components ((o out))
    (with-components ((d dquat))
      (v3:with-components ((v (translation-to-vec3 dquat)))
        (q:to-mat4! o d.r)
        (psetf o03 vx o13 vy o23 vz))))
  out)

(declaim (inline to-mat4))
(declaim (ftype (function (dquat) m4:matrix) to-mat4))
(defun to-mat4 (dquat)
  "Convert DQUAT to a 3-dimensional transformation matrix, storing the result in a freshly allocated
matrix."
  (to-mat4! (m4:id) dquat))

(declaim (ftype (function (dquat m4:matrix) dquat) from-mat4!))
(defun from-mat4! (out matrix)
  "Convert MATRIX to a dual quaternion, storing the result in the existing dual quaternion, OUT."
  (let ((rot (rotation-from-quat (q:from-mat4 matrix)))
        (tr (translation-from-vec3 (m4:translation-to-vec3 matrix))))
    (*! out tr rot))
  out)

(declaim (inline from-mat4))
(declaim (ftype (function (m4:matrix) dquat) from-mat4))
(defun from-mat4 (matrix)
  "Convert MATRIX to a dual quaternion, storing the result in a freshly allocated dual quaternion."
  (from-mat4! (id) matrix))

(declaim (ftype (function (dquat) (values single-float single-float v3:vec v3:vec)) to-screw))
(defun to-screw (dquat)
  "Convert DQUAT to a set of Screw parameters."
  (with-components ((d (normalize dquat)))
    (let* ((angle (cl:* 2 (acos (alexandria:clamp d.rw -1 1))))
           (dir (v3:normalize (q:to-vec3 d.r)))
           (tr (translation-to-vec3 dquat))
           (pitch (v3:dot tr dir))
           (moment (v3:scale (v3:+ (v3:cross tr dir)
                                   (v3:scale (v3:- (v3:scale tr (v3:dot dir dir))
                                                   (v3:scale dir pitch))
                                             (/ (tan (/ angle 2)))))
                             0.5f0)))
      (values angle pitch dir moment))))

(declaim (ftype (function (dquat single-float single-float v3:vec v3:vec) dquat) from-screw!))
(defun from-screw! (out angle pitch direction moment)
  "Convert a set of Screw parameters to a dual quaternion, storing the result in the existing dual
quaternion, OUT."
  (let* ((half-angle (cl:* angle 0.5f0))
         (c (cos half-angle))
         (s (sin half-angle)))
    (v3:with-components ((r (v3:scale direction s))
                         (d (v3:+ (v3:scale moment s) (v3:scale direction (cl:* pitch c 0.5f0)))))
      (setf (dq-real out) (q:make c rx ry rz)
            (dq-dual out) (q:make (cl:- (cl:* pitch s 0.5f0)) dx dy dz))))
  out)

(declaim (inline from-screw))
(declaim (ftype (function (single-float single-float v3:vec v3:vec) dquat) from-screw))
(defun from-screw (angle pitch direction moment)
  "Convert a set of Screw parameters to a dual quaternion, storing the result in a freshly allocated
dual quaternion."
  (from-screw! (id) angle pitch direction moment))

(declaim (ftype (function (dquat dquat dquat single-float) dquat) sclerp!))
(defun sclerp! (out dquat1 dquat2 factor)
  "Perform a 'Screw' spherical linear interpolation between DQUAT1 and DQUAT2 by FACTOR, storing the
result in the existing dual quaternion, OUT."
  (let ((diff (* (inverse dquat1) dquat2)))
    (multiple-value-bind (angle pitch direction moment) (to-screw diff)
      (*! out dquat1 (from-screw (cl:* angle factor) (cl:* pitch factor) direction moment))))
  out)

(declaim (inline sclerp))
(declaim (ftype (function (dquat dquat single-float) dquat) sclerp))
(defun sclerp (dquat1 dquat2 factor)
  "Perform a 'Screw' spherical linear interpolation between DQUAT1 and DQUAT2 by FACTOR, storing the
result in a freshly allocated dual quaternion."
  (sclerp! (id) dquat1 dquat2 factor))

(declaim (ftype (function (dquat dquat dquat single-float) dquat) nlerp!))
(defun nlerp! (out dquat1 dquat2 factor)
  "Perform a normalized linear interpolation between DQUAT1 and DQUAT2 by FACTOR, storing the result
in the existing dual quaternion, OUT."
  (+! out dquat1 (scale (- dquat2 dquat1) factor)))

(declaim (inline nlerp))
(declaim (ftype (function (dquat dquat single-float) dquat) nlerp))
(defun nlerp (dquat1 dquat2 factor)
  "Perform a normalized linear interpolation between DQUAT1 and DQUAT2 by FACTOR, storing the result
in a freshly allocated dual quaternion."
  (nlerp! (id) dquat1 dquat2 factor))
