(in-package #:cl-user)

(defpackage #:box.math.quat
  (:local-nicknames (#:% #:box.math.internal)
                    (#:v3 #:box.math.vec3)
                    (#:v4 #:box.math.vec4)
                    (#:m3 #:box.math.mat3)
                    (#:m4 #:box.math.mat4))
  (:use #:cl)
  (:shadow
   #:=
   #:+
   #:-
   #:*
   #:conjugate
   #:length)
  (:export
   #:quat
   #:with-quaternions
   #:w
   #:x
   #:y
   #:z
   #:+zero+
   #:+id+
   #:id!
   #:id
   #:make
   #:zero!
   #:zero
   #:=
   #:~
   #:copy!
   #:copy
   #:+!
   #:+
   #:-!
   #:-
   #:*!
   #:*
   #:scale!
   #:scale
   #:conjugate!
   #:conjugate
   #:cross!
   #:cross
   #:length-squared
   #:length
   #:normalize!
   #:normalize
   #:negate!
   #:negate
   #:dot
   #:inverse!
   #:inverse
   #:rotate!
   #:rotate
   #:to-vec3!
   #:to-vec3
   #:to-vec4!
   #:to-vec4
   #:from-vec3!
   #:from-vec3
   #:from-vec4!
   #:from-vec4
   #:to-mat3!
   #:to-mat3
   #:to-mat4!
   #:to-mat4
   #:from-mat3!
   #:from-mat3
   #:from-mat4!
   #:from-mat4
   #:slerp!
   #:slerp))

(in-package #:box.math.quat)

;;; Structure

(deftype quat () '(simple-array single-float (4)))

(defstruct (quat (:type (vector single-float))
                 (:constructor %make (w x y z))
                 (:conc-name nil)
                 (:copier nil))
  "A 4-dimensional complex number that describes a 3-dimensional rotation."
  (w 0f0 :type single-float)
  (x 0f0 :type single-float)
  (y 0f0 :type single-float)
  (z 0f0 :type single-float))

(defmacro with-quaternions (((prefix quat) &rest rest) &body body)
  "A convenience macro for concisely accessing the components of quaternions."
  `(with-accessors ((,prefix identity)
                    (,(%::make-accessor-symbol prefix 'w) w)
                    (,(%::make-accessor-symbol prefix 'x) x)
                    (,(%::make-accessor-symbol prefix 'y) y)
                    (,(%::make-accessor-symbol prefix 'z) z))
       ,quat
     ,(if rest
          `(with-quaternions ,rest ,@body)
          `(progn ,@body))))

;;; Constants

(au:define-constant +zero+
    (make-array 4 :element-type 'single-float
                  :initial-contents '(0f0 0f0 0f0 0f0))
  :test #'equalp
  :documentation "A quaternion with each component as zero.")

(au:define-constant +id+
    (make-array 4 :element-type 'single-float
                  :initial-contents '(1f0 0f0 0f0 0f0))
  :test #'equalp
  :documentation "An identity quaternion.")

;;; Operations

(declaim (inline make))
(declaim (ftype (function (real real real real) quat) make))
(defun make (w x y z)
  "Create a new quaternion."
  (%make (float w 1f0) (float x 1f0) (float y 1f0) (float z 1f0)))

(declaim (ftype (function (quat) quat) id!))
(defun id! (quat)
  "Modify QUAT to be an identity quaternion."
  (with-quaternions ((q quat))
    (psetf qw 1f0 qx 0f0 qy 0f0 qz 0f0))
  quat)

(declaim (ftype (function () quat) id))
(defun id ()
  "Create an identity quaternion."
  (id! (make 0f0 0f0 0f0 0f0)))

(declaim (inline zero!))
(declaim (ftype (function (quat) quat) zero!))
(defun zero! (quat)
  "Set each component of QUAT to zero."
  (with-quaternions ((q quat))
    (psetf qw 0f0 qx 0f0 qy 0f0 qz 0f0))
  quat)

(declaim (inline zero))
(declaim (ftype (function () quat) zero))
(defun zero ()
  "Create a new quaternion with all components initialized to zero."
  (make 0f0 0f0 0f0 0f0))

(declaim (inline =))
(declaim (ftype (function (quat quat) boolean) =))
(defun = (quat1 quat2)
  "Check if all components of QUAT1 are numerically equal to the components of
QUAT2."
  (with-quaternions ((q1 quat1)
                     (q2 quat2))
    (and (cl:= q1w q2w)
         (cl:= q1x q2x)
         (cl:= q1y q2y)
         (cl:= q1z q2z))))

(declaim (inline ~))
(declaim (ftype (function (quat quat &key (:tolerance single-float))
                          boolean) ~))
(defun ~ (quat1 quat2 &key (tolerance 1e-7))
  "Check if all components of QUAT1 are approximately equal to the components of
QUAT2, according to TOLERANCE."
  (with-quaternions ((q1 quat1)
                     (q2 quat2))
    (and (%::~ q1w q2w tolerance)
         (%::~ q1x q2x tolerance)
         (%::~ q1y q2y tolerance)
         (%::~ q1z q2z tolerance))))

(declaim (inline copy!))
(declaim (ftype (function (quat quat) quat) copy!))
(defun copy! (out quat)
  "Copy each component of QUAT to the existing quaternion, OUT."
  (with-quaternions ((o out)
                     (q quat))
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
  "Calculate the sum of QUAT1 and QUAT2, storing the result in the existing
quaternion, OUT."
  (with-quaternions ((o out)
                     (q1 quat1)
                     (q2 quat2))
    (psetf ow (cl:+ q1w q2w)
           ox (cl:+ q1x q2x)
           oy (cl:+ q1y q2y)
           oz (cl:+ q1z q2z)))
  out)

(declaim (inline +))
(declaim (ftype (function (quat quat) quat) +))
(defun + (quat1 quat2)
  "Calculate the sum of QUAT1 and QUAT2, storing the result in a freshly
allocated quaternion."
  (+! (id) quat1 quat2))

(declaim (inline -!))
(declaim (ftype (function (quat quat quat) quat) -!))
(defun -! (out quat1 quat2)
  "Calculate the difference of QUAT2 from QUAT1, storing the result in the
existing quaternion, OUT."
  (with-quaternions ((o out)
                     (q1 quat1)
                     (q2 quat2))
    (psetf ow (cl:- q1w q2w)
           ox (cl:- q1x q2x)
           oy (cl:- q1y q2y)
           oz (cl:- q1z q2z)))
  out)

(declaim (inline -))
(declaim (ftype (function (quat quat) quat) -))
(defun - (quat1 quat2)
  "Calculate the difference of QUAT2 from QUAT1, storing the result in a freshly
allocated quaternion."
  (-! (id) quat1 quat2))

(declaim (inline *!))
(declaim (ftype (function (quat quat quat) quat) *!))
(defun *! (out quat1 quat2)
  "Calculate the product of QUAT1 and QUAT2, storing the result in the existing
quaternion, OUT."
  (with-quaternions ((o out)
                     (q1 quat1)
                     (q2 quat2))
    (psetf ow (cl:- (cl:* q1w q2w) (cl:* q1x q2x) (cl:* q1y q2y)
                    (cl:* q1z q2z))
           ox (cl:- (cl:+ (cl:* q1w q2x) (cl:* q1x q2w) (cl:* q1y q2z))
                    (cl:* q1z q2y))
           oy (cl:- (cl:+ (cl:* q1w q2y) (cl:* q1y q2w) (cl:* q1z q2x))
                    (cl:* q1x q2z))
           oz (cl:- (cl:+ (cl:* q1w q2z) (cl:* q1z q2w) (cl:* q1x q2y))
                    (cl:* q1y q2x))))
  out)

(declaim (inline *))
(declaim (ftype (function (quat quat) quat) *))
(defun * (quat1 quat2)
  "Calculate the product of QUAT1 and QUAT2, storing the result in a freshly
allocated quaternion."
  (*! (id) quat1 quat2))

(declaim (inline scale!))
(declaim (ftype (function (quat quat single-float) quat) scale!))
(defun scale! (out quat scalar)
  "Scale QUAT by SCALAR, storing the result in the existing quaternion, OUT."
  (with-quaternions ((o out)
                     (q quat))
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
  "Calculate the conjugate of QUAT, storing the result in the existing
quaternion, OUT."
  (with-quaternions ((o out)
                     (q quat))
    (psetf ow qw
           ox (cl:- qx)
           oy (cl:- qy)
           oz (cl:- qz)))
  out)

(declaim (inline conjugate))
(declaim (ftype (function (quat) quat) conjugate))
(defun conjugate (quat)
  "Calculate the conjugate of QUAT, storing the result in a freshly allocated
quaternion."
  (conjugate! (id) quat))

(declaim (inline cross!))
(declaim (ftype (function (quat quat quat) quat) cross!))
(defun cross! (out quat1 quat2)
  "Calculate the cross product of QUAT1 and QUAT2, storing the result in the
existing quaternion, OUT."
  (scale! out (+ (* quat2 (conjugate quat1)) (* quat1 quat2)) 0.5f0))

(declaim (inline cross))
(declaim (ftype (function (quat quat) quat) cross))
(defun cross (quat1 quat2)
  "Calculate the cross product of QUAT1 and QUAT2, storing the result in a
freshly allocated quaternion."
  (cross! (id) quat1 quat2))

(declaim (inline length-squared))
(declaim (ftype (function (quat) single-float) length-squared))
(defun length-squared (quat)
  "Calculate the magnitude (also known as length or Euclidean norm) of QUAT.
This results in a squared value, which is cheaper to compute. It is useful when
you want to compare relative lengths, which does not need the expensive square
root function.

See LENGTH for other cases."
  (with-quaternions ((q quat))
    (cl:+ (cl:* qw qw) (cl:* qx qx) (cl:* qy qy) (cl:* qz qz))))

(declaim (inline length))
(declaim (ftype (function (quat) single-float) length))
(defun length (quat)
  "Compute the magnitude (also known as length or Euclidean norm) of QUAT.

See LENGTH-SQUARED if you only need to compare lengths, as it is cheaper to
compute without the square root call of this function."
  (sqrt (length-squared quat)))

(declaim (inline normalize!))
(declaim (ftype (function (quat quat) quat) normalize!))
(defun normalize! (out quat)
  "Convert QUAT to be of unit length, storing the result in the existing
quaternion, OUT."
  (let ((length (length quat)))
    (unless (zerop length)
      (scale! out quat (/ length))))
  out)

(declaim (inline normalize))
(declaim (ftype (function (quat) quat) normalize))
(defun normalize (quat)
  "Convert QUAT to be of unit length, storing the result in a freshly allocated
quaternion."
  (normalize! (id) quat))

(declaim (inline negate!))
(declaim (ftype (function (quat quat) quat) negate!))
(defun negate! (out quat)
  "Negate each component of QUAT, storing the result in the existing quaternion,
OUT."
  (scale! out quat -1f0))

(declaim (inline negate))
(declaim (ftype (function (quat) quat) negate))
(defun negate (quat)
  "Negate each component of QUAT, storing the result in a freshly allocated
quaternion."
  (negate! (id) quat))

(declaim (inline dot))
(declaim (ftype (function (quat quat) single-float) dot))
(defun dot (quat1 quat2)
  "Calculate the dot product of QUAT1 and QUAT2. Returns a scalar."
  (with-quaternions ((q1 quat1)
                     (q2 quat2))
    (cl:+ (cl:* q1w q2w) (cl:* q1x q2x) (cl:* q1y q2y) (cl:* q1z q2z))))

(declaim (inline inverse!))
(declaim (ftype (function (quat quat) quat) inverse!))
(defun inverse! (out quat)
  "Calculate the inverse of QUAT, storing the result in the existing quaternion,
OUT."
  (conjugate! out quat)
  (scale! out out (/ (length-squared quat)))
  out)

(declaim (inline inverse))
(declaim (ftype (function (quat) quat) inverse))
(defun inverse (quat)
  "Calculate the inverse of QUAT, storing the result in a freshly allocated
quaternion."
  (inverse! (id) quat))

(declaim (inline rotate!))
(declaim (ftype (function (quat quat v3:vec &key (:space keyword)) quat)
                rotate!))
(defun rotate! (out quat vec &key (space :local))
  "Rotate QUAT by the vector of Euler angles, VEC, storing the result in the
existing quaternion, OUT."
  (with-quaternions ((o out)
                     (q (copy quat)))
    (v3:with-vectors ((v (v3:scale vec 0.5f0))
                      (c (v3:make (cos vx) (cos vy) (cos vz)))
                      (s (v3:make (sin vx) (sin vy) (sin vz))))
      (psetf ow (cl:- (cl:* cx cy cz) (cl:* sx sy sz))
             ox (cl:+ (cl:* sx cy cz) (cl:* cx sy sz))
             oy (cl:- (cl:* cx sy cz) (cl:* sx cy sz))
             oz (cl:+ (cl:* sx sy cz) (cl:* cx cy sz)))
      (ecase space
        (:local (*! out q out))
        (:world (*! out out q)))))
  out)

(declaim (ftype (function (quat v3:vec &key (:space keyword)) quat) rotate))
(defun rotate (quat vec &key (space :local))
  "Rotate QUAT by the vector of Euler angles, VEC, storing the result in a
freshly allocated quaternion."
  (rotate! (id) quat vec :space space))

(declaim (inline to-vec3!))
(declaim (ftype (function (v3:vec quat) v3:vec) to-vec3!))
(defun to-vec3! (out quat)
  "Copy the imaginary components of QUAT to the existing vector, OUT."
  (v3:with-vectors ((v out))
    (with-quaternions ((q quat))
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
  (v4:with-vectors ((v out))
    (with-quaternions ((q quat))
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
  "Copy the components of VEC to the imaginary components of the existing
quaternion, OUT."
  (with-quaternions ((q out))
    (v3:with-vectors ((v vec))
      (setf qw 0f0 qx vx qy vy qz vz)))
  out)

(declaim (inline from-vec3))
(declaim (ftype (function (v3:vec) quat) from-vec3))
(defun from-vec3 (vec)
  "Copy the components of VEC to the imaginary components of a freshly allocated
quaternion."
  (from-vec3! (zero) vec))

(declaim (inline from-vec4!))
(declaim (ftype (function (quat v4:vec) quat) from-vec4!))
(defun from-vec4! (out vec)
  "Copy the components of VEC to the existing quaternion, OUT."
  (with-quaternions ((q out))
    (v4:with-vectors ((v vec))
      (setf qw vx qx vy qy vz qz vw)))
  out)

(declaim (inline v4->q))
(declaim (ftype (function (v4:vec) quat) from-vec4))
(defun from-vec4 (vec)
  "Copy the components of VEC to a freshly allocated quaternion."
  (from-vec4! (zero) vec))

(declaim (inline to-mat4!))
(declaim (ftype (function (m4:matrix quat) m4:matrix) to-mat4!))
(defun to-mat3! (out quat)
  "Convert QUAT to a mat3, storing the result in the existing matrix, OUT."
  (m3:with-matrices ((o out))
    (with-quaternions ((q quat))
      (let* ((s (cl:/ 2 (length-squared quat)))
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
               o01 (cl:+ xy wz)
               o02 (cl:- xz wy)
               o10 (cl:- xy wz)
               o11 (cl:- 1 (cl:+ xx zz))
               o12 (cl:+ yz wx)
               o20 (cl:+ xz wy)
               o21 (cl:- yz wx)
               o22 (cl:- 1 (cl:+ xx yy))))))
  out)

(declaim (inline to-mat3))
(declaim (ftype (function (quat) m3:matrix) to-mat3))
(defun to-mat3 (quat)
  "Convert QUAT to a mat3, storing the result in a freshly allocated matrix."
  (to-mat3! (m3:id) quat))

(declaim (inline to-mat4!))
(declaim (ftype (function (m4:matrix quat) m4:matrix) to-mat4!))
(defun to-mat4! (out quat)
  "Convert QUAT to a mat4, storing the result in the existing matrix, OUT."
  (m4:with-matrices ((o out))
    (with-quaternions ((q quat))
      (let* ((s (/ 2 (length-squared quat)))
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
               o03 0f0
               o10 (cl:+ xy wz)
               o11 (cl:- 1 (cl:+ xx zz))
               o12 (cl:- yz wx)
               o13 0f0
               o20 (cl:- xz wy)
               o21 (cl:+ yz wx)
               o22 (cl:- 1 (cl:+ xx yy))
               o23 0f0
               o30 0f0
               o31 0f0
               o32 0f0
               o33 1f0))))
  out)

(declaim (inline to-mat4))
(declaim (ftype (function (quat) m4:matrix) to-mat4))
(defun to-mat4 (quat)
  "Convert QUAT to a mat4, storing the result in a freshly allocated matrix."
  (to-mat4! (m4:id) quat))

(declaim (ftype (function (quat m3:matrix) quat) from-mat3!))
(defun from-mat3! (out matrix)
  (with-quaternions ((o out))
    (m3:with-matrices ((m matrix))
      (let* ((x-rot-denom (sqrt
                           (cl:+
                            (cl:* m00 m00) (cl:* m10 m10) (cl:* m20 m20))))
             (y-rot-denom (sqrt
                           (cl:+
                            (cl:* m01 m01) (cl:* m11 m11) (cl:* m21 m21))))
             (z-rot-denom (sqrt
                           (cl:+
                            (cl:* m02 m02) (cl:* m12 m12) (cl:* m22 m22))))
             (nm00 (/ m00 x-rot-denom))
             (nm10 (/ m10 x-rot-denom))
             (nm20 (/ m20 x-rot-denom))
             (nm01 (/ m01 y-rot-denom))
             (nm11 (/ m11 y-rot-denom))
             (nm21 (/ m21 y-rot-denom))
             (nm02 (/ m02 z-rot-denom))
             (nm12 (/ m12 z-rot-denom))
             (nm22 (/ m22 z-rot-denom)))
        (let ((trace (cl:+ nm00 nm11 nm22 1f0))
              (col1 (1+ (cl:- nm00 nm11 nm22)))
              (col2 (1+ (cl:- nm11 nm00 nm22)))
              (col3 (1+ (cl:- nm22 nm00 nm11)))
              (s 0f0))
          (cond
            ((plusp trace)
             (setf s (/ 0.5f0 (sqrt trace))
                   ow (/ 0.25f0 s)
                   ox (cl:* (cl:- nm21 nm12) s)
                   oy (cl:* (cl:- nm02 nm20) s)
                   oz (cl:* (cl:- nm10 nm01) s)))
            ((and (>= col1 col2) (>= col1 col3))
             (setf s (/ 0.5f0 (sqrt col1))
                   ow (cl:* (cl:- nm21 nm12) s)
                   ox (/ 0.25f0 s)
                   oy (cl:* (cl:+ nm10 nm01) s)
                   oz (cl:* (cl:+ nm02 nm20) s)))
            ((and (>= col2 col1) (>= col2 col3))
             (setf s (/ 0.5f0 (sqrt col2))
                   ow (cl:* (cl:- nm02 nm20) s)
                   ox (cl:* (cl:+ nm01 nm10) s)
                   oy (/ 0.25f0 s)
                   oz (cl:* (cl:+ nm12 nm21) s)))
            (t
             (setf s (/ 0.5f0 (sqrt col3))
                   ow (cl:* (cl:- nm10 nm01) s)
                   ox (cl:* (cl:+ nm02 nm20) s)
                   oy (cl:* (cl:+ nm12 nm21) s)
                   oz (/ 0.25f0 s)))))))
    o))

(declaim (inline from-mat3))
(declaim (ftype (function (m3:matrix) quat) from-mat3))
(defun from-mat3 (matrix)
  "Convert MATRIX to a quaternion, storing the result in a freshly allocated
quaternion."
  (from-mat3! (id) matrix))

(declaim (inline from-mat4))
(declaim (ftype (function (quat m4:matrix) quat) from-mat4!))
(defun from-mat4! (out matrix)
  (with-quaternions ((o out))
    (m4:with-matrices ((m matrix))
      (let* ((x-rot-denom (sqrt
                           (cl:+
                            (cl:* m00 m00) (cl:* m10 m10) (cl:* m20 m20))))
             (y-rot-denom (sqrt
                           (cl:+
                            (cl:* m01 m01) (cl:* m11 m11) (cl:* m21 m21))))
             (z-rot-denom (sqrt
                           (cl:+
                            (cl:* m02 m02) (cl:* m12 m12) (cl:* m22 m22))))
             (nm00 (/ m00 x-rot-denom))
             (nm10 (/ m10 x-rot-denom))
             (nm20 (/ m20 x-rot-denom))
             (nm01 (/ m01 y-rot-denom))
             (nm11 (/ m11 y-rot-denom))
             (nm21 (/ m21 y-rot-denom))
             (nm02 (/ m02 z-rot-denom))
             (nm12 (/ m12 z-rot-denom))
             (nm22 (/ m22 z-rot-denom)))
        (let ((trace (cl:+ nm00 nm11 nm22 m33))
              (col1 (1+ (cl:- nm00 nm11 nm22)))
              (col2 (1+ (cl:- nm11 nm00 nm22)))
              (col3 (1+ (cl:- nm22 nm00 nm11)))
              (s 0f0))
          (cond
            ((plusp trace)
             (setf s (/ 0.5f0 (sqrt trace))
                   ow (/ 0.25f0 s)
                   ox (cl:* (cl:- nm21 nm12) s)
                   oy (cl:* (cl:- nm02 nm20) s)
                   oz (cl:* (cl:- nm10 nm01) s)))
            ((and (>= col1 col2) (>= col1 col3))
             (setf s (/ 0.5f0 (sqrt col1))
                   ow (cl:* (cl:- nm21 nm12) s)
                   ox (/ 0.25f0 s)
                   oy (cl:* (cl:+ nm10 nm01) s)
                   oz (cl:* (cl:+ nm02 nm20) s)))
            ((and (>= col2 col1) (>= col2 col3))
             (setf s (/ 0.5f0 (sqrt col2))
                   ow (cl:* (cl:- nm02 nm20) s)
                   ox (cl:* (cl:+ nm01 nm10) s)
                   oy (/ 0.25f0 s)
                   oz (cl:* (cl:+ nm12 nm21) s)))
            (t
             (setf s (/ 0.5f0 (sqrt col3))
                   ow (cl:* (cl:- nm10 nm01) s)
                   ox (cl:* (cl:+ nm02 nm20) s)
                   oy (cl:* (cl:+ nm12 nm21) s)
                   oz (/ 0.25f0 s)))))))
    o))

(declaim (ftype (function (m4:matrix) quat) from-mat4))
(defun from-mat4 (matrix)
  "Convert MATRIX to a quaternion, storing the result in a freshly allocated
quaternion."
  (from-mat4! (id) matrix))

(declaim (ftype (function (quat quat quat single-float) quat) slerp!))
(defun slerp! (out quat1 quat2 factor)
  "Perform a spherical linear interpolation between QUAT1 and QUAT2 by FACTOR,
storing the result in the existing quaternion, OUT."
  (with-quaternions ((o out)
                     (q1 quat1)
                     (q2 quat2))
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
  "Perform a spherical linear interpolation between QUAT1 and QUAT2 by FACTOR,
storing the result in a freshly allocated quaternion."
  (slerp! (id) quat1 quat2 factor))
