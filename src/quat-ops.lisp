(in-package #:net.mfiano.lisp.origin.quat)

;;; type

(deftype quat () '(simple-array single-float (4)))

;;; accessors

(int:define-op w ((quat quat)) (:out single-float)
  (aref quat 0))

(int:define-op (setf w) ((value single-float) (quat quat)) (:out single-float)
  (setf (aref quat 0) value))

(int:define-op x ((quat quat)) (:out single-float)
  (aref quat 1))

(int:define-op (setf x) ((value single-float) (quat quat)) (:out single-float)
  (setf (aref quat 1) value))

(int:define-op y ((quat quat)) (:out single-float)
  (aref quat 2))

(int:define-op (setf y) ((value single-float) (quat quat)) (:out single-float)
  (setf (aref quat 2) value))

(int:define-op z ((quat quat)) (:out single-float)
  (aref quat 3))

(int:define-op (setf z) ((value single-float) (quat quat)) (:out single-float)
  (setf (aref quat 3) value))

;;; constructors

(int:define-op %quat (&rest (args single-float)) (:inline t :out quat)
  (make-array 4 :element-type 'single-float :initial-contents args))

(ss:defstore quat (&rest args))

(ss:defspecialization (quat :inline t) () quat
  (%quat 0f0 0f0 0f0 0f0))

(ss:defspecialization (quat :inline t) ((w real)) quat
  (%quat (float w 1f0) 0f0 0f0 0f0))

(ss:defspecialization (quat :inline t) ((w real) (x real) (y real) (z real))
    quat
  (%quat (float w 1f0) (float x 1f0) (float y 1f0) (float z 1f0)))

(ss:defspecialization (quat :inline t) ((xyz v3:vec)) quat
  (v3:with-components ((v xyz))
    (%quat 0f0 vx vy vz)))

(ss:defspecialization (quat :inline t) ((xyzw v4:vec)) quat
  (v4:with-components ((v xyzw))
    (%quat vx vy vz vw)))

(ss:defspecialization (quat :inline t)
    ((quat net.mfiano.lisp.origin.dquat:quat))
    quat
  (net.mfiano.lisp.origin.dquat:with-components ((q quat))
    (%quat (float qw 1f0) (float qx 1f0) (float qy 1f0) (float qz 1f0))))

;;; constants

(u:define-constant +zero+ (%quat 0f0 0f0 0f0 0f0) :test #'equalp)

(u:define-constant +id+ (%quat 1f0 0f0 0f0 0f0) :test #'equalp)

;;; operators

(int:define-op id! ((in quat)) (:out quat)
  (with-components ((q in))
    (setf qw 1f0 qx 0f0 qy 0f0 qz 0f0))
  in)

(int:define-op id-p ((in quat)) (:out boolean)
  (with-components ((q in))
    (and (cl:= qw 1f0)
         (cl:= qx qy qz 0f0))))

(int:define-op zero! ((in quat)) (:out quat)
  (with-components ((q in))
    (setf qw 0f0 qx 0f0 qy 0f0 qz 0f0))
  in)

(int:define-op = ((in1 quat) (in2 quat)
                  &key (rel single-float 1e-7) (abs single-float rel))
    (:out boolean)
  (with-components ((q1 in1) (q2 in2))
    (and (<= (abs (cl:- q1w q2w))
             (max abs (cl:* rel (max (abs q1w) (abs q2w)))))
         (<= (abs (cl:- q1x q2x))
             (max abs (cl:* rel (max (abs q1x) (abs q2x)))))
         (<= (abs (cl:- q1y q2y))
             (max abs (cl:* rel (max (abs q1y) (abs q2y)))))
         (<= (abs (cl:- q1z q2z))
             (max abs (cl:* rel (max (abs q1z) (abs q2z))))))))

(int:define-op random! ((out quat) (min single-float) (max single-float))
    (:out quat)
  (let ((diff (cl:- max min)))
    (with-components ((o out))
      (setf ow (cl:+ min (cl:random diff))
            ox (cl:+ min (cl:random diff))
            oy (cl:+ min (cl:random diff))
            oz (cl:+ min (cl:random diff)))))
  out)

(int:define-op random ((min single-float) (max single-float)) (:out quat)
  (random! (quat) min max))

(int:define-op copy! ((out quat) (in quat)) (:out quat)
  (with-components ((o out) (q in))
    (setf ow qw ox qx oy qy oz qz))
  out)

(int:define-op copy ((in quat)) (:out quat)
  (copy! (quat 1) in))

(int:define-op +! ((out quat) (in1 quat) (in2 quat)) (:out quat)
  (with-components ((o out) (q1 in1) (q2 in2))
    (setf ow (cl:+ q1w q2w)
          ox (cl:+ q1x q2x)
          oy (cl:+ q1y q2y)
          oz (cl:+ q1z q2z)))
  out)

(int:define-op + ((in1 quat) (in2 quat)) (:out quat)
  (+! (quat 1) in1 in2))

(int:define-op -! ((out quat) (in1 quat) (in2 quat)) (:out quat)
  (with-components ((o out) (q1 in1) (q2 in2))
    (setf ow (cl:- q1w q2w)
          ox (cl:- q1x q2x)
          oy (cl:- q1y q2y)
          oz (cl:- q1z q2z)))
  out)

(int:define-op - ((in1 quat) (in2 quat)) (:out quat)
  (-! (quat 1) in1 in2))

(defmacro %* (ow ox oy oz q1w q1x q1y q1z q2w q2x q2y q2z)
  `(setf ,ow (cl:- (cl:* ,q1w ,q2w) (cl:* ,q1x ,q2x) (cl:* ,q1y ,q2y)
                   (cl:* ,q1z ,q2z))
         ,ox (cl:- (cl:+ (cl:* ,q1w ,q2x) (cl:* ,q1x ,q2w) (cl:* ,q1y ,q2z))
                   (cl:* ,q1z ,q2y))
         ,oy (cl:- (cl:+ (cl:* ,q1w ,q2y) (cl:* ,q1y ,q2w) (cl:* ,q1z ,q2x))
                   (cl:* ,q1x ,q2z))
         ,oz (cl:- (cl:+ (cl:* ,q1w ,q2z) (cl:* ,q1z ,q2w) (cl:* ,q1x ,q2y))
                   (cl:* ,q1y ,q2x))))

(int:define-op *! ((out quat) (in1 quat) (in2 quat)) (:out quat)
  (with-components ((o out) (q1 in1) (q2 in2))
    (%* ow ox oy oz q1w q1x q1y q1z q2w q2x q2y q2z))
  out)

(int:define-op * ((in1 quat) (in2 quat)) (:out quat)
  (*! (quat 1) in1 in2))

(defmacro %scale (ow ox oy oz w x y z scalar)
  `(setf ,ow (cl:* ,w ,scalar)
         ,ox (cl:* ,x ,scalar)
         ,oy (cl:* ,y ,scalar)
         ,oz (cl:* ,z ,scalar)))

(int:define-op scale! ((out quat) (in quat) (scalar single-float)) (:out quat)
  (with-components ((o out) (v in))
    (%scale ow ox oy oz vw vx vy vz scalar))
  out)

(int:define-op scale ((in quat) (scalar float)) (:out quat)
  (scale! (quat) in scalar))

(int:define-op conjugate! ((out quat) (in quat)) (:out quat)
  (with-components ((o out) (q in))
    (setf ow qw
          ox (cl:- qx)
          oy (cl:- qy)
          oz (cl:- qz)))
  out)

(int:define-op conjugate ((in quat)) (:out quat)
  (conjugate! (quat 1) in))

(int:define-op cross! ((out quat) (in1 quat) (in2 quat)) (:out quat)
  (scale! out (+ (* in2 (conjugate in1)) (* in1 in2)) 0.5f0))

(int:define-op cross ((in1 quat) (in2 quat)) (:out quat)
  (cross! (quat 1) in1 in2))

(int:define-op length-squared ((in quat)) (:out single-float)
  (with-components ((q in))
    (cl:+ (expt qw 2) (expt qx 2) (expt qy 2) (expt qz 2))))

(int:define-op length ((in quat)) (:out single-float)
  (sqrt (length-squared in)))

(int:define-op normalize! ((out quat) (in quat)) (:out quat)
  (let ((length (length in)))
    (unless (zerop length)
      (scale! out in (/ length))))
  out)

(int:define-op normalize ((in quat)) (:out quat)
  (normalize! (quat 1) in))

(int:define-op negate! ((out quat) (in quat)) (:out quat)
  (scale! out in -1f0))

(int:define-op negate ((in quat)) (:out quat)
  (negate! (quat 1) in))

(int:define-op dot ((in1 quat) (in2 quat)) (:out single-float)
  (with-components ((q1 in1) (q2 in2))
    (cl:+ (cl:* q1w q2w) (cl:* q1x q2x) (cl:* q1y q2y) (cl:* q1z q2z))))

(int:define-op inverse! ((out quat) (in quat)) (:out quat)
  (conjugate! out in)
  (scale! out out (/ (length-squared in)))
  out)

(int:define-op inverse ((in quat)) (:out quat)
  (inverse! (quat 1) in))

(int:define-op rotate-euler! ((out quat) (in quat) (vec v3:vec)
                              &key (space keyword :local))
    (:out quat :inline nil)
  (with-components ((o out) (q in))
    (v3:with-components ((v vec))
      (v3:with-elements ((v (cl:* vx 0.5) (cl:* vy 0.5) (cl:* vz 0.5))
                         (c (cos vx) (cos vy) (cos vz))
                         (s (sin vx) (sin vy) (sin vz)))
        (with-elements ((r (cl:- (cl:* cx cy cz)
                                 (cl:* sx sy sz))
                           (cl:+ (cl:* sx cy cz)
                                 (cl:* cx sy sz))
                           (cl:- (cl:* cx sy cz)
                                 (cl:* sx cy sz))
                           (cl:+ (cl:* sx sy cz)
                                 (cl:* cx cy sz)))
                        (q1 0f0 0f0 0f0 0f0)
                        (q2 0f0 0f0 0f0 0f0))
          (ecase space
            (:local
             (setf q1w qw q1x qx q1y qy q1z qz
                   q2w rw q2x rx q2y ry q2z rz))
            (:world
             (setf q1w rw q1x rx q1y ry q1z rz
                   q2w qw q2x qx q2y qy q2z qz)))
          (%* ow ox oy oz q1w q1x q1y q1z q2w q2x q2y q2z)))))
  out)

(int:define-op rotate-euler ((in quat) (vec v3:vec) &key (space keyword :local))
    (:out quat)
  (rotate-euler! (quat 1) in vec :space space))

(int:define-op rotate! ((out quat) (in1 quat) (in2 quat)
                        &key (space keyword :local))
    (:out quat :inline nil)
  (ecase space
    (:local
     (*! out in1 in2))
    (:world
     (*! out in2 in1)))
  (normalize! out out))

(int:define-op rotate ((in1 quat) (in2 quat) &key (space keyword :local))
    (:out quat)
  (rotate! (quat 1) in1 in2 :space space))

(int:define-op to-euler! ((out v3:vec) (in quat)) (:out v3:vec)
  (with-components ((q in))
    (let* ((sinr-cosp (cl:* 2f0 (cl:+ (cl:* qw qx) (cl:* qy qz))))
           (y2 (cl:* qy qy))
           (cosr-cosp (cl:- 1f0 (cl:* 2f0 (cl:+ (cl:* qx qx) y2))))
           (roll (atan sinr-cosp cosr-cosp))
           (sinp (cl:* 2f0 (cl:- (cl:* qw qy) (cl:* qz qx))))
           (pitch (if (>= (abs sinp) 1f0)
                      (cl:* const:pi/2 (signum sinp))
                      (asin (the (single-float -1f0 1f0) sinp))))
           (siny-cosp (cl:* 2f0 (cl:+ (cl:* qw qz) (cl:* qx qy))))
           (cosy-cosp (cl:- 1f0 (cl:* 2f0 (cl:+ y2 (cl:* qz qz)))))
           (yaw (atan siny-cosp cosy-cosp)))
      (v3:with-components ((o out))
        (setf ox roll
              oy pitch
              oz yaw))))
  out)

(int:define-op to-euler ((in quat)) (:out v3:vec)
  (to-euler! (v3:vec) in))

(int:define-op to-mat3! ((out m3:mat) (in quat)) (:out m3:mat)
  (let ((s (/ 2 (length-squared in))))
    (m3:with-components ((o out))
      (with-components ((q in))
        (v3:with-elements ((s (cl:* qx s) (cl:* qy s) (cl:* qz s))
                           (a (cl:* qx sx) (cl:* qx sy) (cl:* qx sz))
                           (b (cl:* qy sy) (cl:* qy sz) (cl:* qz sz))
                           (c (cl:* qw sx) (cl:* qw sy) (cl:* qw sz)))
          (setf o00 (cl:- 1 (cl:+ bx bz))
                o01 (cl:- ay cz)
                o02 (cl:+ az cy)
                o10 (cl:+ ay cz)
                o11 (cl:- 1 (cl:+ ax bz))
                o12 (cl:- bz cx)
                o20 (cl:- az cy)
                o21 (cl:+ by cx)
                o22 (cl:- 1 (cl:+ ax bx)))))))
  out)

(int:define-op to-mat3 ((in quat)) (:out m3:mat)
  (to-mat3! (m3:mat 1) in))

(int:define-op to-mat4! ((out m4:mat) (in quat)) (:out m4:mat)
  (let ((s (/ 2 (length-squared in))))
    (m4:with-components ((o out))
      (with-components ((q in))
        (v3:with-elements ((s (cl:* qx s) (cl:* qy s) (cl:* qz s))
                           (a (cl:* qx sx) (cl:* qx sy) (cl:* qx sz))
                           (b (cl:* qy sy) (cl:* qy sz) (cl:* qz sz))
                           (c (cl:* qw sx) (cl:* qw sy) (cl:* qw sz)))
          (setf o00 (cl:- 1 (cl:+ bx bz))
                o01 (cl:- ay cz)
                o02 (cl:+ az cy)
                o03 0f0
                o10 (cl:+ ay cz)
                o11 (cl:- 1 (cl:+ ax bz))
                o12 (cl:- by cx)
                o13 0f0
                o20 (cl:- az cy)
                o21 (cl:+ by cx)
                o22 (cl:- 1 (cl:+ ax bx))
                o23 0f0
                o30 0f0
                o31 0f0
                o32 0f0
                o33 1f0)))))
  out)

(int:define-op to-mat4 ((in quat)) (:out m4:mat)
  (to-mat4! (m4:mat 1) in))

(int:define-op from-mat3! ((out quat) (in m3:mat)) (:out quat :inline nil)
  (with-components ((o out))
    (m3:with-components ((m in))
      (v3:with-elements ((r (v3::%length m00 m10 m20)
                            (v3::%length m01 m11 m21)
                            (v3::%length m02 m12 m22)))
        (m3:with-elements ((nm (/ m00 rx) (/ m01 ry) (/ m02 rz)
                               (/ m10 rx) (/ m11 ry) (/ m12 rz)
                               (/ m20 rx) (/ m21 ry) (/ m22 rz)))
          (let ((trace (m4::%trace nm00 nm11 nm22 1f0))
                (col1 (1+ (cl:- nm00 nm11 nm22)))
                (col2 (1+ (cl:- nm11 nm00 nm22)))
                (col3 (1+ (cl:- nm22 nm00 nm11))))
            (declare ((single-float 0f0) col1 col2 col3))
            (cond
              ((plusp trace)
               (let ((s (/ 0.5f0 (sqrt trace))))
                 (setf ow (/ 0.25f0 s)
                       ox (cl:* (cl:- nm21 nm12) s)
                       oy (cl:* (cl:- nm02 nm20) s)
                       oz (cl:* (cl:- nm10 nm01) s))))
              ((and (>= col1 col2) (>= col1 col3))
               (let ((s (/ 0.5f0 (sqrt col1))))
                 (setf ow (cl:* (cl:- nm21 nm12) s)
                       ox (/ 0.25f0 s)
                       oy (cl:* (cl:+ nm10 nm01) s)
                       oz (cl:* (cl:+ nm02 nm20) s))))
              ((and (>= col2 col1) (>= col2 col3))
               (let ((s (/ 0.5f0 (sqrt col2))))
                 (setf ow (cl:* (cl:- nm02 nm20) s)
                       ox (cl:* (cl:+ nm01 nm10) s)
                       oy (/ 0.25f0 s)
                       oz (cl:* (cl:+ nm12 nm21) s))))
              (t
               (let ((s (/ 0.5f0 (sqrt col3))))
                 (setf ow (cl:* (cl:- nm10 nm01) s)
                       ox (cl:* (cl:+ nm02 nm20) s)
                       oy (cl:* (cl:+ nm12 nm21) s)
                       oz (/ 0.25f0 s))))))))))
  out)

(int:define-op from-mat3 ((in m3:mat)) (:out quat)
  (from-mat3! (quat 1) in))

(int:define-op from-mat4! ((out quat) (in m4:mat)) (:out quat)
  (with-components ((o out))
    (m4:with-components ((m in))
      (v3:with-elements ((r (v3::%length m00 m10 m20)
                            (v3::%length m01 m11 m21)
                            (v3::%length m02 m12 m22)))
        (m3:with-elements ((nm (/ m00 rx) (/ m01 ry) (/ m02 rz)
                               (/ m10 rx) (/ m11 ry) (/ m12 rz)
                               (/ m20 rx) (/ m21 ry) (/ m22 rz)))
          (let* ((trace (m4::%trace nm00 nm11 nm22 m33))
                 (col1 (1+ (cl:- nm00 nm11 nm22)))
                 (col2 (1+ (cl:- nm11 nm00 nm22)))
                 (col3 (1+ (cl:- nm22 nm00 nm11))))
            (declare ((single-float 0f0) col1 col2 col3))
            (cond
              ((plusp trace)
               (let ((s (/ 0.5f0 (sqrt trace))))
                 (setf ow (/ 0.25f0 s)
                       ox (cl:* (cl:- nm21 nm12) s)
                       oy (cl:* (cl:- nm02 nm20) s)
                       oz (cl:* (cl:- nm10 nm01) s))))
              ((and (>= col1 col2) (>= col1 col3))
               (let ((s (/ 0.5f0 (sqrt col1))))
                 (setf ow (cl:* (cl:- nm21 nm12) s)
                       ox (/ 0.25f0 s)
                       oy (cl:* (cl:+ nm10 nm01) s)
                       oz (cl:* (cl:+ nm02 nm20) s))))
              ((and (>= col2 col1) (>= col2 col3))
               (let ((s (/ 0.5f0 (sqrt col2))))
                 (setf ow (cl:* (cl:- nm02 nm20) s)
                       ox (cl:* (cl:+ nm01 nm10) s)
                       oy (/ 0.25f0 s)
                       oz (cl:* (cl:+ nm12 nm21) s))))
              (t
               (let ((s (/ 0.5f0 (sqrt col3))))
                 (setf ow (cl:* (cl:- nm10 nm01) s)
                       ox (cl:* (cl:+ nm02 nm20) s)
                       oy (cl:* (cl:+ nm12 nm21) s)
                       oz (/ 0.25f0 s)))))))))
    o))

(int:define-op from-mat4 ((in m4:mat)) (:out quat)
  (from-mat4! (quat 1) in))

(int:define-op slerp! ((out quat) (in1 quat) (in2 quat) (factor single-float))
    (:out quat :inline nil)
  (let ((factor factor))
    (with-components ((o out) (q1 in1) (q2 in2))
      (let ((dot (dot q1 q2))
            (q2 q2))
        (when (minusp dot)
          (negate! q2 q2)
          (setf dot (cl:- dot)))
        (if (> (abs dot) 0.9995f0)
            (setf ow (u:lerp factor q1w q2w)
                  ox (u:lerp factor q1x q2x)
                  oy (u:lerp factor q1y q2y)
                  oz (u:lerp factor q1z q2z))
            (let* ((angle (acos (the (single-float -0.9995 0.9995) dot)))
                   (sin-angle (sin angle))
                   (scale1 (/ (sin (cl:* angle (cl:- 1 factor))) sin-angle))
                   (scale2 (/ (sin (cl:* factor angle)) sin-angle)))
              (setf ow (cl:+ (cl:* q1w scale1) (cl:* q2w scale2))
                    ox (cl:+ (cl:* q1x scale1) (cl:* q2x scale2))
                    oy (cl:+ (cl:* q1y scale1) (cl:* q2y scale2))
                    oz (cl:+ (cl:* q1z scale1) (cl:* q2z scale2))))))))
  out)

(int:define-op slerp ((in1 quat) (in2 quat) (factor float)) (:out quat)
  (slerp! (quat 1) in1 in2 factor))

(defmacro %from-axis-angle (qw qx qy qz vx vy vz angle)
  (u:with-gensyms (half-angle c s)
    `(let* ((,half-angle (cl:* ,angle 0.5))
            (,c (cos ,half-angle))
            (,s (sin ,half-angle)))
       (setf ,qw ,c
             ,qx (cl:* ,vx ,s)
             ,qy (cl:* ,vy ,s)
             ,qz (cl:* ,vz ,s)))))

(int:define-op from-axis-angle! ((out quat) (axis v3:vec) (angle single-float))
    (:out quat)
  (with-components ((o out))
    (v3:with-components ((v axis))
      (%from-axis-angle ow ox oy oz vx vy vz angle)))
  out)

(int:define-op from-axis-angle ((axis v3:vec) (angle single-float)) (:out quat)
  (from-axis-angle! (quat 1) axis angle))

(int:define-op orient! ((out quat) (space keyword)
                        &rest (axes/angles (or keyword v3:vec single-float)))
    (:out quat)
  (declare (ignorable space axes/angles))
  (with-components ((o out))
    (with-elements ((q 1f0 0f0 0f0 0f0))
      (v3:with-elements ((v 0f0 0f0 0f0))
        (id! out)
        (loop :for (axis angle) :of-type ((or keyword v3:vec) single-float)
                :on axes/angles :by #'cddr
              :do (v3:with-components ((a axis))
                    (case axis
                      (:x (setf vx 1f0 vy 0f0 vz 0f0))
                      (:y (setf vx 0f0 vy 1f0 vz 0f0))
                      (:z (setf vx 0f0 vy 0f0 vz 1f0))
                      (t
                       (setf vx ax vy ay vz az)
                       (v3::%normalize vx vy vz vx vy vz))))
                  (%from-axis-angle qw qx qy qz vx vy vz angle)
                  (ecase space
                    (:local (%* ow ox oy oz qw qx qy qz ow ox oy oz))
                    (:world (%* ow ox oy oz ow ox oy oz qw qx qy qz)))
                  (normalize! out out)))))
  out)

(int:define-op orient ((space keyword)
                       &rest (axes/angles (or keyword v3:vec single-float)))
    (:out quat)
  (apply #'orient! (quat 1) space axes/angles))

(int:define-op from-velocity! ((out quat) (velocity v3:vec)
                               (delta single-float))
    (:out quat)
  "`VELOCITY` is a vec3 angular velocity whose magnitude represents
radians/second rotation about its vector. `DELTA` is an amount of time. Returns
a normalized quaternion that represents the angular velocity rotation in `DELTA`
amount of time. Destructively modifies `OUT`."
  (with-components ((o out))
    (v3:with-components ((av velocity))
      (v3:with-elements ((nav 0f0 0f0 0f0))
        (v3::%normalize navx navy navz avx avy avz)
        (%from-axis-angle ow ox oy oz navx navy navz
                          (cl:* (v3:length velocity) delta))
        (normalize! out out)))))

(int:define-op from-velocity ((velocity v3:vec) (delta single-float))
    (:out quat)
  "`VELOCITY` is a vec3 angular velocity whose magnitude represents
radians/second rotation about its vector. `DELTA` is an amount of time. Returns
a normalized quaternion that represents the angular velocity rotation in `DELTA`
amount of time. Allocates a fresh quaternion."
  (from-velocity! (quat 1) velocity delta))
