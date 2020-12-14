(in-package #:net.mfiano.lisp.origin.dquat)

;;; type

(deftype quat () '(simple-array double-float (4)))

;;; accessors

(int:define-op w ((quat quat)) (:out double-float :speed nil)
  (aref quat 0))

(int:define-op (setf w) ((value double-float) (quat quat))
    (:out double-float :speed nil)
  (setf (aref quat 0) value))

(int:define-op x ((quat quat)) (:out double-float :speed nil)
  (aref quat 1))

(int:define-op (setf x) ((value double-float) (quat quat))
    (:out double-float :speed nil)
  (setf (aref quat 1) value))

(int:define-op y ((quat quat)) (:out double-float :speed nil)
  (aref quat 2))

(int:define-op (setf y) ((value double-float) (quat quat))
    (:out double-float :speed nil)
  (setf (aref quat 2) value))

(int:define-op z ((quat quat)) (:out double-float :speed nil)
  (aref quat 3))

(int:define-op (setf z) ((value double-float) (quat quat))
    (:out double-float :speed nil)
  (setf (aref quat 3) value))

;;; constructors

(int:define-op %quat (&rest (args double-float)) (:inline t :out quat)
  (make-array 4 :element-type 'double-float :initial-contents args))

(ss:defstore quat (&rest args))

(ss:defspecialization (quat :inline t) () quat
  (%quat 0d0 0d0 0d0 0d0))

(ss:defspecialization (quat :inline t) ((w real)) quat
  (%quat (float w 1d0) 0d0 0d0 0d0))

(ss:defspecialization (quat :inline t) ((w real) (x real) (y real) (z real))
    quat
  (%quat (float w 1d0) (float x 1d0) (float y 1d0) (float z 1d0)))

(ss:defspecialization (quat :inline t) ((xyz dv3:vec)) quat
  (dv3:with-components ((v xyz))
    (%quat 0d0 vx vy vz)))

(ss:defspecialization (quat :inline t) ((xyzw dv4:vec)) quat
  (dv4:with-components ((v xyzw))
    (%quat vx vy vz vw)))

(ss:defspecialization (quat :inline t) ((quat q:quat)) quat
  (q:with-components ((q quat))
    (%quat (float qw 1d0) (float qx 1d0) (float qy 1d0) (float qz 1d0))))

;;; constants

(u:define-constant +zero+ (%quat 0d0 0d0 0d0 0d0) :test #'equalp)

(u:define-constant +id+ (%quat 1d0 0d0 0d0 0d0) :test #'equalp)

;;; operators

(int:define-op id! ((in quat)) (:out quat)
  (with-components ((q in))
    (psetf qw 1d0 qx 0d0 qy 0d0 qz 0d0))
  in)

(int:define-op id-p ((in quat)) (:out boolean)
  (with-components ((q in))
    (and (cl:= qw 1d0)
         (cl:= qx qy qz 0d0))))

(int:define-op zero! ((in quat)) (:out quat)
  (with-components ((q in))
    (psetf qw 0d0 qx 0d0 qy 0d0 qz 0d0))
  in)

(int:define-op = ((in1 quat) (in2 quat)
                  &key (rel double-float 1d-7) (abs double-float rel))
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

(int:define-op random! ((out quat) (min double-float) (max double-float))
    (:out quat)
  (let ((diff (cl:- max min)))
    (with-components ((o out))
      (psetf ow (cl:+ min (cl:random diff))
             ox (cl:+ min (cl:random diff))
             oy (cl:+ min (cl:random diff))
             oz (cl:+ min (cl:random diff)))))
  out)

(int:define-op random ((min double-float) (max double-float)) (:out quat)
  (random! (quat) min max))

(int:define-op copy! ((out quat) (in quat)) (:out quat)
  (with-components ((o out) (q in))
    (psetf ow qw ox qx oy qy oz qz))
  out)

(int:define-op copy ((in quat)) (:out quat)
  (copy! (quat 1) in))

(int:define-op +! ((out quat) (in1 quat) (in2 quat)) (:out quat)
  (with-components ((o out) (q1 in1) (q2 in2))
    (psetf ow (cl:+ q1w q2w)
           ox (cl:+ q1x q2x)
           oy (cl:+ q1y q2y)
           oz (cl:+ q1z q2z)))
  out)

(int:define-op + ((in1 quat) (in2 quat)) (:out quat)
  (+! (quat 1) in1 in2))

(int:define-op -! ((out quat) (in1 quat) (in2 quat)) (:out quat)
  (with-components ((o out) (q1 in1) (q2 in2))
    (psetf ow (cl:- q1w q2w)
           ox (cl:- q1x q2x)
           oy (cl:- q1y q2y)
           oz (cl:- q1z q2z)))
  out)

(int:define-op - ((in1 quat) (in2 quat)) (:out quat)
  (-! (quat 1) in1 in2))

(defmacro %* (ow ox oy oz q1w q1x q1y q1z q2w q2x q2y q2z)
  `(psetf ,ow (cl:- (cl:* ,q1w ,q2w) (cl:* ,q1x ,q2x) (cl:* ,q1y ,q2y)
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
  `(psetf ,ow (cl:* ,w ,scalar)
          ,ox (cl:* ,x ,scalar)
          ,oy (cl:* ,y ,scalar)
          ,oz (cl:* ,z ,scalar)))

(int:define-op scale! ((out quat) (in quat) (scalar double-float)) (:out quat)
  (with-components ((o out) (v in))
    (%scale ow ox oy oz vw vx vy vz scalar))
  out)

(int:define-op scale ((in quat) (scalar float)) (:out quat)
  (scale! (quat) in scalar))

(int:define-op conjugate! ((out quat) (in quat)) (:out quat)
  (with-components ((o out) (q in))
    (psetf ow qw
           ox (cl:- qx)
           oy (cl:- qy)
           oz (cl:- qz)))
  out)

(int:define-op conjugate ((in quat)) (:out quat)
  (conjugate! (quat 1) in))

(int:define-op cross! ((out quat) (in1 quat) (in2 quat)) (:out quat)
  (scale! out (+ (* in2 (conjugate in1)) (* in1 in2)) 0.5d0))

(int:define-op cross ((in1 quat) (in2 quat)) (:out quat)
  (cross! (quat 1) in1 in2))

(int:define-op length-squared ((in quat)) (:out double-float :speed nil)
  (with-components ((q in))
    (cl:+ (expt qw 2) (expt qx 2) (expt qy 2) (expt qz 2))))

(int:define-op length ((in quat)) (:out double-float :speed nil)
  (sqrt (length-squared in)))

(int:define-op normalize! ((out quat) (in quat)) (:out quat)
  (let ((length (length in)))
    (unless (zerop length)
      (scale! out in (/ length))))
  out)

(int:define-op normalize ((in quat)) (:out quat)
  (normalize! (quat 1) in))

(int:define-op negate! ((out quat) (in quat)) (:out quat)
  (scale! out in -1d0))

(int:define-op negate ((in quat)) (:out quat)
  (negate! (quat 1) in))

(int:define-op dot ((in1 quat) (in2 quat)) (:out double-float :speed nil)
  (with-components ((q1 in1) (q2 in2))
    (cl:+ (cl:* q1w q2w) (cl:* q1x q2x) (cl:* q1y q2y) (cl:* q1z q2z))))

(int:define-op inverse! ((out quat) (in quat)) (:out quat)
  (conjugate! out in)
  (scale! out out (/ (length-squared in)))
  out)

(int:define-op inverse ((in quat)) (:out quat)
  (inverse! (quat 1) in))

(int:define-op rotate-euler! ((out quat) (in quat) (vec dv3:vec)
                              &key (space keyword :local))
    (:out quat :inline nil)
  (with-components ((o out) (q in))
    (dv3:with-components ((v vec))
      (dv3:with-elements ((v (cl:* vx 0.5) (cl:* vy 0.5) (cl:* vz 0.5))
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
                        (q1 0d0 0d0 0d0 0d0)
                        (q2 0d0 0d0 0d0 0d0))
          (ecase space
            (:local
             (psetf q1w qw q1x qx q1y qy q1z qz
                    q2w rw q2x rx q2y ry q2z rz))
            (:world
             (psetf q1w rw q1x rx q1y ry q1z rz
                    q2w qw q2x qx q2y qy q2z qz)))
          (%* ow ox oy oz q1w q1x q1y q1z q2w q2x q2y q2z)))))
  out)

(int:define-op rotate-euler ((in quat) (vec dv3:vec)
                             &key (space keyword :local))
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

(int:define-op to-euler! ((out dv3:vec) (in quat)) (:out dv3:vec :speed nil)
  (with-components ((q in))
    (let* ((sinr-cosp (cl:* 2d0 (cl:+ (cl:* qw qx) (cl:* qy qz))))
           (cosr-cosp (cl:- 1d0 (cl:* 2d0 (cl:+ (cl:* qx qx) (cl:* qy qy)))))
           (roll (atan sinr-cosp cosr-cosp))
           (sinp (cl:* 2d0 (cl:- (cl:* qw qy) (cl:* qz qx))))
           (pitch (if (>= (abs sinp) 1d0)
                      (cl:* (/ pi 2) (signum sinp))
                      (asin (the (double-float -1d0 1d0) sinp))))
           (siny-cosp (cl:* 2d0 (cl:+ (cl:* qw qz) (cl:* qx qy))))
           (cosy-cosp (cl:- 1d0 (cl:* 2d0 (cl:+ (cl:* qy qy) (cl:* qz qz)))))
           (yaw (atan siny-cosp cosy-cosp)))
      (dv3:with-components ((o out))
        (psetf ox roll
               oy pitch
               oz yaw))))
  out)

(int:define-op to-euler ((in quat)) (:out dv3:vec :speed nil)
  (to-euler! (dv3:vec) in))

(int:define-op to-vec3! ((out dv3:vec) (in quat)) (:out dv3:vec)
  (dv3:with-components ((v out))
    (with-components ((q in))
      (psetf vx qx vy qy vz qz)))
  out)

(int:define-op to-vec3 ((in quat)) (:out dv3:vec)
  (to-vec3! (dv3:vec) in))

(int:define-op to-vec4! ((out dv4:vec) (in quat)) (:out dv4:vec)
  (dv4:with-components ((v out))
    (with-components ((q in))
      (psetf vx qw vy qx vz qy vw qz)))
  out)

(int:define-op to-vec4 ((in quat)) (:out dv4:vec)
  (to-vec4! (dv4:vec) in))

(int:define-op from-vec3! ((out quat) (in dv3:vec)) (:out quat)
  (with-components ((q out))
    (dv3:with-components ((v in))
      (psetf qw 0d0 qx vx qy vy qz vz)))
  out)

(int:define-op from-vec3 ((in dv3:vec)) (:out quat)
  (from-vec3! (quat) in))

(int:define-op from-vec4! ((out quat) (in dv4:vec)) (:out quat)
  (with-components ((q out))
    (dv4:with-components ((v in))
      (psetf qw vx qx vy qy vz qz vw)))
  out)

(int:define-op from-vec4 ((in dv4:vec)) (:out quat)
  (from-vec4! (quat) in))

(int:define-op to-mat3! ((out dm3:mat) (in quat)) (:out dm3:mat)
  (let ((s (/ 2 (length-squared in))))
    (dm3:with-components ((o out))
      (with-components ((q in))
        (dv3:with-elements ((s (cl:* qx s) (cl:* qy s) (cl:* qz s))
                            (a (cl:* qx sx) (cl:* qx sy) (cl:* qx sz))
                            (b (cl:* qy sy) (cl:* qy sz) (cl:* qz sz))
                            (c (cl:* qw sx) (cl:* qw sy) (cl:* qw sz)))
          (psetf o00 (cl:- 1 (cl:+ bx bz))
                 o01 (cl:- ay cz)
                 o02 (cl:+ az cy)
                 o10 (cl:+ ay cz)
                 o11 (cl:- 1 (cl:+ ax bz))
                 o12 (cl:- bz cx)
                 o20 (cl:- az cy)
                 o21 (cl:+ by cx)
                 o22 (cl:- 1 (cl:+ ax bx)))))))
  out)

(int:define-op to-mat3 ((in quat)) (:out dm3:mat)
  (to-mat3! (dm3:mat 1) in))

(int:define-op to-mat4! ((out dm4:mat) (in quat)) (:out dm4:mat)
  (let ((s (/ 2 (length-squared in))))
    (dm4:with-components ((o out))
      (with-components ((q in))
        (dv3:with-elements ((s (cl:* qx s) (cl:* qy s) (cl:* qz s))
                            (a (cl:* qx sx) (cl:* qx sy) (cl:* qx sz))
                            (b (cl:* qy sy) (cl:* qy sz) (cl:* qz sz))
                            (c (cl:* qw sx) (cl:* qw sy) (cl:* qw sz)))
          (psetf o00 (cl:- 1 (cl:+ bx bz))
                 o01 (cl:- ay cz)
                 o02 (cl:+ az cy)
                 o03 0d0
                 o10 (cl:+ ay cz)
                 o11 (cl:- 1 (cl:+ ax bz))
                 o12 (cl:- by cx)
                 o13 0d0
                 o20 (cl:- az cy)
                 o21 (cl:+ by cx)
                 o22 (cl:- 1 (cl:+ ax bx))
                 o23 0d0
                 o30 0d0
                 o31 0d0
                 o32 0d0
                 o33 1d0)))))
  out)

(int:define-op to-mat4 ((in quat)) (:out dm4:mat)
  (to-mat4! (dm4:mat 1) in))

(int:define-op from-mat3! ((out quat) (in dm3:mat)) (:out quat :inline nil)
  (with-components ((o out))
    (dm3:with-components ((m in))
      (dv3:with-elements ((r (dv3::%length m00 m10 m20)
                             (dv3::%length m01 m11 m21)
                             (dv3::%length m02 m12 m22)))
        (dm3:with-elements ((nm (/ m00 rx) (/ m01 ry) (/ m02 rz)
                                (/ m10 rx) (/ m11 ry) (/ m12 rz)
                                (/ m20 rx) (/ m21 ry) (/ m22 rz)))
          (let ((trace (dm4::%trace nm00 nm11 nm22 1d0))
                (col1 (1+ (cl:- nm00 nm11 nm22)))
                (col2 (1+ (cl:- nm11 nm00 nm22)))
                (col3 (1+ (cl:- nm22 nm00 nm11))))
            (declare ((double-float 0d0) col1 col2 col3))
            (cond
              ((plusp trace)
               (let ((s (/ 0.5d0 (sqrt trace))))
                 (setf ow (/ 0.25d0 s)
                       ox (cl:* (cl:- nm21 nm12) s)
                       oy (cl:* (cl:- nm02 nm20) s)
                       oz (cl:* (cl:- nm10 nm01) s))))
              ((and (>= col1 col2) (>= col1 col3))
               (let ((s (/ 0.5d0 (sqrt col1))))
                 (setf ow (cl:* (cl:- nm21 nm12) s)
                       ox (/ 0.25d0 s)
                       oy (cl:* (cl:+ nm10 nm01) s)
                       oz (cl:* (cl:+ nm02 nm20) s))))
              ((and (>= col2 col1) (>= col2 col3))
               (let ((s (/ 0.5d0 (sqrt col2))))
                 (setf ow (cl:* (cl:- nm02 nm20) s)
                       ox (cl:* (cl:+ nm01 nm10) s)
                       oy (/ 0.25d0 s)
                       oz (cl:* (cl:+ nm12 nm21) s))))
              (t
               (let ((s (/ 0.5d0 (sqrt col3))))
                 (setf ow (cl:* (cl:- nm10 nm01) s)
                       ox (cl:* (cl:+ nm02 nm20) s)
                       oy (cl:* (cl:+ nm12 nm21) s)
                       oz (/ 0.25d0 s))))))))))
  out)

(int:define-op from-mat3 ((in dm3:mat)) (:out quat)
  (from-mat3! (quat 1) in))

(int:define-op from-mat4! ((out quat) (in dm4:mat)) (:out quat)
  (with-components ((o out))
    (dm4:with-components ((m in))
      (dv3:with-elements ((r (dv3::%length m00 m10 m20)
                             (dv3::%length m01 m11 m21)
                             (dv3::%length m02 m12 m22)))
        (dm3:with-elements ((nm (/ m00 rx) (/ m01 ry) (/ m02 rz)
                                (/ m10 rx) (/ m11 ry) (/ m12 rz)
                                (/ m20 rx) (/ m21 ry) (/ m22 rz)))
          (let* ((trace (dm4::%trace nm00 nm11 nm22 m33))
                 (col1 (1+ (cl:- nm00 nm11 nm22)))
                 (col2 (1+ (cl:- nm11 nm00 nm22)))
                 (col3 (1+ (cl:- nm22 nm00 nm11))))
            (declare ((double-float 0d0) col1 col2 col3))
            (cond
              ((plusp trace)
               (let ((s (/ 0.5d0 (sqrt trace))))
                 (setf ow (/ 0.25d0 s)
                       ox (cl:* (cl:- nm21 nm12) s)
                       oy (cl:* (cl:- nm02 nm20) s)
                       oz (cl:* (cl:- nm10 nm01) s))))
              ((and (>= col1 col2) (>= col1 col3))
               (let ((s (/ 0.5d0 (sqrt col1))))
                 (setf ow (cl:* (cl:- nm21 nm12) s)
                       ox (/ 0.25d0 s)
                       oy (cl:* (cl:+ nm10 nm01) s)
                       oz (cl:* (cl:+ nm02 nm20) s))))
              ((and (>= col2 col1) (>= col2 col3))
               (let ((s (/ 0.5d0 (sqrt col2))))
                 (setf ow (cl:* (cl:- nm02 nm20) s)
                       ox (cl:* (cl:+ nm01 nm10) s)
                       oy (/ 0.25d0 s)
                       oz (cl:* (cl:+ nm12 nm21) s))))
              (t
               (let ((s (/ 0.5d0 (sqrt col3))))
                 (setf ow (cl:* (cl:- nm10 nm01) s)
                       ox (cl:* (cl:+ nm02 nm20) s)
                       oy (cl:* (cl:+ nm12 nm21) s)
                       oz (/ 0.25d0 s)))))))))
    o))

(int:define-op from-mat4 ((in dm4:mat)) (:out quat)
  (from-mat4! (quat 1) in))

(int:define-op slerp! ((out quat) (in1 quat) (in2 quat) (factor double-float))
    (:out quat :inline nil)
  (let ((factor factor))
    (with-components ((o out) (q1 in1) (q2 in2))
      (let ((dot (dot q1 q2))
            (q2 q2))
        (when (minusp dot)
          (negate! q2 q2)
          (setf dot (cl:- dot)))
        (if (> (abs dot) 0.9995d0)
            (psetf ow (u:lerp factor q1w q2w)
                   ox (u:lerp factor q1x q2x)
                   oy (u:lerp factor q1y q2y)
                   oz (u:lerp factor q1z q2z))
            (let* ((angle (acos (the (double-float -0.9995d0 0.9995d0) dot)))
                   (sin-angle (sin angle))
                   (scale1 (/ (sin (cl:* angle (cl:- 1 factor))) sin-angle))
                   (scale2 (/ (sin (cl:* factor angle)) sin-angle)))
              (psetf ow (cl:+ (cl:* q1w scale1) (cl:* q2w scale2))
                     ox (cl:+ (cl:* q1x scale1) (cl:* q2x scale2))
                     oy (cl:+ (cl:* q1y scale1) (cl:* q2y scale2))
                     oz (cl:+ (cl:* q1z scale1) (cl:* q2z scale2))))))))
  out)

(int:define-op slerp ((in1 quat) (in2 quat) (factor float)) (:out quat)
  (slerp! (quat 1) in1 in2 factor))

(defmacro %from-axis-angle (qw qx qy qz vx vy vz angle)
  (u:with-gensyms (half-angle c s)
    `(let* ((,half-angle (cl:* ,angle 0.5))
            (,c (float (cos ,half-angle) 1d0))
            (,s (float (sin ,half-angle) 1d0)))
       (psetf ,qw ,c
              ,qx (cl:* ,vx ,s)
              ,qy (cl:* ,vy ,s)
              ,qz (cl:* ,vz ,s)))))

(int:define-op from-axis-angle! ((out quat) (axis dv3:vec) (angle double-float))
    (:out quat)
  (with-components ((o out))
    (dv3:with-components ((v axis))
      (%from-axis-angle ow ox oy oz vx vy vz angle)))
  out)

(int:define-op from-axis-angle ((axis dv3:vec) (angle double-float)) (:out quat)
  (from-axis-angle! (quat 1) axis angle))

(int:define-op orient! ((out quat) (space keyword)
                        &rest (axes/angles (or keyword dv3:vec double-float)))
    (:out quat)
  (declare (ignorable space axes/angles))
  (with-components ((o out))
    (with-elements ((q 1d0 0d0 0d0 0d0))
      (dv3:with-elements ((v 0d0 0d0 0d0))
        (id! out)
        (loop :for (axis angle) :of-type ((or keyword dv3:vec) double-float)
                :on axes/angles :by #'cddr
              :do (dv3:with-components ((a axis))
                    (case axis
                      (:x (psetf vx 1d0 vy 0d0 vz 0d0))
                      (:y (psetf vx 0d0 vy 1d0 vz 0d0))
                      (:z (psetf vx 0d0 vy 0d0 vz 1d0))
                      (t
                       (psetf vx ax vy ay vz az)
                       (dv3::%normalize vx vy vz vx vy vz))))
                  (%from-axis-angle qw qx qy qz vx vy vz angle)
                  (ecase space
                    (:local (%* ow ox oy oz qw qx qy qz ow ox oy oz))
                    (:world (%* ow ox oy oz ow ox oy oz qw qx qy qz)))
                  (normalize! out out)))))
  out)

(int:define-op orient ((space keyword)
                       &rest (axes/angles (or keyword dv3:vec double-float)))
    (:out quat)
  (apply #'orient! (quat 1) space axes/angles))

(int:define-op from-velocity! ((out quat) (velocity dv3:vec)
                               (delta double-float))
    (:out quat)
  "`VELOCITY` is a vec3 angular velocity whose magnitude represents
radians/second rotation about its vector. `DELTA` is an amount of time. Returns
a normalized quaternion that represents the angular velocity rotation in `DELTA`
amount of time. Destructively modifies `OUT`."
  (with-components ((o out))
    (dv3:with-components ((av velocity))
      (dv3:with-elements ((nav 0d0 0d0 0d0))
        (dv3::%normalize navx navy navz avx avy avz)
        (%from-axis-angle ow ox oy oz navx navy navz
                          (cl:* (dv3:length velocity) delta))
        (normalize! out out)))))

(int:define-op from-velocity ((velocity dv3:vec) (delta double-float))
    (:out quat)
  "`VELOCITY` is a vec3 angular velocity whose magnitude represents
radians/second rotation about its vector. `DELTA` is an amount of time. Returns
a normalized quaternion that represents the angular velocity rotation in `DELTA`
amount of time. Allocates a fresh quaternion."
  (from-velocity! (quat 1) velocity delta))
