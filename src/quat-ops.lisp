(in-package #:net.mfiano.lisp.origin.quat)

;;; type

(deftype quat () '(simple-array u:f32 (4)))

;;; accessors

(u:fn-> w (quat) u:f32)
(u:defun-inline w (quat)
  (declare (optimize speed))
  (aref quat 0))

(u:fn-> (setf w) (u:f32 quat) u:f32)
(u:defun-inline (setf w) (value quat)
  (declare (optimize speed))
  (setf (aref quat 0) value))

(u:fn-> x (quat) u:f32)
(u:defun-inline x (quat)
  (declare (optimize speed))
  (aref quat 1))

(u:fn-> (setf x) (u:f32 quat) u:f32)
(u:defun-inline (setf x) (value quat)
  (declare (optimize speed))
  (setf (aref quat 1) value))

(u:fn-> y (quat) u:f32)
(u:defun-inline y (quat)
  (declare (optimize speed))
  (aref quat 2))

(u:fn-> (setf y) (u:f32 quat) u:f32)
(u:defun-inline (setf y) (value quat)
  (declare (optimize speed))
  (setf (aref quat 2) value))

(u:fn-> z (quat) u:f32)
(u:defun-inline z (quat)
  (declare (optimize speed))
  (aref quat 3))

(u:fn-> (setf z) (u:f32 quat) u:f32)
(u:defun-inline (setf z) (value quat)
  (declare (optimize speed))
  (setf (aref quat 3) value))

;;; constructors

(u:fn-> %quat (&rest u:f32) quat)
(u:eval-always
  (u:defun-inline %quat (&rest args)
    (declare (optimize speed))
    (make-array 4 :element-type 'single-float :initial-contents args)))

(ss:defstore quat (&rest args))

(ss:defspecialization (quat :inline t) () quat
  (%quat 1f0 0f0 0f0 0f0))

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

(u:define-constant +id+ (%quat 1f0 0f0 0f0 0f0) :test #'equalp)

;;; operators

(u:fn-> = (quat quat &key (:rel u:f32) (:abs u:f32)) boolean)
(u:defun-inline = (quat1 quat2 &key (rel 1e-7) (abs rel))
  (declare (optimize speed))
  (with-components ((q1 quat1) (q2 quat2))
    (and (int:= q1w q2w rel abs)
         (int:= q1x q2x rel abs)
         (int:= q1y q2y rel abs)
         (int:= q1z q2z rel abs))))

(u:fn-> id! (quat) quat)
(u:defun-inline id! (quat)
  (declare (optimize speed))
  (with-components ((q quat))
    (psetf qw 1f0 qx 0f0 qy 0f0 qz 0f0))
  quat)

(u:fn-> id () quat)
(u:defun-inline id ()
  (declare (optimize speed))
  (%quat 1f0 0f0 0f0 0f0))

(u:fn-> id-p (quat) boolean)
(u:defun-inline id-p (quat)
  (declare (optimize speed))
  (= quat +id+))

(u:fn-> random! (quat u:f32 u:f32) quat)
(u:defun-inline random! (out min max)
  (declare (optimize speed))
  (let ((diff (cl:- max min)))
    (with-components ((o out))
      (psetf ow (cl:+ min (cl:random diff))
             ox (cl:+ min (cl:random diff))
             oy (cl:+ min (cl:random diff))
             oz (cl:+ min (cl:random diff)))))
  out)

(u:fn-> random (u:f32 u:f32) quat)
(u:defun-inline random (min max)
  (declare (optimize speed))
  (random! (id) min max))

(u:fn-> copy! (quat quat) quat)
(u:defun-inline copy! (out quat)
  (declare (optimize speed))
  (with-components ((o out) (q quat))
    (psetf ow qw ox qx oy qy oz qz))
  out)

(u:fn-> copy (quat) quat)
(u:defun-inline copy (quat)
  (declare (optimize speed))
  (copy! (id) quat))

(u:fn-> +! (quat quat quat) quat)
(u:defun-inline +! (out quat1 quat2)
  (declare (optimize speed))
  (with-components ((o out) (q1 quat1) (q2 quat2))
    (psetf ow (cl:+ q1w q2w)
           ox (cl:+ q1x q2x)
           oy (cl:+ q1y q2y)
           oz (cl:+ q1z q2z)))
  out)

(u:fn-> + (quat quat) quat)
(u:defun-inline + (quat1 quat2)
  (declare (optimize speed))
  (+! (id) quat1 quat2))

(u:fn-> -! (quat quat quat) quat)
(u:defun-inline -! (out quat1 quat2)
  (declare (optimize speed))
  (with-components ((o out) (q1 quat1) (q2 quat2))
    (psetf ow (cl:- q1w q2w)
           ox (cl:- q1x q2x)
           oy (cl:- q1y q2y)
           oz (cl:- q1z q2z)))
  out)

(u:fn-> - (quat quat) quat)
(u:defun-inline - (quat1 quat2)
  (declare (optimize speed))
  (-! (id) quat1 quat2))

(defmacro %* (ow ox oy oz q1w q1x q1y q1z q2w q2x q2y q2z)
  `(psetf ,ow (cl:- (cl:* ,q1w ,q2w) (cl:* ,q1x ,q2x) (cl:* ,q1y ,q2y)
                    (cl:* ,q1z ,q2z))
          ,ox (cl:- (cl:+ (cl:* ,q1w ,q2x) (cl:* ,q1x ,q2w) (cl:* ,q1y ,q2z))
                    (cl:* ,q1z ,q2y))
          ,oy (cl:- (cl:+ (cl:* ,q1w ,q2y) (cl:* ,q1y ,q2w) (cl:* ,q1z ,q2x))
                    (cl:* ,q1x ,q2z))
          ,oz (cl:- (cl:+ (cl:* ,q1w ,q2z) (cl:* ,q1z ,q2w) (cl:* ,q1x ,q2y))
                    (cl:* ,q1y ,q2x))))

(u:fn-> *! (quat quat quat) quat)
(u:defun-inline *! (out quat1 quat2)
  (declare (optimize speed))
  (with-components ((o out) (q1 quat1) (q2 quat2))
    (%* ow ox oy oz q1w q1x q1y q1z q2w q2x q2y q2z))
  out)

(u:fn-> * (quat quat) quat)
(u:defun-inline * (quat1 quat2)
  (declare (optimize speed))
  (*! (id) quat1 quat2))

(defmacro %scale (ow ox oy oz w x y z scalar)
  `(psetf ,ow (cl:* ,w ,scalar)
          ,ox (cl:* ,x ,scalar)
          ,oy (cl:* ,y ,scalar)
          ,oz (cl:* ,z ,scalar)))

(u:fn-> scale! (quat quat u:f32) quat)
(u:defun-inline scale! (out quat scalar)
  (declare (optimize speed))
  (with-components ((o out) (v quat))
    (%scale ow ox oy oz vw vx vy vz scalar))
  out)

(u:fn-> scale (quat u:f32) quat)
(u:defun-inline scale (quat scalar)
  (declare (optimize speed))
  (scale! (id) quat scalar))

(u:fn-> conjugate! (quat quat) quat)
(u:defun-inline conjugate! (out quat)
  (declare (optimize speed))
  (with-components ((o out) (q quat))
    (psetf ow qw
           ox (cl:- qx)
           oy (cl:- qy)
           oz (cl:- qz)))
  out)

(u:fn-> conjugate (quat) quat)
(u:defun-inline conjugate (quat)
  (declare (optimize speed))
  (conjugate! (id) quat))

(u:fn-> cross! (quat quat quat) quat)
(u:defun-inline cross! (out quat1 quat2)
  (declare (optimize speed))
  (scale! out (+ (* quat2 (conjugate quat1)) (* quat1 quat2)) 0.5f0))

(u:fn-> cross (quat quat) quat)
(u:defun-inline cross (quat1 quat2)
  (declare (optimize speed))
  (cross! (id) quat1 quat2))

(u:fn-> length-squared (quat) u:f32)
(u:defun-inline length-squared (quat)
  (declare (optimize speed))
  (with-components ((q quat))
    (cl:+ (expt qw 2) (expt qx 2) (expt qy 2) (expt qz 2))))

(u:fn-> length (quat) u:f32)
(u:defun-inline length (quat)
  (declare (optimize speed))
  (sqrt (length-squared quat)))

(u:fn-> normalize! (quat quat) quat)
(u:defun-inline normalize! (out quat)
  (declare (optimize speed))
  (let ((length (length quat)))
    (unless (zerop length)
      (scale! out quat (/ length))))
  out)

(u:fn-> normalize (quat) quat)
(u:defun-inline normalize (quat)
  (declare (optimize speed))
  (normalize! (id) quat))

(u:fn-> negate! (quat quat) quat)
(u:defun-inline negate! (out quat)
  (declare (optimize speed))
  (scale! out quat -1f0))

(u:fn-> negate (quat) quat)
(u:defun-inline negate (quat)
  (declare (optimize speed))
  (negate! (id) quat))

(u:fn-> dot (quat quat) u:f32)
(u:defun-inline dot (quat1 quat2)
  (declare (optimize speed))
  (with-components ((q1 quat1) (q2 quat2))
    (cl:+ (cl:* q1w q2w) (cl:* q1x q2x) (cl:* q1y q2y) (cl:* q1z q2z))))

(u:fn-> inverse! (quat quat) quat)
(u:defun-inline inverse! (out quat)
  (declare (optimize speed))
  (conjugate! out quat)
  (scale! out out (/ (length-squared quat)))
  out)

(u:fn-> inverse (quat) quat)
(u:defun-inline inverse (quat)
  (declare (optimize speed))
  (inverse! (id) quat))

(u:fn-> rotate-euler! (quat quat v3:vec &key (:space keyword)) quat)
(defun rotate-euler! (out quat vec &key (space :local))
  (declare (optimize speed))
  (with-components ((o out) (q quat))
    (v3:with-components ((v vec))
      (v3:with-elements ((v (cl:* vx 0.5f0) (cl:* vy 0.5f0) (cl:* vz 0.5f0))
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
             (psetf q1w qw q1x qx q1y qy q1z qz
                    q2w rw q2x rx q2y ry q2z rz))
            (:world
             (psetf q1w rw q1x rx q1y ry q1z rz
                    q2w qw q2x qx q2y qy q2z qz)))
          (%* ow ox oy oz q1w q1x q1y q1z q2w q2x q2y q2z)))))
  out)

(u:fn-> rotate-euler (quat v3:vec &key (:space keyword)) quat)
(u:defun-inline rotate-euler (quat vec &key (space :local))
  (declare (optimize speed))
  (rotate-euler! (id) quat vec :space space))

(u:fn-> rotate! (quat quat quat &key (:space keyword)) quat)
(defun rotate! (out quat1 quat2 &key (space :local))
  (declare (optimize speed))
  (ecase space
    (:local
     (*! out quat1 quat2))
    (:world
     (*! out quat2 quat1)))
  (normalize! out out))

(u:fn-> rotate (quat quat &key (:space keyword)) quat)
(u:defun-inline rotate (quat1 quat2 &key (space :local))
  (declare (optimize speed))
  (rotate! (id) quat1 quat2 :space space))

(u:fn-> to-euler! (v3:vec quat) v3:vec)
(u:defun-inline to-euler! (out quat)
  (declare (optimize speed))
  (with-components ((q quat))
    (let* ((sinr-cosp (cl:* 2f0 (cl:+ (cl:* qw qx) (cl:* qy qz))))
           (cosr-cosp (cl:- 1f0 (cl:* 2f0 (cl:+ (cl:* qx qx) (cl:* qy qy)))))
           (roll (atan sinr-cosp cosr-cosp))
           (sinp (cl:* 2f0 (cl:- (cl:* qw qy) (cl:* qz qx))))
           (pitch (if (>= (abs sinp) 1f0)
                      (cl:* const:pi/2 (signum sinp))
                      (asin (the (single-float -1f0 1f0) sinp))))
           (siny-cosp (cl:* 2f0 (cl:+ (cl:* qw qz) (cl:* qx qy))))
           (cosy-cosp (cl:- 1f0 (cl:* 2f0 (cl:+ (cl:* qy qy) (cl:* qz qz)))))
           (yaw (atan siny-cosp cosy-cosp)))
      (v3:with-components ((o out))
        (psetf ox roll
               oy pitch
               oz yaw))))
  out)

(u:fn-> to-euler (quat) v3:vec)
(u:defun-inline to-euler (quat)
  (declare (optimize speed))
  (to-euler! (v3:zero) quat))

(u:fn-> to-mat3! (m3:mat quat) m3:mat)
(u:defun-inline to-mat3! (out quat)
  (declare (optimize speed))
  (let ((s (/ 2 (length-squared quat))))
    (m3:with-components ((o out))
      (with-components ((q quat))
        (v3:with-elements ((s (cl:* qx s) (cl:* qy s) (cl:* qz s))
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

(u:fn-> to-mat3 (quat) m3:mat)
(u:defun-inline to-mat3 (quat)
  (declare (optimize speed))
  (to-mat3! (m3:id) quat))

(u:fn-> to-mat4! (m4:mat quat) m4:mat)
(u:defun-inline to-mat4! (out quat)
  (declare (optimize speed))
  (let ((s (/ 2 (length-squared quat))))
    (m4:with-components ((o out))
      (with-components ((q quat))
        (v3:with-elements ((s (cl:* qx s) (cl:* qy s) (cl:* qz s))
                           (a (cl:* qx sx) (cl:* qx sy) (cl:* qx sz))
                           (b (cl:* qy sy) (cl:* qy sz) (cl:* qz sz))
                           (c (cl:* qw sx) (cl:* qw sy) (cl:* qw sz)))
          (psetf o00 (cl:- 1 (cl:+ bx bz))
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

(u:fn-> to-mat4 (quat) m4:mat)
(u:defun-inline to-mat4 (quat)
  (declare (optimize speed))
  (to-mat4! (m4:id) quat))

(u:fn-> from-mat3! (quat m3:mat) quat)
(defun from-mat3! (out mat)
  (declare (optimize speed))
  (with-components ((o out))
    (m3:with-components ((m mat))
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

(u:fn-> from-mat3 (m3:mat) quat)
(u:defun-inline from-mat3 (mat)
  (declare (optimize speed))
  (from-mat3! (id) mat))

(u:fn-> from-mat4! (quat m4:mat) quat)
(u:defun-inline from-mat4! (out mat)
  (declare (optimize speed))
  (with-components ((o out))
    (m4:with-components ((m mat))
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
                       oz (/ 0.25f0 s))))))))))
  out)

(u:fn-> from-mat4 (m4:mat) quat)
(u:defun-inline from-mat4 (mat)
  (declare (optimize speed))
  (from-mat4! (id) mat))

(u:fn-> slerp! (quat quat quat u:f32) quat)
(defun slerp! (out quat1 quat2 factor)
  (declare (optimize speed))
  (let ((factor factor))
    (with-components ((o out) (q1 quat1) (q2 quat2))
      (let ((dot (dot q1 q2))
            (q2 q2))
        (when (minusp dot)
          (negate! q2 q2)
          (setf dot (cl:- dot)))
        (if (> (abs dot) 0.9995f0)
            (psetf ow (u:lerp factor q1w q2w)
                   ox (u:lerp factor q1x q2x)
                   oy (u:lerp factor q1y q2y)
                   oz (u:lerp factor q1z q2z))
            (let* ((angle (acos (the (single-float -0.9995f0 0.9995f0) dot)))
                   (sin-angle (sin angle))
                   (scale1 (/ (sin (cl:* angle (cl:- 1 factor))) sin-angle))
                   (scale2 (/ (sin (cl:* factor angle)) sin-angle)))
              (psetf ow (cl:+ (cl:* q1w scale1) (cl:* q2w scale2))
                     ox (cl:+ (cl:* q1x scale1) (cl:* q2x scale2))
                     oy (cl:+ (cl:* q1y scale1) (cl:* q2y scale2))
                     oz (cl:+ (cl:* q1z scale1) (cl:* q2z scale2))))))))
  out)

(u:fn-> slerp (quat quat u:f32) quat)
(u:defun-inline slerp (quat1 quat2 factor)
  (declare (optimize speed))
  (slerp! (id) quat1 quat2 factor))

(defmacro %from-axis-angle (qw qx qy qz vx vy vz angle)
  (u:with-gensyms (half-angle c s)
    `(let* ((,half-angle (cl:* ,angle 0.5))
            (,c (cos ,half-angle))
            (,s (sin ,half-angle)))
       (psetf ,qw ,c
              ,qx (cl:* ,vx ,s)
              ,qy (cl:* ,vy ,s)
              ,qz (cl:* ,vz ,s)))))

(u:fn-> from-axis-angle! (quat v3:vec u:f32) quat)
(u:defun-inline from-axis-angle! (out axis angle)
  (declare (optimize speed))
  (with-components ((o out))
    (v3:with-components ((v axis))
      (%from-axis-angle ow ox oy oz vx vy vz angle)))
  out)

(u:fn-> from-axis-angle (v3:vec u:f32) quat)
(u:defun-inline from-axis-angle (axis angle)
  (declare (optimize speed))
  (from-axis-angle! (id) axis angle))

(u:fn-> orient! (quat keyword &rest (or keyword v3:vec u:f32)) quat)
(u:defun-inline orient! (out space &rest axes/angles)
  (declare (optimize speed))
  (with-components ((o out))
    (with-elements ((q 1f0 0f0 0f0 0f0))
      (v3:with-elements ((v 0f0 0f0 0f0))
        (id! out)
        (loop :for (axis angle) :of-type ((or keyword v3:vec) u:f32)
                :on axes/angles :by #'cddr
              :do (v3:with-components ((a axis))
                    (case axis
                      (:x (psetf vx 1f0 vy 0f0 vz 0f0))
                      (:y (psetf vx 0f0 vy 1f0 vz 0f0))
                      (:z (psetf vx 0f0 vy 0f0 vz 1f0))
                      (t
                       (psetf vx ax vy ay vz az)
                       (v3::%normalize vx vy vz vx vy vz))))
                  (%from-axis-angle qw qx qy qz vx vy vz angle)
                  (ecase space
                    (:local (%* ow ox oy oz qw qx qy qz ow ox oy oz))
                    (:world (%* ow ox oy oz ow ox oy oz qw qx qy qz)))
                  (normalize! out out)))))
  out)

(u:fn-> orient (keyword &rest (or keyword v3:vec u:f32)) quat)
(u:defun-inline orient (space &rest axes/angles)
  (declare (optimize speed))
  (apply #'orient! (id) space axes/angles))

(u:fn-> from-velocity! (quat v3:vec u:f32) quat)
(u:defun-inline from-velocity! (out velocity delta)
  (declare (optimize speed))
  (with-components ((o out))
    (v3:with-components ((av velocity))
      (v3:with-elements ((nav 0f0 0f0 0f0))
        (v3::%normalize navx navy navz avx avy avz)
        (%from-axis-angle ow ox oy oz navx navy navz
                          (cl:* (v3:length velocity) delta))
        (normalize! out out)))))

(u:fn-> from-velocity (v3:vec u:f32) quat)
(u:defun-inline from-velocity (velocity delta)
  (declare (optimize speed))
  (from-velocity! (id) velocity delta))
