(in-package #:cl-user)

(defpackage #:origin.quat
  (:local-nicknames (#:a #:alexandria)
                    (#:v3 #:origin.vec3)
                    (#:v4 #:origin.vec4)
                    (#:m3 #:origin.mat3)
                    (#:m4 #:origin.mat4))
  (:use #:cl #:origin.internal)
  (:shadow
   #:=
   #:+
   #:-
   #:*
   #:conjugate
   #:length
   #:random)
  (:export
   #:quat
   #:with-components
   #:with-elements
   #:w
   #:x
   #:y
   #:z
   #:+zero+
   #:+id+
   #:id!
   #:id
   #:zero!
   #:zero
   #:=
   #:~
   #:random!
   #:random
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
   #:rotate-euler!
   #:rotate-euler
   #:rotate!
   #:rotate
   #:to-euler!
   #:to-euler
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
   #:slerp
   #:from-axis-angle!
   #:from-axis-angle
   #:orient!
   #:orient))

(in-package #:origin.quat)

(deftype quat () '(simple-array single-float (4)))

(defmacro with-components (((prefix quat) &rest rest) &body body)
  (a:once-only (quat)
    `(symbol-macrolet ((,prefix ,quat)
                       (,(make-accessor-symbol prefix "W") (aref ,quat 0))
                       (,(make-accessor-symbol prefix "X") (aref ,quat 1))
                       (,(make-accessor-symbol prefix "Y") (aref ,quat 2))
                       (,(make-accessor-symbol prefix "Z") (aref ,quat 3)))
       ,(if rest
            `(with-components ,rest ,@body)
            `(progn ,@body)))))

(defmacro with-elements (((prefix w x y z) &rest rest) &body body)
  (let ((%w (make-accessor-symbol prefix "W"))
        (%x (make-accessor-symbol prefix "X"))
        (%y (make-accessor-symbol prefix "Y"))
        (%z (make-accessor-symbol prefix "Z")))
    `(let ((,%w ,w) (,%x ,x) (,%y ,y) (,%z ,z))
       (declare (ignorable ,%w ,%x ,%y ,%z))
       ,(if rest
            `(with-elements ,rest ,@body)
            `(progn ,@body)))))

(a:define-constant +zero+
    (make-array 4 :element-type 'single-float
                  :initial-contents '(0f0 0f0 0f0 0f0))
  :test #'equalp)

(a:define-constant +id+
    (make-array 4 :element-type 'single-float
                  :initial-contents '(1f0 0f0 0f0 0f0))
  :test #'equalp)

(define-op quat ((w single-float) (x single-float) (y single-float)
                 (z single-float))
    (:out quat)
  (let ((quat (make-array 4 :element-type 'single-float :initial-element 0f0)))
    (with-components ((q quat))
      (psetf qw w qx x qy y qz z))
    quat))

(define-op id! ((in quat)) (:out quat)
  (with-components ((q in))
    (psetf qw 1f0 qx 0f0 qy 0f0 qz 0f0))
  in)

(define-op id () (:out quat)
  (id! (quat 0f0 0f0 0f0 0f0)))

(define-op zero! ((in quat)) (:out quat)
  (with-components ((q in))
    (psetf qw 0f0 qx 0f0 qy 0f0 qz 0f0))
  in)

(define-op zero () (:out quat)
  (quat 0f0 0f0 0f0 0f0))

(define-op = ((in1 quat) (in2 quat)) (:out boolean)
  (with-components ((q1 in1) (q2 in2))
    (and (cl:= q1w q2w)
         (cl:= q1x q2x)
         (cl:= q1y q2y)
         (cl:= q1z q2z))))

(define-op ~ ((in1 quat) (in2 quat) &key (tolerance single-float 1e-7))
    (:out boolean)
  (with-components ((q1 in1) (q2 in2))
    (and (cl:< (cl:abs (cl:- q1w q2w)) tolerance)
         (cl:< (cl:abs (cl:- q1x q2x)) tolerance)
         (cl:< (cl:abs (cl:- q1y q2y)) tolerance)
         (cl:< (cl:abs (cl:- q1z q2z)) tolerance))))

(define-op random! ((out quat)
                    &key (min single-float 0f0) (max single-float 1f0))
    (:out quat)
  (with-components ((o out))
    (psetf ow (cl:+ min (cl:random (cl:- max min)))
           ox (cl:+ min (cl:random (cl:- max min)))
           oy (cl:+ min (cl:random (cl:- max min)))
           oz (cl:+ min (cl:random (cl:- max min)))))
  out)

(define-op random (&key (min single-float 0f0) (max single-float 1f0))
    (:out quat)
  (random! (zero) :min min :max max))

(define-op copy! ((out quat) (in quat)) (:out quat)
  (with-components ((o out) (q in))
    (psetf ow qw ox qx oy qy oz qz))
  out)

(define-op copy ((in quat)) (:out quat)
  (copy! (id) in))

(define-op +! ((out quat) (in1 quat) (in2 quat)) (:out quat)
  (with-components ((o out) (q1 in1) (q2 in2))
    (psetf ow (cl:+ q1w q2w)
           ox (cl:+ q1x q2x)
           oy (cl:+ q1y q2y)
           oz (cl:+ q1z q2z)))
  out)

(define-op + ((in1 quat) (in2 quat)) (:out quat)
  (+! (id) in1 in2))

(define-op -! ((out quat) (in1 quat) (in2 quat)) (:out quat)
  (with-components ((o out) (q1 in1) (q2 in2))
    (psetf ow (cl:- q1w q2w)
           ox (cl:- q1x q2x)
           oy (cl:- q1y q2y)
           oz (cl:- q1z q2z)))
  out)

(define-op - ((in1 quat) (in2 quat)) (:out quat)
  (-! (id) in1 in2))

(defmacro %* (ow ox oy oz q1w q1x q1y q1z q2w q2x q2y q2z)
  `(psetf ,ow (cl:- (cl:* ,q1w ,q2w) (cl:* ,q1x ,q2x) (cl:* ,q1y ,q2y)
                    (cl:* ,q1z ,q2z))
          ,ox (cl:- (cl:+ (cl:* ,q1w ,q2x) (cl:* ,q1x ,q2w) (cl:* ,q1y ,q2z))
                    (cl:* ,q1z ,q2y))
          ,oy (cl:- (cl:+ (cl:* ,q1w ,q2y) (cl:* ,q1y ,q2w) (cl:* ,q1z ,q2x))
                    (cl:* ,q1x ,q2z))
          ,oz (cl:- (cl:+ (cl:* ,q1w ,q2z) (cl:* ,q1z ,q2w) (cl:* ,q1x ,q2y))
                    (cl:* ,q1y ,q2x))))

(define-op *! ((out quat) (in1 quat) (in2 quat)) (:out quat)
  (with-components ((o out) (q1 in1) (q2 in2))
    (%* ow ox oy oz q1w q1x q1y q1z q2w q2x q2y q2z))
  out)

(define-op * ((in1 quat) (in2 quat)) (:out quat)
  (*! (id) in1 in2))

(defmacro %scale (ow ox oy oz w x y z scalar)
  `(psetf ,ow (cl:* ,w ,scalar)
          ,ox (cl:* ,x ,scalar)
          ,oy (cl:* ,y ,scalar)
          ,oz (cl:* ,z ,scalar)))

(define-op scale! ((out quat) (in quat) (scalar single-float)) (:out quat)
  (with-components ((o out) (v in))
    (%scale ow ox oy oz vw vx vy vz scalar))
  out)

(define-op scale ((in quat) (scalar float)) (:out quat)
  (scale! (zero) in scalar))

(define-op conjugate! ((out quat) (in quat)) (:out quat)
  (with-components ((o out) (q in))
    (psetf ow qw
           ox (cl:- qx)
           oy (cl:- qy)
           oz (cl:- qz)))
  out)

(define-op conjugate ((in quat)) (:out quat)
  (conjugate! (id) in))

(define-op cross! ((out quat) (in1 quat) (in2 quat)) (:out quat)
  (scale! out (+ (* in2 (conjugate in1)) (* in1 in2)) 0.5f0))

(define-op cross ((in1 quat) (in2 quat)) (:out quat)
  (cross! (id) in1 in2))

(define-op length-squared ((in quat)) (:out single-float)
  (with-components ((q in))
    (cl:+ (expt qw 2) (expt qx 2) (expt qy 2) (expt qz 2))))

(define-op length ((in quat)) (:out single-float)
  (sqrt (length-squared in)))

(define-op normalize! ((out quat) (in quat)) (:out quat)
  (let ((length (length in)))
    (unless (zerop length)
      (scale! out in (/ length))))
  out)

(define-op normalize ((in quat)) (:out quat)
  (normalize! (id) in))

(define-op negate! ((out quat) (in quat)) (:out quat)
  (scale! out in -1f0))

(define-op negate ((in quat)) (:out quat)
  (negate! (id) in))

(define-op dot ((in1 quat) (in2 quat)) (:out single-float)
  (with-components ((q1 in1) (q2 in2))
    (cl:+ (cl:* q1w q2w) (cl:* q1x q2x) (cl:* q1y q2y) (cl:* q1z q2z))))

(define-op inverse! ((out quat) (in quat)) (:out quat)
  (conjugate! out in)
  (scale! out out (/ (length-squared in)))
  out)

(define-op inverse ((in quat)) (:out quat)
  (inverse! (id) in))

(define-op rotate-euler! ((out quat) (in quat) (vec v3:vec)
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
              (psetf q1w qw q1x qx q1y qy q1z qz
                     q2w rw q2x rx q2y ry q2z rz))
            (:world
             (psetf q1w rw q1x rx q1y ry q1z rz
                    q2w qw q2x qx q2y qy q2z qz)))
          (%* ow ox oy oz q1w q1x q1y q1z q2w q2x q2y q2z)))))
  out)

(define-op rotate-euler ((in quat) (vec v3:vec) &key (space keyword :local))
    (:out quat)
  (rotate-euler! (id) in vec :space space))

(define-op rotate! ((out quat) (in1 quat) (in2 quat)
                    &key (space keyword :local))
    (:out quat :inline nil)
  (ecase space
    (:local
      (*! out in1 in2))
    (:world
     (*! out in2 in1)))
  (normalize! out out))

(define-op rotate ((in1 quat) (in2 quat) &key (space keyword :local))
    (:out quat)
  (rotate! (id) in1 in2 :space space))

(define-op to-euler! ((out v3:vec) (in quat)) (:out v3:vec :speed nil)
  (with-components ((q in))
    (let* ((sinr-cosp (cl:* 2f0 (cl:+ (cl:* qw qx) (cl:* qy qz))))
           (cosr-cosp (cl:- 1f0 (cl:* 2f0 (cl:+ (cl:* qx qx) (cl:* qy qy)))))
           (roll (atan sinr-cosp cosr-cosp))
           (sinp (cl:* 2f0 (cl:- (cl:* qw qy) (cl:* qz qx))))
           (pitch (if (>= (abs sinp) 1f0)
                      (float (cl:* (abs (cl:* pi 0.5f0)) (signum sinp)) 1f0)
                      (asin sinp)))
           (siny-cosp (cl:* 2f0 (cl:+ (cl:* qw qz) (cl:* qx qy))))
           (cosy-cosp (cl:- 1f0 (cl:* 2f0 (cl:+ (cl:* qy qy) (cl:* qz qz)))))
           (yaw (atan siny-cosp cosy-cosp)))
      (v3:with-components ((o out))
        (psetf ox roll
               oy pitch
               oz yaw))))
  out)

(define-op to-euler ((in quat)) (:out v3:vec :speed nil)
  (to-euler! (v3:zero) in))

(define-op to-vec3! ((out v3:vec) (in quat)) (:out v3:vec)
  (v3:with-components ((v out))
    (with-components ((q in))
      (psetf vx qx vy qy vz qz)))
  out)

(define-op to-vec3 ((in quat)) (:out v3:vec)
  (to-vec3! (v3:zero) in))

(define-op to-vec4! ((out v4:vec) (in quat)) (:out v4:vec)
  (v4:with-components ((v out))
    (with-components ((q in))
      (psetf vx qw vy qx vz qy vw qz)))
  out)

(define-op to-vec4 ((in quat)) (:out v4:vec)
  (to-vec4! (v4:zero) in))

(define-op from-vec3! ((out quat) (in v3:vec)) (:out quat)
  (with-components ((q out))
    (v3:with-components ((v in))
      (psetf qw 0f0 qx vx qy vy qz vz)))
  out)

(define-op from-vec3 ((in v3:vec)) (:out quat)
  (from-vec3! (zero) in))

(define-op from-vec4! ((out quat) (in v4:vec)) (:out quat)
  (with-components ((q out))
    (v4:with-components ((v in))
      (psetf qw vx qx vy qy vz qz vw)))
  out)

(define-op from-vec4 ((in v4:vec)) (:out quat)
  (from-vec4! (zero) in))

(define-op to-mat3! ((out m3:mat) (in quat)) (:out m3:mat)
  (let ((s (/ 2 (length-squared in))))
    (m3:with-components ((o out))
      (with-components ((q in))
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

(define-op to-mat3 ((in quat)) (:out m3:mat)
  (to-mat3! (m3:id) in))

(define-op to-mat4! ((out m4:mat) (in quat)) (:out m4:mat)
  (let ((s (/ 2 (length-squared in))))
    (m4:with-components ((o out))
      (with-components ((q in))
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

(define-op to-mat4 ((in quat)) (:out m4:mat)
  (to-mat4! (m4:id) in))

(define-op from-mat3! ((out quat) (in m3:mat))
    (:out quat :inline nil :speed nil)
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
                     oz (/ 0.25f0 s)))))))))
  out)

(define-op from-mat3 ((in m3:mat)) (:out quat)
  (from-mat3! (id) in))

(define-op from-mat4! ((out quat) (in m4:mat)) (:out quat :speed nil)
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
                     oz (/ 0.25f0 s))))))))
    o))

(define-op from-mat4 ((in m4:mat)) (:out quat :speed nil)
  (from-mat4! (id) in))

(define-op slerp! ((out quat) (in1 quat) (in2 quat) (factor single-float))
    (:out quat :inline nil)
  (let ((factor factor))
    (with-components ((o out) (q1 in1) (q2 in2))
      (let ((dot (dot q1 q2))
            (q2 q2))
        (when (minusp dot)
          (negate! q2 q2)
          (setf dot (cl:- dot)))
        (if (> (abs dot) 0.9995f0)
            (psetf ow (a:lerp factor q1w q2w)
                   ox (a:lerp factor q1x q2x)
                   oy (a:lerp factor q1y q2y)
                   oz (a:lerp factor q1z q2z))
            (let* ((angle (acos (the (single-float -0.9995 0.9995) dot)))
                   (sin-angle (sin angle))
                   (scale1 (/ (sin (cl:* angle (cl:- 1 factor))) sin-angle))
                   (scale2 (/ (sin (cl:* factor angle)) sin-angle)))
              (psetf ow (cl:+ (cl:* q1w scale1) (cl:* q2w scale2))
                     ox (cl:+ (cl:* q1x scale1) (cl:* q2x scale2))
                     oy (cl:+ (cl:* q1y scale1) (cl:* q2y scale2))
                     oz (cl:+ (cl:* q1z scale1) (cl:* q2z scale2))))))))
  out)

(define-op slerp ((in1 quat) (in2 quat) (factor float)) (:out quat)
  (slerp! (id) in1 in2 factor))

(defmacro %from-axis-angle (qw qx qy qz vx vy vz angle)
  (a:with-gensyms (half-angle c s)
    `(let* ((,half-angle (cl:* ,angle 0.5))
            (,c (float (cos ,half-angle) 1f0))
            (,s (float (sin ,half-angle) 1f0)))
       (psetf ,qw ,c
              ,qx (cl:* ,vx ,s)
              ,qy (cl:* ,vy ,s)
              ,qz (cl:* ,vz ,s)))))

(define-op from-axis-angle! ((out quat) (axis v3:vec) (angle single-float))
    (:out quat)
  (with-components ((o out))
    (v3:with-components ((v axis))
      (%from-axis-angle ow ox oy oz vx vy vz angle)))
  out)

(define-op from-axis-angle ((axis v3:vec) (angle single-float)) (:out quat)
  (from-axis-angle! (id) axis angle))

(define-op orient! ((out quat) (space keyword)
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

(define-op orient ((space keyword)
                   &rest (axes/angles (or keyword v3:vec single-float)))
    (:out quat)
  (apply #'orient! (id) space axes/angles))
