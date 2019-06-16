(in-package #:cl-user)

(defpackage #:origin.quat
  (:local-nicknames (#:v3 #:origin.vec3)
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

(in-package #:origin.quat)

(deftype quat () '(simple-array single-float (4)))

(defstruct (quat (:type (vector single-float))
                 (:constructor %make (w x y z))
                 (:conc-name nil)
                 (:predicate nil)
                 (:copier nil))
  (w 0f0 :type single-float)
  (x 0f0 :type single-float)
  (y 0f0 :type single-float)
  (z 0f0 :type single-float))

(defmacro with-components (((prefix quat) &rest rest) &body body)
  `(with-accessors ((,prefix identity)
                    (,(make-accessor-symbol prefix 'w) w)
                    (,(make-accessor-symbol prefix 'x) x)
                    (,(make-accessor-symbol prefix 'y) y)
                    (,(make-accessor-symbol prefix 'z) z))
       ,quat
     ,(if rest
          `(with-components ,rest ,@body)
          `(progn ,@body))))

(au:define-constant +zero+
    (make-array 4 :element-type 'single-float
                  :initial-contents '(0f0 0f0 0f0 0f0))
  :test #'equalp)

(au:define-constant +id+
    (make-array 4 :element-type 'single-float
                  :initial-contents '(1f0 0f0 0f0 0f0))
  :test #'equalp)

(define-op make ((w real) (x real) (y real) (z real)) (:out quat)
  (%make (float w 1f0) (float x 1f0) (float y 1f0) (float z 1f0)))

(define-op id! ((in quat)) (:out quat)
  (with-components ((q in))
    (psetf qw 1f0 qx 0f0 qy 0f0 qz 0f0))
  in)

(define-op id () (:out quat)
  (id! (make 0f0 0f0 0f0 0f0)))

(define-op zero! ((in quat)) (:out quat)
  (with-components ((q in))
    (psetf qw 0f0 qx 0f0 qy 0f0 qz 0f0))
  in)

(define-op zero () (:out quat)
  (make 0f0 0f0 0f0 0f0))

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

(define-op random! ((out quat) &key (min real 0.0) (max real 1.0)) (:out quat)
  (with-components ((o out))
    (psetf ow (cl:+ min (cl:random (cl:- max min)))
           ox (cl:+ min (cl:random (cl:- max min)))
           oy (cl:+ min (cl:random (cl:- max min)))
           oz (cl:+ min (cl:random (cl:- max min)))))
  out)

(define-op random (&key (min real 0.0) (max real 1.0)) (:out quat)
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

(define-op *! ((out quat) (in1 quat) (in2 quat)) (:out quat)
  (with-components ((o out) (q1 in1) (q2 in2))
    (psetf ow (cl:- (cl:* q1w q2w) (cl:* q1x q2x) (cl:* q1y q2y)
                    (cl:* q1z q2z))
           ox (cl:- (cl:+ (cl:* q1w q2x) (cl:* q1x q2w) (cl:* q1y q2z))
                    (cl:* q1z q2y))
           oy (cl:- (cl:+ (cl:* q1w q2y) (cl:* q1y q2w) (cl:* q1z q2x))
                    (cl:* q1x q2z))
           oz (cl:- (cl:+ (cl:* q1w q2z) (cl:* q1z q2w) (cl:* q1x q2y))
                    (cl:* q1y q2x))))
  out)

(define-op * ((in1 quat) (in2 quat)) (:out quat)
  (*! (id) in1 in2))

(define-op scale! ((out quat) (in quat) (scalar single-float)) (:out quat)
  (with-components ((o out) (q in))
    (psetf ow (cl:* qw scalar)
           ox (cl:* qx scalar)
           oy (cl:* qy scalar)
           oz (cl:* qz scalar)))
  out)

(define-op scale ((quat quat) (scalar single-float)) (:out quat)
  (scale! (id) quat scalar))

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
    (cl:+ (cl:* qw qw) (cl:* qx qx) (cl:* qy qy) (cl:* qz qz))))

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

(define-op rotate! ((out quat) (in quat) (vec v3:vec)
                    &key (space keyword :local))
    (:out quat :inline nil)
  (with-components ((o out) (q in))
    (v3:with-components ((v vec))
      (let* ((vx (cl:* vx 0.5f0))
             (vy (cl:* vy 0.5f0))
             (vz (cl:* vz 0.5f0))
             (cx (cos vx))
             (cy (cos vy))
             (cz (cos vz))
             (sx (sin vx))
             (sy (sin vy))
             (sz (sin vz))
             (rw (cl:- (cl:* cx cy cz)
                       (cl:* sx sy sz)))
             (rx (cl:+ (cl:* sx cy cz)
                       (cl:* cx sy sz)))
             (ry (cl:- (cl:* cx sy cz)
                       (cl:* sx cy sz)))
             (rz (cl:+ (cl:* sx sy cz)
                       (cl:* cx cy sz)))
             (lqw 0f0)
             (lqx 0f0)
             (lqy 0f0)
             (lqz 0f0)
             (rqw 0f0)
             (rqx 0f0)
             (rqy 0f0)
             (rqz 0f0))
        (ecase space
          (:local
            (psetf lqw qw lqx qx lqy qy lqz qz
                   rqw rw rqx rx rqy ry rqz rz))
          (:world
           (psetf lqw rw lqx rx lqy ry lqz rz
                  rqw qw rqx qx rqy qy rqz qz)))
        (psetf
         ow (cl:- (cl:* lqw rqw)
                  (cl:* lqx rqx)
                  (cl:* lqy rqy)
                  (cl:* lqz rqz))
         ox (cl:- (cl:* lqw rqx)
                  (cl:* lqx rqw)
                  (cl:* lqy rqz)
                  (cl:* lqz rqy))
         oy (cl:- (cl:* lqw rqy)
                  (cl:* lqy rqw)
                  (cl:* lqz rqx)
                  (cl:* lqx rqz))
         oz (cl:- (cl:* lqw rqz)
                  (cl:* lqz rqw)
                  (cl:* lqx rqy)
                  (cl:* lqy rqx))))))
  out)

(define-op rotate ((in quat) (vec v3:vec) &key (space keyword :local))
    (:out quat)
  (rotate! (id) in vec :space space))

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
  (m3:with-components ((o out))
    (with-components ((q in))
      (let* ((s (cl:/ 2 (length-squared in)))
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

(define-op to-mat3 ((in quat)) (:out m3:mat)
  (to-mat3! (m3:id) in))

(define-op to-mat4! ((out m4:mat) (in quat)) (:out m4:mat)
  (m4:with-components ((o out))
    (with-components ((q in))
      (let* ((s (/ 2 (length-squared in)))
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

(define-op to-mat4 ((in quat)) (:out m4:mat)
  (to-mat4! (m4:id) in))

(define-op from-mat3! ((out quat) (in m3:mat)) (:out quat :inline nil)
  (with-components ((o out))
    (m3:with-components ((m in))
      (let* ((x-rot-denom (sqrt
                           (cl:+ (cl:* m00 m00) (cl:* m10 m10) (cl:* m20 m20))))
             (y-rot-denom (sqrt
                           (cl:+ (cl:* m01 m01) (cl:* m11 m11) (cl:* m21 m21))))
             (z-rot-denom (sqrt
                           (cl:+ (cl:* m02 m02) (cl:* m12 m12) (cl:* m22 m22))))
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
                   oz (/ 0.25f0 s))))))))
  out)

(define-op from-mat3 ((in m3:mat)) (:out quat)
  (from-mat3! (id) in))

(define-op from-mat4! ((out quat) (in m4:mat)) (:out quat)
  (with-components ((o out))
    (m4:with-components ((m in))
      (let* ((x-rot-denom (sqrt
                           (cl:+ (cl:* m00 m00) (cl:* m10 m10) (cl:* m20 m20))))
             (y-rot-denom (sqrt
                           (cl:+ (cl:* m01 m01) (cl:* m11 m11) (cl:* m21 m21))))
             (z-rot-denom (sqrt
                           (cl:+ (cl:* m02 m02) (cl:* m12 m12) (cl:* m22 m22))))
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

(define-op from-mat4 ((in m4:mat)) (:out quat)
  (from-mat4! (id) in))

(define-op slerp! ((out quat) (in1 quat) (in2 quat) (factor single-float))
    (:out quat :inline nil)
  (with-components ((o out) (q1 in1) (q2 in2))
    (let ((dot (dot q1 q2))
          (q2 q2))
      (when (minusp dot)
        (negate! q2 q2)
        (psetf dot (cl:- dot)))
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

(define-op slerp ((in1 quat) (in2 quat) (factor single-float)) (:out quat)
  (slerp! (id) in1 in2 factor))
