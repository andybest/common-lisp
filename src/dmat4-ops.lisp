(in-package #:net.mfiano.lisp.origin.dmat4)

;;; constructors

(int:define-op %mat (&rest (args double-float)) (:inline t :out mat)
  (make-array 16 :element-type 'double-float :initial-contents args))

(ss:defstore mat (&rest args))

(ss:defspecialization (mat :inline t) () mat
  (%mat 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0))

(ss:defspecialization (mat :inline t) ((x real)) mat
  (%mat (float x 1d0) 0d0 0d0 0d0
        0d0 (float x 1d0) 0d0 0d0
        0d0 0d0 (float x 1d0) 0d0
        0d0 0d0 0d0 (float x 1d0)))

(ss:defspecialization (mat :inline t) ((mat dm2:mat)) mat
  (dm2:with-components ((m mat))
    (%mat m00 m01 0d0 0d0 m10 m11 0d0 0d0 0d0 0d0 1d0 0d0 0d0 0d0 0d0 1d0)))

(ss:defspecialization (mat :inline t) ((mat dm3:mat)) mat
  (dm3:with-components ((m mat))
    (%mat m00 m01 m02 0d0 m10 m11 m12 0d0 m20 m21 m22 0d0 0d0 0d0 0d0 1d0)))

(ss:defspecialization (mat :inline t) ((mat mat)) mat
  (with-components ((m mat))
    (%mat m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33)))

(ss:defspecialization (mat :inline t) ((a dv4:vec) (b dv4:vec) (c dv4:vec)
                                       (d dv4:vec))
    mat
  (dv4:with-components ((a a) (b b) (c c) (d d))
    (%mat ax ay az aw bx by bz bw cx cy cz cw dx dy dz dw)))

(ss:defspecialization (mat :inline t) ((a real) (b real) (c real) (d real)
                                       (e real) (f real) (g real) (h real)
                                       (i real) (j real) (k real) (l real)
                                       (m real) (n real) (o real) (p real))
    mat
  (%mat (float a 1d0) (float b 1d0) (float c 1d0) (float d 1d0)
        (float e 1d0) (float f 1d0) (float g 1d0) (float h 1d0)
        (float i 1d0) (float j 1d0) (float k 1d0) (float l 1d0)
        (float m 1d0) (float n 1d0) (float o 1d0) (float p 1d0)))

(ss:defspecialization (mat :inline t) ((mat m4:mat))
    mat
  (m4:with-components ((m mat))
    (%mat (float m00 1d0) (float m01 1d0) (float m02 1d0) (float m03 1d0)
          (float m10 1d0) (float m11 1d0) (float m12 1d0) (float m13 1d0)
          (float m20 1d0) (float m21 1d0) (float m22 1d0) (float m23 1d0)
          (float m30 1d0) (float m31 1d0) (float m32 1d0) (float m33 1d0))))

;;; constants

(u:define-constant +zero+
    (%mat 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0)
  :test #'equalp)

(u:define-constant +id+
    (%mat 1d0 0d0 0d0 0d0 0d0 1d0 0d0 0d0 0d0 0d0 1d0 0d0 0d0 0d0 0d0 1d0)
  :test #'equalp)

;;; operators

(int:define-op zero! ((in mat)) (:out mat)
  (with-components ((m in))
    (psetf m00 0d0 m01 0d0 m02 0d0 m03 0d0
           m10 0d0 m11 0d0 m12 0d0 m13 0d0
           m20 0d0 m21 0d0 m22 0d0 m23 0d0
           m30 0d0 m31 0d0 m32 0d0 m33 0d0))
  in)

(int:define-op zero-p ((in mat)) (:out boolean)
  (with-components ((m in))
    (cl:= 0d0 m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33)))

(int:define-op id! ((in mat)) (:out mat)
  (with-components ((m in))
    (psetf m00 1d0 m01 0d0 m02 0d0 m03 0d0
           m10 0d0 m11 1d0 m12 0d0 m13 0d0
           m20 0d0 m21 0d0 m22 1d0 m23 0d0
           m30 0d0 m31 0d0 m32 0d0 m33 1d0))
  in)

(int:define-op id-p ((in mat)) (:out boolean)
  (with-components ((m in))
    (and (cl:= 0d0 m01 m02 m03 m10 m12 m13 m20 m21 m23 m30 m31 m32)
         (cl:= 1d0 m00 m11 m22 m33))))

(int:define-op = ((in1 mat) (in2 mat)) (:out boolean)
  (with-components ((a in1) (b in2))
    (and (cl:= a00 b00) (cl:= a01 b01) (cl:= a02 b02) (cl:= a03 b03)
         (cl:= a10 b10) (cl:= a11 b11) (cl:= a12 b12) (cl:= a13 b13)
         (cl:= a20 b20) (cl:= a21 b21) (cl:= a22 b22) (cl:= a23 b23)
         (cl:= a30 b30) (cl:= a31 b31) (cl:= a32 b32) (cl:= a33 b33))))

(int:define-op ~ ((in1 mat) (in2 mat) &key (tolerance double-float 1d-7))
    (:out boolean)
  (with-components ((a in1) (b in2))
    (and (cl:< (cl:abs (cl:- a00 b00)) tolerance)
         (cl:< (cl:abs (cl:- a01 b01)) tolerance)
         (cl:< (cl:abs (cl:- a02 b02)) tolerance)
         (cl:< (cl:abs (cl:- a03 b03)) tolerance)
         (cl:< (cl:abs (cl:- a10 b10)) tolerance)
         (cl:< (cl:abs (cl:- a11 b11)) tolerance)
         (cl:< (cl:abs (cl:- a12 b12)) tolerance)
         (cl:< (cl:abs (cl:- a13 b13)) tolerance)
         (cl:< (cl:abs (cl:- a20 b20)) tolerance)
         (cl:< (cl:abs (cl:- a21 b21)) tolerance)
         (cl:< (cl:abs (cl:- a22 b22)) tolerance)
         (cl:< (cl:abs (cl:- a23 b23)) tolerance)
         (cl:< (cl:abs (cl:- a30 b30)) tolerance)
         (cl:< (cl:abs (cl:- a31 b31)) tolerance)
         (cl:< (cl:abs (cl:- a32 b32)) tolerance)
         (cl:< (cl:abs (cl:- a33 b33)) tolerance))))

(int:define-op random! ((out mat)
                        &key (min double-float 0d0) (max double-float 1d0))
    (:out mat)
  (with-components ((o out))
    (psetf o00 (cl:+ min (cl:random (cl:- max min)))
           o01 (cl:+ min (cl:random (cl:- max min)))
           o02 (cl:+ min (cl:random (cl:- max min)))
           o03 (cl:+ min (cl:random (cl:- max min)))
           o10 (cl:+ min (cl:random (cl:- max min)))
           o11 (cl:+ min (cl:random (cl:- max min)))
           o12 (cl:+ min (cl:random (cl:- max min)))
           o13 (cl:+ min (cl:random (cl:- max min)))
           o20 (cl:+ min (cl:random (cl:- max min)))
           o21 (cl:+ min (cl:random (cl:- max min)))
           o22 (cl:+ min (cl:random (cl:- max min)))
           o23 (cl:+ min (cl:random (cl:- max min)))
           o30 (cl:+ min (cl:random (cl:- max min)))
           o31 (cl:+ min (cl:random (cl:- max min)))
           o32 (cl:+ min (cl:random (cl:- max min)))
           o33 (cl:+ min (cl:random (cl:- max min)))))
  out)

(int:define-op random (&key (min double-float 0d0) (max double-float 1d0))
    (:out mat)
  (random! (mat) :min min :max max))

(int:define-op copy! ((out mat) (in mat)) (:out mat)
  (with-components ((o out) (m in))
    (psetf o00 m00 o01 m01 o02 m02 o03 m03
           o10 m10 o11 m11 o12 m12 o13 m13
           o20 m20 o21 m21 o22 m22 o23 m23
           o30 m30 o31 m31 o32 m32 o33 m33))
  out)

(int:define-op copy ((in mat)) (:out mat)
  (copy! (mat) in))

(int:define-op clamp! ((out mat) (in mat)
                       &key
                       (min double-float most-negative-double-float)
                       (max double-float most-positive-double-float))
    (:out mat)
  (with-components ((o out) (m in))
    (psetf o00 (u:clamp m00 min max)
           o01 (u:clamp m01 min max)
           o02 (u:clamp m02 min max)
           o03 (u:clamp m03 min max)
           o10 (u:clamp m10 min max)
           o11 (u:clamp m11 min max)
           o12 (u:clamp m12 min max)
           o13 (u:clamp m13 min max)
           o20 (u:clamp m20 min max)
           o21 (u:clamp m21 min max)
           o22 (u:clamp m22 min max)
           o23 (u:clamp m23 min max)
           o30 (u:clamp m30 min max)
           o31 (u:clamp m31 min max)
           o32 (u:clamp m32 min max)
           o33 (u:clamp m33 min max)))
  out)

(int:define-op clamp ((in mat)
                      &key
                      (min double-float most-negative-double-float)
                      (max double-float most-positive-double-float))
    (:out mat)
  (clamp! (mat) in :min min :max max))

(int:define-op +! ((out mat) (in1 mat) (in2 mat)) (:out mat)
  (with-components ((o out) (a in1) (b in2))
    (psetf o00 (cl:+ a00 b00)
           o10 (cl:+ a10 b10)
           o20 (cl:+ a20 b20)
           o30 (cl:+ a30 b30)
           o01 (cl:+ a01 b01)
           o11 (cl:+ a11 b11)
           o21 (cl:+ a21 b21)
           o31 (cl:+ a31 b31)
           o02 (cl:+ a02 b02)
           o12 (cl:+ a12 b12)
           o22 (cl:+ a22 b22)
           o32 (cl:+ a32 b32)
           o03 (cl:+ a03 b03)
           o13 (cl:+ a13 b13)
           o23 (cl:+ a23 b23)
           o33 (cl:+ a33 b33)))
  out)

(int:define-op + ((in1 mat) (in2 mat)) (:out mat)
  (+! (mat) in1 in2))

(int:define-op -! ((out mat) (in1 mat) (in2 mat)) (:out mat)
  (with-components ((o out) (a in1) (b in2))
    (psetf o00 (cl:- a00 b00)
           o10 (cl:- a10 b10)
           o20 (cl:- a20 b20)
           o30 (cl:- a30 b30)
           o01 (cl:- a01 b01)
           o11 (cl:- a11 b11)
           o21 (cl:- a21 b21)
           o31 (cl:- a31 b31)
           o02 (cl:- a02 b02)
           o12 (cl:- a12 b12)
           o22 (cl:- a22 b22)
           o32 (cl:- a32 b32)
           o03 (cl:- a03 b03)
           o13 (cl:- a13 b13)
           o23 (cl:- a23 b23)
           o33 (cl:- a33 b33)))
  out)

(int:define-op - ((in1 mat) (in2 mat)) (:out mat)
  (-! (mat) in1 in2))

(defmacro %* (o00 o01 o02 o03 o10 o11 o12 o13 o20 o21 o22 o23 o30 o31 o32 o33
              a00 a01 a02 a03 a10 a11 a12 a13 a20 a21 a22 a23 a30 a31 a32 a33
              b00 b01 b02 b03 b10 b11 b12 b13 b20 b21 b22 b23 b30 b31 b32 b33)
  `(psetf ,o00 (cl:+ (cl:* ,a00 ,b00) (cl:* ,a01 ,b10) (cl:* ,a02 ,b20)
                     (cl:* ,a03 ,b30))
          ,o10 (cl:+ (cl:* ,a10 ,b00) (cl:* ,a11 ,b10) (cl:* ,a12 ,b20)
                     (cl:* ,a13 ,b30))
          ,o20 (cl:+ (cl:* ,a20 ,b00) (cl:* ,a21 ,b10) (cl:* ,a22 ,b20)
                     (cl:* ,a23 ,b30))
          ,o30 (cl:+ (cl:* ,a30 ,b00) (cl:* ,a31 ,b10) (cl:* ,a32 ,b20)
                     (cl:* ,a33 ,b30))
          ,o01 (cl:+ (cl:* ,a00 ,b01) (cl:* ,a01 ,b11) (cl:* ,a02 ,b21)
                     (cl:* ,a03 ,b31))
          ,o11 (cl:+ (cl:* ,a10 ,b01) (cl:* ,a11 ,b11) (cl:* ,a12 ,b21)
                     (cl:* ,a13 ,b31))
          ,o21 (cl:+ (cl:* ,a20 ,b01) (cl:* ,a21 ,b11) (cl:* ,a22 ,b21)
                     (cl:* ,a23 ,b31))
          ,o31 (cl:+ (cl:* ,a30 ,b01) (cl:* ,a31 ,b11) (cl:* ,a32 ,b21)
                     (cl:* ,a33 ,b31))
          ,o02 (cl:+ (cl:* ,a00 ,b02) (cl:* ,a01 ,b12) (cl:* ,a02 ,b22)
                     (cl:* ,a03 ,b32))
          ,o12 (cl:+ (cl:* ,a10 ,b02) (cl:* ,a11 ,b12) (cl:* ,a12 ,b22)
                     (cl:* ,a13 ,b32))
          ,o22 (cl:+ (cl:* ,a20 ,b02) (cl:* ,a21 ,b12) (cl:* ,a22 ,b22)
                     (cl:* ,a23 ,b32))
          ,o32 (cl:+ (cl:* ,a30 ,b02) (cl:* ,a31 ,b12) (cl:* ,a32 ,b22)
                     (cl:* ,a33 ,b32))
          ,o03 (cl:+ (cl:* ,a00 ,b03) (cl:* ,a01 ,b13) (cl:* ,a02 ,b23)
                     (cl:* ,a03 ,b33))
          ,o13 (cl:+ (cl:* ,a10 ,b03) (cl:* ,a11 ,b13) (cl:* ,a12 ,b23)
                     (cl:* ,a13 ,b33))
          ,o23 (cl:+ (cl:* ,a20 ,b03) (cl:* ,a21 ,b13) (cl:* ,a22 ,b23)
                     (cl:* ,a23 ,b33))
          ,o33 (cl:+ (cl:* ,a30 ,b03) (cl:* ,a31 ,b13) (cl:* ,a32 ,b23)
                     (cl:* ,a33 ,b33))))

(int:define-op *! ((out mat) (in1 mat) (in2 mat)) (:out mat)
  (with-components ((o out) (a in1) (b in2))
    (%* o00 o01 o02 o03 o10 o11 o12 o13 o20 o21 o22 o23 o30 o31 o32 o33
        a00 a01 a02 a03 a10 a11 a12 a13 a20 a21 a22 a23 a30 a31 a32 a33
        b00 b01 b02 b03 b10 b11 b12 b13 b20 b21 b22 b23 b30 b31 b32 b33))
  out)

(int:define-op * ((in1 mat) (in2 mat)) (:out mat)
  (*! (mat) in1 in2))

(int:define-op copy-rotation! ((out mat) (in mat)) (:out mat)
  (with-components ((o out) (m in))
    (psetf o00 m00 o01 m01 o02 m02
           o10 m10 o11 m11 o12 m12
           o20 m20 o21 m21 o22 m22))
  out)

(int:define-op copy-rotation ((in mat)) (:out mat)
  (copy-rotation! (mat 1) in))

(int:define-op rotation-to-mat3! ((out dm3:mat) (in mat)) (:out dm3:mat)
  (dm3:with-components ((o out))
    (with-components ((m in))
      (psetf o00 m00 o01 m01 o02 m02
             o10 m10 o11 m11 o12 m12
             o20 m20 o21 m21 o22 m22)))
  out)

(int:define-op rotation-to-mat3 ((in mat)) (:out dm3:mat)
  (rotation-to-mat3! (dm3:mat 1) in))

(int:define-op normalize-rotation! ((out mat) (in mat)) (:out mat)
  (with-components ((o out) (m in))
    (dv3::%normalize o00 o10 o20 m00 m10 m20)
    (dv3::%normalize o01 o11 o21 m01 m11 m21)
    (dv3::%normalize o02 o12 o22 m02 m12 m22))
  out)

(int:define-op normalize-rotation ((in mat)) (:out mat)
  (normalize-rotation! (copy in) in))

(int:define-op get-column! ((out dv4:vec) (in mat) (index (integer 0 3)))
    (:out dv4:vec)
  (with-components ((m in))
    (dv4:with-components ((o out))
      (ecase index
        (0 (psetf ox m00 oy m10 oz m20 ow m30))
        (1 (psetf ox m01 oy m11 oz m21 ow m31))
        (2 (psetf ox m02 oy m12 oz m22 ow m32))
        (3 (psetf ox m03 oy m13 oz m23 ow m33)))))
  out)

(int:define-op get-column ((in mat) (index (integer 0 3))) (:out dv4:vec)
  (get-column! (dv4:vec) in index))

(int:define-op set-column! ((out mat) (in mat) (vec dv4:vec)
                            (index (integer 0 3)))
    (:out mat)
  (with-components ((o out))
    (dv4:with-components ((v vec))
      (copy! out in)
      (ecase index
        (0 (psetf o00 vx o10 vy o20 vz o30 vw))
        (1 (psetf o01 vx o11 vy o21 vz o31 vw))
        (2 (psetf o02 vx o12 vy o22 vz o32 vw))
        (3 (psetf o03 vx o13 vy o23 vz o33 vw)))))
  out)

(int:define-op set-column ((in mat) (vec dv4:vec) (index (integer 0 3)))
    (:out mat)
  (set-column! (mat 1) in vec index))

(int:define-op get-translation! ((out dv3:vec) (in mat)) (:out dv3:vec)
  (with-components ((m in))
    (dv3:with-components ((o out))
      (psetf ox m03 oy m13 oz m23)))
  out)

(int:define-op get-translation ((in mat)) (:out dv3:vec)
  (get-translation! (dv3:vec) in))

(int:define-op set-translation! ((out mat) (in mat) (vec dv3:vec)) (:out mat)
  (with-components ((o out) (m in))
    (dv3:with-components ((v vec))
      (copy-rotation! out in)
      (psetf o03 vx o13 vy o23 vz o33 m33)))
  out)

(int:define-op set-translation ((in mat) (vec dv3:vec)) (:out mat)
  (set-translation! (copy in) in vec))

(int:define-op translate! ((out mat) (in mat) (vec dv3:vec)) (:out mat)
  (with-components ((o out) (m in))
    (dv3:with-components ((v vec))
      (copy! out in)
      (psetf o00 (cl:+ m00 (cl:* m30 vx))
             o01 (cl:+ m01 (cl:* m31 vx))
             o02 (cl:+ m02 (cl:* m32 vx))
             o03 (cl:+ m03 (cl:* m33 vx))
             o10 (cl:+ m10 (cl:* m30 vy))
             o11 (cl:+ m11 (cl:* m31 vy))
             o12 (cl:+ m12 (cl:* m32 vy))
             o13 (cl:+ m13 (cl:* m33 vy))
             o20 (cl:+ m20 (cl:* m30 vz))
             o21 (cl:+ m21 (cl:* m31 vz))
             o22 (cl:+ m22 (cl:* m32 vz))
             o23 (cl:+ m23 (cl:* m33 vz))
             o30 m30
             o31 m31
             o32 m32
             o33 m33)))
  out)

(int:define-op translate ((in mat) (vec dv3:vec)) (:out mat)
  (translate! (mat 1) in vec))

(int:define-op rotation-axis-to-vec3! ((out dv3:vec) (in mat) (axis keyword))
    (:out dv3:vec)
  (dv3:with-components ((v out))
    (with-components ((m in))
      (ecase axis
        (:x (psetf vx m00 vy m10 vz m20))
        (:y (psetf vx m01 vy m11 vz m21))
        (:z (psetf vx m02 vy m12 vz m22)))))
  out)

(int:define-op rotation-axis-to-vec3 ((in mat) (axis keyword)) (:out dv3:vec)
  (rotation-axis-to-vec3! (dv3:vec) in axis))

(int:define-op rotation-axis-from-vec3! ((in mat) (vec dv3:vec) (axis keyword))
    (:out mat)
  (with-components ((m in))
    (dv3:with-components ((v vec))
      (ecase axis
        (:x (psetf m00 vx m10 vy m20 vz))
        (:y (psetf m01 vx m11 vy m21 vz))
        (:z (psetf m02 vx m12 vy m22 vz)))))
  in)

(int:define-op rotation-axis-from-vec3 ((in mat) (vec dv3:vec) (axis keyword))
    (:out mat)
  (rotation-axis-from-vec3! (copy in) vec axis))

(int:define-op rotate! ((out mat) (in mat) (vec dv3:vec)
                        &key (space keyword :local))
    (:out mat :inline nil)
  (macrolet ((rotate-angle (angle s c &body body)
               `(let ((,s (sin ,angle))
                      (,c (cos ,angle)))
                  ,@body
                  (ecase space
                    (:local (dm3::%* o00 o01 o02 o10 o11 o12 o20 o21 o22
                                     o00 o01 o02 o10 o11 o12 o20 o21 o22
                                     m00 m01 m02 m10 m11 m12 m20 m21 m22))
                    (:world (dm3::%* o00 o01 o02 o10 o11 o12 o20 o21 o22
                                     m00 m01 m02 m10 m11 m12 m20 m21 m22
                                     o00 o01 o02 o10 o11 o12 o20 o21 o22))))))
    (dm3:with-elements ((m 1d0 0d0 0d0 0d0 1d0 0d0 0d0 0d0 1d0))
      (with-components ((o out))
        (dv3:with-components ((v vec))
          (copy! out in)
          (rotate-angle vz s c
                        (psetf m00 c m01 (cl:- s) m10 s m11 c))
          (rotate-angle vx s c
                        (psetf m00 1d0 m01 0d0 m02 0d0
                               m10 0d0 m11 c m12 (cl:- s)
                               m20 0d0 m21 s m22 c))
          (rotate-angle vy s c
                        (psetf m00 c m01 0d0 m02 s
                               m10 0d0 m11 1d0 m12 0d0
                               m20 (cl:- s) m21 0d0 m22 c))))))
  out)

(int:define-op rotate ((in mat) (vec dv3:vec)) (:out mat)
  (rotate! (mat 1) in vec))

(int:define-op get-scale! ((out dv3:vec) (in mat)) (:out dv3:vec)
  (dv3:with-components ((o out))
    (psetf ox (dv3:length (rotation-axis-to-vec3 in :x))
           oy (dv3:length (rotation-axis-to-vec3 in :y))
           oz (dv3:length (rotation-axis-to-vec3 in :z))))
  out)

(int:define-op get-scale ((in mat)) (:out dv3:vec)
  (get-scale! (dv3:vec) in))

(int:define-op set-scale! ((out mat) (in mat) (vec dv3:vec)) (:out mat)
  (with-components ((o out))
    (dv3:with-components ((v vec))
      (copy! out in)
      (psetf o00 vx o11 vy o22 vz)))
  out)

(int:define-op set-scale ((in mat) (vec dv3:vec)) (:out mat)
  (set-scale! (copy in) in vec))

(int:define-op scale! ((out mat) (in mat) (vec dv3:vec)) (:out mat)
  (with-components ((o out) (m in))
    (dv3:with-components ((v vec))
      (psetf o00 (cl:* m00 vx)
             o01 (cl:* m01 vx)
             o02 (cl:* m02 vx)
             o03 (cl:* m03 vx)
             o10 (cl:* m10 vy)
             o11 (cl:* m11 vy)
             o12 (cl:* m12 vy)
             o13 (cl:* m13 vy)
             o20 (cl:* m20 vz)
             o21 (cl:* m21 vz)
             o22 (cl:* m22 vz)
             o23 (cl:* m23 vz)
             o30 m30
             o31 m31
             o32 m32
             o33 m33)))
  out)

(int:define-op scale ((in mat) (vec dv3:vec)) (:out mat)
  (scale! (mat 1) in vec))

(defmacro %*v4! (ox oy oz ow
                 m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33
                 vx vy vz vw)
  `(psetf ,ox (cl:+ (cl:* ,m00 ,vx) (cl:* ,m01 ,vy) (cl:* ,m02 ,vz)
                    (cl:* ,m03 ,vw))
          ,oy (cl:+ (cl:* ,m10 ,vx) (cl:* ,m11 ,vy) (cl:* ,m12 ,vz)
                    (cl:* ,m13 ,vw))
          ,oz (cl:+ (cl:* ,m20 ,vx) (cl:* ,m21 ,vy) (cl:* ,m22 ,vz)
                    (cl:* ,m23 ,vw))
          ,ow (cl:+ (cl:* ,m30 ,vx) (cl:* ,m31 ,vy) (cl:* ,m32 ,vz)
                    (cl:* ,m33 ,vw))))

(int:define-op *v4! ((out dv4:vec) (in mat) (vec dv4:vec)) (:out dv4:vec)
  (dv4:with-components ((v vec) (o out))
    (with-components ((m in))
      (%*v4! ox oy oz ow
             m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33
             vx vy vz vw)))
  out)

(int:define-op *v4 ((in mat) (vec dv4:vec)) (:out dv4:vec)
  (*v4! (dv4:vec) in vec))

(int:define-op transpose! ((out mat) (in mat)) (:out mat)
  (with-components ((o (copy! out in)))
    (rotatef o01 o10)
    (rotatef o02 o20)
    (rotatef o03 o30)
    (rotatef o12 o21)
    (rotatef o13 o31)
    (rotatef o23 o32))
  out)

(int:define-op transpose ((in mat)) (:out mat)
  (transpose! (mat 1) in))

(int:define-op orthogonal-p ((in mat)) (:out boolean :inline nil)
  (~ (* in (transpose in)) +id+))

(int:define-op orthonormalize! ((out mat) (in mat)) (:out mat :inline nil)
  (let* ((x (rotation-axis-to-vec3 in :x))
         (y (rotation-axis-to-vec3 in :y))
         (z (rotation-axis-to-vec3 in :z)))
    (dv3:normalize! x x)
    (dv3:normalize! y (dv3:- y (dv3:scale x (dv3:dot y x))))
    (dv3:cross! z x y)
    (rotation-axis-from-vec3! out x :x)
    (rotation-axis-from-vec3! out y :y)
    (rotation-axis-from-vec3! out z :z))
  out)

(int:define-op orthonormalize ((in mat)) (:out mat)
  (orthonormalize! (mat 1) in))

(defmacro %trace (m00 m11 m22 m33)
  `(cl:+ ,m00 ,m11 ,m22 ,m33))

(int:define-op trace ((in mat)) (:out double-float :speed nil)
  (with-components ((m in))
    (%trace m00 m11 m22 m33)))

(int:define-op diagonal-p ((in mat)) (:out boolean)
  (with-components ((m in))
    (cl:= 0d0 m10 m20 m30 m01 m21 m31 m02 m12 m32 m03 m13 m23)))

(int:define-op main-diagonal! ((out dv4:vec) (in mat)) (:out dv4:vec)
  (with-components ((m in))
    (dv4:with-components ((v out))
      (psetf vx m00 vy m11 vz m22 vw m33)))
  out)

(int:define-op main-diagonal ((in mat)) (:out dv4:vec)
  (main-diagonal! (dv4:vec) in))

(int:define-op anti-diagonal! ((out dv4:vec) (in mat)) (:out dv4:vec)
  (with-components ((m in))
    (dv4:with-components ((v out))
      (psetf vx m03 vy m12 vz m21 vw m30)))
  out)

(int:define-op anti-diagonal ((in mat)) (:out dv4:vec)
  (anti-diagonal! (dv4:vec) in))

(int:define-op determinant ((in mat)) (:out double-float :speed nil)
  (with-components ((m in))
    (cl:- (cl:+ (cl:* m00 m11 m22 m33) (cl:* m00 m12 m23 m31)
                (cl:* m00 m13 m21 m32) (cl:* m01 m10 m23 m32)
                (cl:* m01 m12 m20 m33) (cl:* m01 m13 m22 m30)
                (cl:* m02 m10 m21 m33) (cl:* m02 m11 m23 m30)
                (cl:* m02 m13 m20 m31) (cl:* m03 m10 m22 m31)
                (cl:* m03 m11 m20 m32) (cl:* m03 m12 m21 m30))
          (cl:* m00 m11 m23 m32) (cl:* m00 m12 m21 m33) (cl:* m00 m13 m22 m31)
          (cl:* m01 m10 m22 m33) (cl:* m01 m12 m23 m30) (cl:* m01 m13 m20 m32)
          (cl:* m02 m10 m23 m31) (cl:* m02 m11 m20 m33) (cl:* m02 m13 m21 m30)
          (cl:* m03 m10 m21 m32) (cl:* m03 m11 m22 m30)
          (cl:* m03 m12 m20 m31))))

(int:define-op invert-orthogonal! ((out mat) (in mat)) (:out mat)
  (copy! out in)
  (with-components ((o out))
    (rotatef o10 o01)
    (rotatef o20 o02)
    (rotatef o21 o12)
    (psetf o03 (cl:+ (cl:* o00 (cl:- o03)) (cl:* o01 (cl:- o13))
                     (cl:* o02 (cl:- o23)))
           o13 (cl:+ (cl:* o10 (cl:- o03)) (cl:* o11 (cl:- o13))
                     (cl:* o12 (cl:- o23)))
           o23 (cl:+ (cl:* o20 (cl:- o03)) (cl:* o21 (cl:- o13))
                     (cl:* o22 (cl:- o23)))))
  out)

(int:define-op invert-orthogonal ((in mat)) (:out mat)
  (invert-orthogonal! (mat 1) in))

(int:define-op invert! ((out mat) (in mat)) (:out (values mat boolean)
                                             :inline nil)
  (let ((determinant (determinant in)))
    (when (< (abs determinant) 1d-15)
      (return-from invert! (values in nil)))
    (with-components ((o out) (m in))
      (psetf o00 (/ (cl:- (cl:+ (cl:* m11 m22 m33) (cl:* m12 m23 m31)
                                (cl:* m13 m21 m32))
                          (cl:* m11 m23 m32) (cl:* m12 m21 m33)
                          (cl:* m13 m22 m31))
                    determinant)
             o01 (/ (cl:- (cl:+ (cl:* m01 m23 m32) (cl:* m02 m21 m33)
                                (cl:* m03 m22 m31))
                          (cl:* m01 m22 m33) (cl:* m02 m23 m31)
                          (cl:* m03 m21 m32))
                    determinant)
             o02 (/ (cl:- (cl:+ (cl:* m01 m12 m33) (cl:* m02 m13 m31)
                                (cl:* m03 m11 m32))
                          (cl:* m01 m13 m32) (cl:* m02 m11 m33)
                          (cl:* m03 m12 m31))
                    determinant)
             o03 (/ (cl:- (cl:+ (cl:* m01 m13 m22) (cl:* m02 m11 m23)
                                (cl:* m03 m12 m21))
                          (cl:* m01 m12 m23) (cl:* m02 m13 m21)
                          (cl:* m03 m11 m22))
                    determinant)
             o10 (/ (cl:- (cl:+ (cl:* m10 m23 m32) (cl:* m12 m20 m33)
                                (cl:* m13 m22 m30))
                          (cl:* m10 m22 m33) (cl:* m12 m23 m30)
                          (cl:* m13 m20 m32))
                    determinant)
             o11 (/ (cl:- (cl:+ (cl:* m00 m22 m33) (cl:* m02 m23 m30)
                                (cl:* m03 m20 m32))
                          (cl:* m00 m23 m32) (cl:* m02 m20 m33)
                          (cl:* m03 m22 m30))
                    determinant)
             o12 (/ (cl:- (cl:+ (cl:* m00 m13 m32) (cl:* m02 m10 m33)
                                (cl:* m03 m12 m30))
                          (cl:* m00 m12 m33) (cl:* m02 m13 m30)
                          (cl:* m03 m10 m32))
                    determinant)
             o13 (/ (cl:- (cl:+ (cl:* m00 m12 m23) (cl:* m02 m13 m20)
                                (cl:* m03 m10 m22))
                          (cl:* m00 m13 m22) (cl:* m02 m10 m23)
                          (cl:* m03 m12 m20))
                    determinant)
             o20 (/ (cl:- (cl:+ (cl:* m10 m21 m33) (cl:* m11 m23 m30)
                                (cl:* m13 m20 m31))
                          (cl:* m10 m23 m31) (cl:* m11 m20 m33)
                          (cl:* m13 m21 m30))
                    determinant)
             o21 (/ (cl:- (cl:+ (cl:* m00 m23 m31) (cl:* m01 m20 m33)
                                (cl:* m03 m21 m30))
                          (cl:* m00 m21 m33) (cl:* m01 m23 m30)
                          (cl:* m03 m20 m31))
                    determinant)
             o22 (/ (cl:- (cl:+ (cl:* m00 m11 m33) (cl:* m01 m13 m30)
                                (cl:* m03 m10 m31))
                          (cl:* m00 m13 m31) (cl:* m01 m10 m33)
                          (cl:* m03 m11 m30))
                    determinant)
             o23 (/ (cl:- (cl:+ (cl:* m00 m13 m21) (cl:* m01 m10 m23)
                                (cl:* m03 m11 m20))
                          (cl:* m00 m11 m23) (cl:* m01 m13 m20)
                          (cl:* m03 m10 m21))
                    determinant)
             o30 (/ (cl:- (cl:+ (cl:* m10 m22 m31) (cl:* m11 m20 m32)
                                (cl:* m12 m21 m30))
                          (cl:* m10 m21 m32) (cl:* m11 m22 m30)
                          (cl:* m12 m20 m31))
                    determinant)
             o31 (/ (cl:- (cl:+ (cl:* m00 m21 m32) (cl:* m01 m22 m30)
                                (cl:* m02 m20 m31))
                          (cl:* m00 m22 m31) (cl:* m01 m20 m32)
                          (cl:* m02 m21 m30))
                    determinant)
             o32 (/ (cl:- (cl:+ (cl:* m00 m12 m31) (cl:* m01 m10 m32)
                                (cl:* m02 m11 m30))
                          (cl:* m00 m11 m32) (cl:* m01 m12 m30)
                          (cl:* m02 m10 m31))
                    determinant)
             o33 (/ (cl:- (cl:+ (cl:* m00 m11 m22) (cl:* m01 m12 m20)
                                (cl:* m02 m10 m21))
                          (cl:* m00 m12 m21) (cl:* m01 m10 m22)
                          (cl:* m02 m11 m20))
                    determinant))))
  (values out t))

(int:define-op invert ((in mat)) (:out mat)
  (invert! (mat 1) in))

(int:define-op set-view! ((out mat) (eye dv3:vec) (target dv3:vec) (up dv3:vec))
    (:out mat :inline nil)
  (with-components ((o (id! out)))
    (dv3:with-components ((e eye) (s target) (u up))
      (dv3:with-elements ((a (cl:- sx ex) (cl:- sy ey) (cl:- sz ez)))
        (dv3::%normalize ax ay az ax ay az)
        (psetf o20 ax o21 ay o22 az)
        (dv3:with-elements ((b (cl:- (cl:* o21 uz) (cl:* o22 uy))
                               (cl:- (cl:* o22 ux) (cl:* o20 uz))
                               (cl:- (cl:* o20 uy) (cl:* o21 ux))))
          (dv3::%normalize bx by bz bx by bz)
          (psetf o00 bx o01 by o02 bz))
        (psetf o10 (cl:- (cl:* o01 o22) (cl:* o02 o21))
               o11 (cl:- (cl:* o02 o20) (cl:* o00 o22))
               o12 (cl:- (cl:* o00 o21) (cl:* o01 o20))
               o20 (cl:- o20)
               o21 (cl:- o21)
               o22 (cl:- o22))
        (psetf o03 (cl:+ (cl:* o00 (cl:- ex))
                         (cl:* o01 (cl:- ey))
                         (cl:* o02 (cl:- ez))
                         o03)
               o13 (cl:+ (cl:* o10 (cl:- ex))
                         (cl:* o11 (cl:- ey))
                         (cl:* o12 (cl:- ez))
                         o13)
               o23 (cl:+ (cl:* o20 (cl:- ex))
                         (cl:* o21 (cl:- ey))
                         (cl:* o22 (cl:- ez))
                         o23)
               o33 (cl:+ (cl:* o30 (cl:- ex))
                         (cl:* o31 (cl:- ey))
                         (cl:* o32 (cl:- ez))
                         o33)))))
  out)

(int:define-op set-view ((eye dv3:vec) (target dv3:vec) (up dv3:vec)) (:out mat)
  (set-view! (mat 1) eye target up))

(int:define-op set-projection/orthographic! ((out mat) (left double-float)
                                             (right double-float)
                                             (bottom double-float)
                                             (top double-float)
                                             (near double-float)
                                             (far double-float))
    (:out mat :inline nil)
  (let ((right-left (cl:- right left))
        (top-bottom (cl:- top bottom))
        (far-near (cl:- far near)))
    (with-components ((m (id! out)))
      (psetf m00 (/ 2f0 right-left)
             m03 (cl:- (/ (cl:+ right left) right-left))
             m11 (/ 2f0 top-bottom)
             m13 (cl:- (/ (cl:+ top bottom) top-bottom))
             m22 (/ -2f0 far-near)
             m23 (cl:- (/ (cl:+ far near) far-near))))
    out))

(int:define-op set-projection/orthographic ((left double-float)
                                            (right double-float)
                                            (bottom double-float)
                                            (top double-float)
                                            (near double-float)
                                            (far double-float))
    (:out mat)
  (set-projection/orthographic! (mat 1) left right bottom top near far))

(int:define-op set-projection/perspective! ((out mat) (fov double-float)
                                            (aspect double-float)
                                            (near double-float)
                                            (far double-float))
    (:out mat)
  (let ((f (/ (tan (/ fov 2f0))))
        (z (cl:- near far)))
    (with-components ((m (zero! out)))
      (psetf m00 (cl:* f (cl:/ aspect))
             m11 f
             m22 (/ (cl:+ near far) z)
             m23 (/ (cl:* 2 near far) z)
             m32 -1d0)))
  out)

(int:define-op set-projection/perspective ((fov double-float)
                                           (aspect double-float)
                                           (near double-float)
                                           (far double-float))
    (:out mat)
  (set-projection/perspective! (mat 1) fov aspect near far))
