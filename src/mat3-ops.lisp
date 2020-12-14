(in-package #:net.mfiano.lisp.origin.mat3)

;;; constructors

(int:define-op %mat (&rest (args single-float)) (:inline t :out mat)
  (make-array 9 :element-type 'single-float :initial-contents args))

(ss:defstore mat (&rest args))

(ss:defspecialization (mat :inline t) () mat
  (%mat 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0))

(ss:defspecialization (mat :inline t) ((x real)) mat
  (%mat (float x 1f0) 0f0 0f0
        0f0 (float x 1f0) 0f0
        0f0 0f0 (float x 1f0)))

(ss:defspecialization (mat :inline t) ((mat m2:mat)) mat
  (m2:with-components ((m mat))
    (%mat m00 m10 0f0 m01 m11 0f0 0f0 0f0 1f0)))

(ss:defspecialization (mat :inline t) ((mat mat)) mat
  (with-components ((m mat))
    (%mat m00 m10 m20 m01 m11 m21 m02 m12 m22)))

(ss:defspecialization (mat :inline t) ((mat net.mfiano.lisp.origin.mat4:mat))
    mat
  (net.mfiano.lisp.origin.mat4:with-components ((m mat))
    (%mat m00 m01 m02 m10 m11 m12 m20 m21 m22)))

(ss:defspecialization (mat :inline t) ((a v3:vec) (b v3:vec) (c v3:vec)) mat
  (v3:with-components ((a a) (b b) (c c))
    (%mat ax ay az bx by bz cx cy cz)))

(ss:defspecialization (mat :inline t) ((a real) (b real) (c real)
                                       (d real) (e real) (f real)
                                       (g real) (h real) (i real))
    mat
  (%mat (float a 1f0) (float b 1f0) (float c 1f0)
        (float d 1f0) (float e 1f0) (float f 1f0)
        (float g 1f0) (float h 1f0) (float i 1f0)))

(ss:defspecialization (mat :inline t) ((mat net.mfiano.lisp.origin.dmat3:mat))
    mat
  (net.mfiano.lisp.origin.dmat3:with-components ((m mat))
    (%mat (float m00 1f0) (float m10 1f0) (float m20 1f0)
          (float m01 1f0) (float m11 1f0) (float m21 1f0)
          (float m02 1f0) (float m12 1f0) (float m22 1f0))))

;;; constants

(u:define-constant +zero+ (%mat 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0)
  :test #'equalp)

(u:define-constant +id+ (%mat 1f0 0f0 0f0 0f0 1f0 0f0 0f0 0f0 1f0)
  :test #'equalp)

;;; operators

(int:define-op zero! ((in mat)) (:out mat)
  (with-components ((m in))
    (psetf m00 0f0 m01 0f0 m02 0f0
           m10 0f0 m11 0f0 m12 0f0
           m20 0f0 m21 0f0 m22 0f0))
  in)

(int:define-op zero-p ((in mat)) (:out boolean)
  (with-components ((m in))
    (cl:= 0f0 m00 m01 m02 m10 m11 m12 m20 m21 m22)))

(int:define-op id! ((in mat)) (:out mat)
  (with-components ((m in))
    (psetf m00 1f0 m01 0f0 m02 0f0
           m10 0f0 m11 1f0 m12 0f0
           m20 0f0 m21 0f0 m22 1f0))
  in)

(int:define-op id-p ((in mat)) (:out boolean)
  (with-components ((m in))
    (and (cl:= 0f0 m01 m02 m10 m12 m20 m21)
         (cl:= 1f0 m00 m11 m22))))

(int:define-op = ((in1 mat) (in2 mat)
                  &key (rel single-float 1e-7) (abs single-float rel))
    (:out boolean)
  (with-components ((a in1) (b in2))
    (and (<= (abs (cl:- a00 b00))
             (max abs (cl:* rel (max (abs a00) (abs b00)))))
         (<= (abs (cl:- a01 b01))
             (max abs (cl:* rel (max (abs a01) (abs b01)))))
         (<= (abs (cl:- a02 b02))
             (max abs (cl:* rel (max (abs a02) (abs b02)))))
         (<= (abs (cl:- a10 b10))
             (max abs (cl:* rel (max (abs a10) (abs b10)))))
         (<= (abs (cl:- a11 b11))
             (max abs (cl:* rel (max (abs a11) (abs b11)))))
         (<= (abs (cl:- a12 b12))
             (max abs (cl:* rel (max (abs a12) (abs b12)))))
         (<= (abs (cl:- a20 b20))
             (max abs (cl:* rel (max (abs a20) (abs b20)))))
         (<= (abs (cl:- a21 b21))
             (max abs (cl:* rel (max (abs a21) (abs b21)))))
         (<= (abs (cl:- a22 b22))
             (max abs (cl:* rel (max (abs a22) (abs b22))))))))

(int:define-op random! ((out mat) (min single-float) (max single-float))
    (:out mat)
  (with-components ((o out))
    (psetf o00 (cl:+ min (cl:random (cl:- max min)))
           o01 (cl:+ min (cl:random (cl:- max min)))
           o02 (cl:+ min (cl:random (cl:- max min)))
           o10 (cl:+ min (cl:random (cl:- max min)))
           o11 (cl:+ min (cl:random (cl:- max min)))
           o12 (cl:+ min (cl:random (cl:- max min)))
           o20 (cl:+ min (cl:random (cl:- max min)))
           o21 (cl:+ min (cl:random (cl:- max min)))
           o22 (cl:+ min (cl:random (cl:- max min)))))
  out)

(int:define-op random ((min single-float) (max single-float)) (:out mat)
  (random! (mat) min max))

(int:define-op copy! ((out mat) (in mat)) (:out mat)
  (with-components ((o out) (m in))
    (psetf o00 m00 o01 m01 o02 m02
           o10 m10 o11 m11 o12 m12
           o20 m20 o21 m21 o22 m22))
  out)

(int:define-op copy ((in mat)) (:out mat)
  (copy! (mat) in))

(int:define-op clamp! ((out mat) (in mat)
                       &key
                       (min single-float most-negative-single-float)
                       (max single-float most-positive-single-float))
    (:out mat)
  (with-components ((o out) (m in))
    (psetf o00 (u:clamp m00 min max)
           o01 (u:clamp m01 min max)
           o02 (u:clamp m02 min max)
           o10 (u:clamp m10 min max)
           o11 (u:clamp m11 min max)
           o12 (u:clamp m12 min max)
           o20 (u:clamp m20 min max)
           o21 (u:clamp m21 min max)
           o22 (u:clamp m22 min max)))
  out)

(int:define-op clamp ((in mat)
                      &key
                      (min single-float most-negative-single-float)
                      (max single-float most-positive-single-float))
    (:out mat)
  (clamp! (mat) in :min min :max max))

(int:define-op +! ((out mat) (in1 mat) (in2 mat)) (:out mat)
  (with-components ((o out) (a in1) (b in2))
    (psetf o00 (cl:+ a00 b00)
           o10 (cl:+ a10 b10)
           o20 (cl:+ a20 b20)
           o01 (cl:+ a01 b01)
           o11 (cl:+ a11 b11)
           o21 (cl:+ a21 b21)
           o02 (cl:+ a02 b02)
           o12 (cl:+ a12 b12)
           o22 (cl:+ a22 b22)))
  out)

(int:define-op + ((in1 mat) (in2 mat)) (:out mat)
  (+! (mat) in1 in2))

(int:define-op -! ((out mat) (in1 mat) (in2 mat)) (:out mat)
  (with-components ((o out) (a in1) (b in2))
    (psetf o00 (cl:- a00 b00)
           o10 (cl:- a10 b10)
           o20 (cl:- a20 b20)
           o01 (cl:- a01 b01)
           o11 (cl:- a11 b11)
           o21 (cl:- a21 b21)
           o02 (cl:- a02 b02)
           o12 (cl:- a12 b12)
           o22 (cl:- a22 b22)))
  out)

(int:define-op - ((in1 mat) (in2 mat)) (:out mat)
  (-! (mat) in1 in2))

(defmacro %* (o00 o01 o02 o10 o11 o12 o20 o21 o22
              a00 a01 a02 a10 a11 a12 a20 a21 a22
              b00 b01 b02 b10 b11 b12 b20 b21 b22)
  `(psetf ,o00 (cl:+ (cl:* ,a00 ,b00) (cl:* ,a01 ,b10) (cl:* ,a02 ,b20))
          ,o10 (cl:+ (cl:* ,a10 ,b00) (cl:* ,a11 ,b10) (cl:* ,a12 ,b20))
          ,o20 (cl:+ (cl:* ,a20 ,b00) (cl:* ,a21 ,b10) (cl:* ,a22 ,b20))
          ,o01 (cl:+ (cl:* ,a00 ,b01) (cl:* ,a01 ,b11) (cl:* ,a02 ,b21))
          ,o11 (cl:+ (cl:* ,a10 ,b01) (cl:* ,a11 ,b11) (cl:* ,a12 ,b21))
          ,o21 (cl:+ (cl:* ,a20 ,b01) (cl:* ,a21 ,b11) (cl:* ,a22 ,b21))
          ,o02 (cl:+ (cl:* ,a00 ,b02) (cl:* ,a01 ,b12) (cl:* ,a02 ,b22))
          ,o12 (cl:+ (cl:* ,a10 ,b02) (cl:* ,a11 ,b12) (cl:* ,a12 ,b22))
          ,o22 (cl:+ (cl:* ,a20 ,b02) (cl:* ,a21 ,b12) (cl:* ,a22 ,b22))))

(int:define-op *! ((out mat) (in1 mat) (in2 mat)) (:out mat)
  (with-components ((o out) (a in1) (b in2))
    (%* o00 o01 o02 o10 o11 o12 o20 o21 o22
        a00 a01 a02 a10 a11 a12 a20 a21 a22
        b00 b01 b02 b10 b11 b12 b20 b21 b22))
  out)

(int:define-op * ((in1 mat) (in2 mat)) (:out mat)
  (*! (mat) in1 in2))

(int:define-op copy-rotation! ((out mat) (in mat)) (:out mat)
  (with-components ((o out) (m in))
    (psetf o00 m00 o01 m01
           o10 m10 o11 m11))
  out)

(int:define-op copy-rotation ((in mat)) (:out mat)
  (copy-rotation! (mat 1) in))

(int:define-op rotation-to-mat2! ((out m2:mat) (in mat)) (:out m2:mat)
  (m2:with-components ((o out))
    (with-components ((m in))
      (psetf o00 m00 o01 m01
             o10 m10 o11 m11)))
  out)

(int:define-op rotation-to-mat2 ((in mat)) (:out m2:mat)
  (rotation-to-mat2! (m2:mat 1) in))

(int:define-op normalize-rotation! ((out mat) (in mat)) (:out mat)
  (with-components ((o out) (m in))
    (v2::%normalize o00 o10 m00 m10)
    (v2::%normalize o01 o11 m01 m11)
    (v2::%normalize o02 o12 m02 m12))
  out)

(int:define-op normalize-rotation ((in mat)) (:out mat)
  (normalize-rotation! (copy in) in))

(int:define-op get-column! ((out v3:vec) (in mat) (index (integer 0 2)))
    (:out v3:vec)
  (with-components ((m in))
    (v3:with-components ((o out))
      (ecase index
        (0 (psetf ox m00 oy m10 oz m20))
        (1 (psetf ox m01 oy m11 oz m21))
        (2 (psetf ox m02 oy m12 oz m22)))))
  out)

(int:define-op get-column ((in mat) (index (integer 0 2))) (:out v3:vec)
  (get-column! (v3:vec) in index))

(int:define-op set-column! ((out mat) (in mat) (vec v3:vec)
                            (index (integer 0 2)))
    (:out mat)
  (with-components ((o out))
    (v3:with-components ((v vec))
      (copy! out in)
      (ecase index
        (0 (psetf o00 vx o10 vy o20 vz))
        (1 (psetf o01 vx o11 vy o21 vz))
        (2 (psetf o02 vx o12 vy o22 vz)))))
  out)

(int:define-op set-column ((in mat) (vec v3:vec) (index (integer 0 2)))
    (:out mat)
  (set-column! (mat 1) in vec index))

(int:define-op get-translation! ((out v2:vec) (in mat)) (:out v2:vec)
  (with-components ((m in))
    (v2:with-components ((o out))
      (psetf ox m02 oy m12)))
  out)

(int:define-op get-translation ((in mat)) (:out v2:vec)
  (get-translation! (v2:vec) in))

(int:define-op set-translation! ((out mat) (in mat) (vec v2:vec)) (:out mat)
  (with-components ((o out) (m in))
    (v2:with-components ((v vec))
      (copy-rotation! out in)
      (psetf o02 vx o12 vy o22 m22)))
  out)

(int:define-op set-translation ((in mat) (vec v2:vec)) (:out mat)
  (set-translation! (copy in) in vec))

(int:define-op translate! ((out mat) (in mat) (vec v2:vec)) (:out mat)
  (with-components ((o out) (m in))
    (v2:with-components ((v vec))
      (copy! out in)
      (psetf o00 (cl:+ m00 (cl:* m20 vx))
             o01 (cl:+ m01 (cl:* m21 vx))
             o02 (cl:+ m02 (cl:* m22 vx))
             o10 (cl:+ m10 (cl:* m20 vy))
             o11 (cl:+ m11 (cl:* m21 vy))
             o12 (cl:+ m12 (cl:* m22 vy))
             o20 m20
             o21 m21
             o22 m22)))
  out)

(int:define-op translate ((in mat) (vec v2:vec)) (:out mat)
  (translate! (mat 1) in vec))

(int:define-op rotation-axis-to-vec2! ((out v2:vec) (in mat) (axis keyword))
    (:out v2:vec)
  (v2:with-components ((v out))
    (with-components ((m in))
      (ecase axis
        (:x (psetf vx m00 vy m10))
        (:y (psetf vx m01 vy m11)))))
  out)

(int:define-op rotation-axis-to-vec2 ((in mat) (axis keyword)) (:out v2:vec)
  (rotation-axis-to-vec2! (v2:vec) in axis))

(int:define-op rotation-axis-from-vec2! ((in mat) (vec v2:vec) (axis keyword))
    (:out mat)
  (with-components ((m in))
    (v2:with-components ((v vec))
      (ecase axis
        (:x (psetf m00 vx m10 vy))
        (:y (psetf m01 vx m11 vy)))))
  in)

(int:define-op rotation-axis-from-vec2 ((in mat) (vec v2:vec) (axis keyword))
    (:out mat)
  (rotation-axis-from-vec2! (copy in) vec axis))

(int:define-op rotate! ((out mat) (in mat) (angle single-float)
                        &key (space keyword :local))
    (:out mat)
  (m2:with-elements ((m 1f0 0f0 0f0 1f0))
    (with-components ((o out))
      (let ((s (sin angle))
            (c (cos angle)))
        (copy! out in)
        (psetf m00 c m01 (cl:- s) m10 s m11 c)
        (ecase space
          (:local (m2::%* o00 o01 o10 o11 o00 o01 o10 o11 m00 m01 m10 m11))
          (:world (m2::%* o00 o01 o10 o11 m00 m01 m10 m11 o00 o01 010 011))))))
  out)

(int:define-op rotate ((in mat) (angle float)) (:out mat)
  (rotate! (mat 1) in angle))

(int:define-op get-scale! ((out v2:vec) (in mat)) (:out v2:vec)
  (v2:with-components ((o out))
    (psetf ox (v2:length (rotation-axis-to-vec2 in :x))
           oy (v2:length (rotation-axis-to-vec2 in :y))))
  out)

(int:define-op get-scale ((in mat)) (:out v2:vec)
  (get-scale! (v2:vec) in))

(int:define-op set-scale! ((out mat) (in mat) (vec v2:vec)) (:out mat)
  (with-components ((o out))
    (v2:with-components ((v vec))
      (copy! out in)
      (psetf o00 vx o11 vy)))
  out)

(int:define-op set-scale ((in mat) (vec v2:vec)) (:out mat)
  (set-scale! (copy in) in vec))

(int:define-op scale! ((out mat) (in mat) (vec v2:vec)) (:out mat)
  (with-components ((o out) (m in))
    (v2:with-components ((v vec))
      (psetf o00 (cl:* m00 vx)
             o01 (cl:* m01 vx)
             o02 (cl:* m02 vx)
             o10 (cl:* m10 vy)
             o11 (cl:* m11 vy)
             o12 (cl:* m12 vy)
             o20 m20
             o21 m21
             o22 m22)))
  out)

(int:define-op scale ((in mat) (vec v2:vec)) (:out mat)
  (scale! (mat 1) in vec))

(int:define-op *v3! ((out v3:vec) (in mat) (vec v3:vec)) (:out v3:vec)
  (v3:with-components ((v vec) (o out))
    (with-components ((m in))
      (psetf ox (cl:+ (cl:* m00 vx) (cl:* m01 vy) (cl:* m02 vz))
             oy (cl:+ (cl:* m10 vx) (cl:* m11 vy) (cl:* m12 vz))
             oz (cl:+ (cl:* m20 vx) (cl:* m21 vy) (cl:* m22 vz)))))
  out)

(int:define-op *v3 ((in mat) (vec v3:vec)) (:out v3:vec)
  (*v3! (v3:vec) in vec))

(int:define-op transpose! ((out mat) (in mat)) (:out mat)
  (with-components ((o (copy! out in)))
    (rotatef o01 o10)
    (rotatef o02 o20)
    (rotatef o12 o21))
  out)

(int:define-op transpose ((in mat)) (:out mat)
  (transpose! (mat 1) in))

(int:define-op orthogonal-p ((in mat)) (:out boolean :inline nil)
  (= (* in (transpose in)) +id+))

(defmacro %trace (m00 m11 m22)
  `(cl:+ ,m00 ,m11 ,m22))

(int:define-op trace ((in mat)) (:out single-float)
  (with-components ((m in))
    (%trace m00 m11 m22)))

(int:define-op diagonal-p ((in mat)) (:out boolean)
  (with-components ((m in))
    (cl:= 0f0 m10 m20 m01 m21 m02 m12)))

(int:define-op main-diagonal! ((out v3:vec) (in mat)) (:out v3:vec)
  (with-components ((m in))
    (v3:with-components ((v out))
      (psetf vx m00 vy m11 vz m22)))
  out)

(int:define-op main-diagonal ((in mat)) (:out v3:vec)
  (main-diagonal! (v3:vec) in))

(int:define-op anti-diagonal! ((out v3:vec) (in mat)) (:out v3:vec)
  (with-components ((m in))
    (v3:with-components ((v out))
      (psetf vx m02 vy m11 vz m20)))
  out)

(int:define-op anti-diagonal ((in mat)) (:out v3:vec)
  (anti-diagonal! (v3:vec) in))
