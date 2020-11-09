(in-package #:net.mfiano.lisp.origin.dmat3)

;;; constructors

(int:define-op %mat (&rest (args double-float)) (:inline t :out mat)
  (make-array 9 :element-type 'double-float :initial-contents args))

(ss:defstore mat (&rest args))

(ss:defspecialization (mat :inline t) () mat
  (%mat 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0))

(ss:defspecialization (mat :inline t) ((x real)) mat
  (%mat (float x 1d0) 0d0 0d0
        0d0 (float x 1d0) 0d0
        0d0 0d0 (float x 1d0)))

(ss:defspecialization (mat :inline t) ((mat dm2:mat)) mat
  (dm2:with-components ((m mat))
    (%mat m00 m10 0d0 m01 m11 0d0 0d0 0d0 1d0)))

(ss:defspecialization (mat :inline t) ((mat mat)) mat
  (with-components ((m mat))
    (%mat m00 m10 m20 m01 m11 m21 m02 m12 m22)))

(ss:defspecialization (mat :inline t) ((mat net.mfiano.lisp.origin.dmat4:mat))
    mat
  (net.mfiano.lisp.origin.dmat4:with-components ((m mat))
    (%mat m00 m10 m20 m01 m11 m21 m02 m12 m22)))

(ss:defspecialization (mat :inline t) ((a dv3:vec) (b dv3:vec) (c dv3:vec)) mat
  (dv3:with-components ((a a) (b b) (c c))
    (%mat ax ay az bx by bz cx cy cz)))

(ss:defspecialization (mat :inline t) ((a real) (b real) (c real)
                                       (d real) (e real) (f real)
                                       (g real) (h real) (i real))
    mat
  (%mat (float a 1d0) (float b 1d0) (float c 1d0)
        (float d 1d0) (float e 1d0) (float f 1d0)
        (float g 1d0) (float h 1d0) (float i 1d0)))

(ss:defspecialization (mat :inline t) ((mat m3:mat))
    mat
  (m3:with-components ((m mat))
    (%mat (float m00 1d0) (float m10 1d0) (float m20 1d0)
          (float m01 1d0) (float m11 1d0) (float m21 1d0)
          (float m02 1d0) (float m12 1d0) (float m22 1d0))))

;;; constants

(u:define-constant +zero+ (%mat 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0)
  :test #'equalp)

(u:define-constant +id+ (%mat 1d0 0d0 0d0 0d0 1d0 0d0 0d0 0d0 1d0)
  :test #'equalp)

;;; operators

(int:define-op zero! ((in mat)) (:out mat)
  (with-components ((m in))
    (psetf m00 0d0 m01 0d0 m02 0d0
           m10 0d0 m11 0d0 m12 0d0
           m20 0d0 m21 0d0 m22 0d0))
  in)

(int:define-op zero-p ((in mat)) (:out boolean)
  (with-components ((m in))
    (cl:= 0d0 m00 m01 m02 m10 m11 m12 m20 m21 m22)))

(int:define-op id! ((in mat)) (:out mat)
  (with-components ((m in))
    (psetf m00 1d0 m01 0d0 m02 0d0
           m10 0d0 m11 1d0 m12 0d0
           m20 0d0 m21 0d0 m22 1d0))
  in)

(int:define-op id-p ((in mat)) (:out boolean)
  (with-components ((m in))
    (and (cl:= 0d0 m01 m02 m10 m12 m20 m21)
         (cl:= 1d0 m00 m11 m22))))

(int:define-op = ((in1 mat) (in2 mat)) (:out boolean)
  (with-components ((a in1) (b in2))
    (and (cl:= a00 b00) (cl:= a01 b01) (cl:= a02 b02)
         (cl:= a10 b10) (cl:= a11 b11) (cl:= a12 b12)
         (cl:= a20 b20) (cl:= a21 b21) (cl:= a22 b22))))

(int:define-op ~ ((in1 mat) (in2 mat) &key (tolerance double-float 1d-7))
    (:out boolean)
  (with-components ((a in1) (b in2))
    (and (cl:< (cl:abs (cl:- a00 b00)) tolerance)
         (cl:< (cl:abs (cl:- a01 b01)) tolerance)
         (cl:< (cl:abs (cl:- a02 b02)) tolerance)
         (cl:< (cl:abs (cl:- a10 b10)) tolerance)
         (cl:< (cl:abs (cl:- a11 b11)) tolerance)
         (cl:< (cl:abs (cl:- a12 b12)) tolerance)
         (cl:< (cl:abs (cl:- a20 b20)) tolerance)
         (cl:< (cl:abs (cl:- a21 b21)) tolerance)
         (cl:< (cl:abs (cl:- a22 b22)) tolerance))))

(int:define-op random! ((out mat)
                        &key (min double-float 0d0) (max double-float 1d0))
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

(int:define-op random (&key (min double-float 0d0) (max double-float 1d0))
    (:out mat)
  (random! (mat) :min min :max max))

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
                       (min double-float most-negative-double-float)
                       (max double-float most-positive-double-float))
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
                      (min double-float most-negative-double-float)
                      (max double-float most-positive-double-float))
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

(int:define-op rotation-to-mat2! ((out dm2:mat) (in mat)) (:out dm2:mat)
  (dm2:with-components ((o out))
    (with-components ((m in))
      (psetf o00 m00 o01 m01
             o10 m10 o11 m11)))
  out)

(int:define-op rotation-to-mat2 ((in mat)) (:out dm2:mat)
  (rotation-to-mat2! (dm2:mat 1) in))

(int:define-op normalize-rotation! ((out mat) (in mat)) (:out mat)
  (with-components ((o out) (m in))
    (dv2::%normalize o00 o10 m00 m10)
    (dv2::%normalize o01 o11 m01 m11)
    (dv2::%normalize o02 o12 m02 m12))
  out)

(int:define-op normalize-rotation ((in mat)) (:out mat)
  (normalize-rotation! (copy in) in))

(int:define-op get-column! ((out dv3:vec) (in mat) (index (integer 0 2)))
    (:out dv3:vec)
  (with-components ((m in))
    (dv3:with-components ((o out))
      (ecase index
        (0 (psetf ox m00 oy m10 oz m20))
        (1 (psetf ox m01 oy m11 oz m21))
        (2 (psetf ox m02 oy m12 oz m22)))))
  out)

(int:define-op get-column ((in mat) (index (integer 0 2))) (:out dv3:vec)
  (get-column! (dv3:vec) in index))

(int:define-op set-column! ((out mat) (in mat) (vec dv3:vec)
                            (index (integer 0 2)))
    (:out mat)
  (with-components ((o out))
    (dv3:with-components ((v vec))
      (copy! out in)
      (ecase index
        (0 (psetf o00 vx o10 vy o20 vz))
        (1 (psetf o01 vx o11 vy o21 vz))
        (2 (psetf o02 vx o12 vy o22 vz)))))
  out)

(int:define-op set-column ((in mat) (vec dv3:vec) (index (integer 0 2)))
    (:out mat)
  (set-column! (mat 1) in vec index))

(int:define-op get-translation! ((out dv2:vec) (in mat)) (:out dv2:vec)
  (with-components ((m in))
    (dv2:with-components ((o out))
      (psetf ox m02 oy m12)))
  out)

(int:define-op get-translation ((in mat)) (:out dv2:vec)
  (get-translation! (dv2:vec) in))

(int:define-op set-translation! ((out mat) (in mat) (vec dv2:vec)) (:out mat)
  (with-components ((o out) (m in))
    (dv2:with-components ((v vec))
      (copy-rotation! out in)
      (psetf o02 vx o12 vy o22 m22)))
  out)

(int:define-op set-translation ((in mat) (vec dv2:vec)) (:out mat)
  (set-translation! (copy in) in vec))

(int:define-op translate! ((out mat) (in mat) (vec dv2:vec)) (:out mat)
  (with-components ((o out) (m in))
    (dv2:with-components ((v vec))
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

(int:define-op translate ((in mat) (vec dv2:vec)) (:out mat)
  (translate! (mat 1) in vec))

(int:define-op rotation-axis-to-vec2! ((out dv2:vec) (in mat) (axis keyword))
    (:out dv2:vec)
  (dv2:with-components ((v out))
    (with-components ((m in))
      (ecase axis
        (:x (psetf vx m00 vy m10))
        (:y (psetf vx m01 vy m11)))))
  out)

(int:define-op rotation-axis-to-vec2 ((in mat) (axis keyword)) (:out dv2:vec)
  (rotation-axis-to-vec2! (dv2:vec) in axis))

(int:define-op rotation-axis-from-vec2! ((in mat) (vec dv2:vec) (axis keyword))
    (:out mat)
  (with-components ((m in))
    (dv2:with-components ((v vec))
      (ecase axis
        (:x (psetf m00 vx m10 vy))
        (:y (psetf m01 vx m11 vy)))))
  in)

(int:define-op rotation-axis-from-vec2 ((in mat) (vec dv2:vec) (axis keyword))
    (:out mat)
  (rotation-axis-from-vec2! (copy in) vec axis))

(int:define-op rotate! ((out mat) (in mat) (angle double-float)
                        &key (space keyword :local))
    (:out mat)
  (dm2:with-elements ((m 1d0 0d0 0d0 1d0))
    (with-components ((o out))
      (let ((s (sin angle))
            (c (cos angle)))
        (copy! out in)
        (psetf m00 c m01 (cl:- s) m10 s m11 c)
        (ecase space
          (:local (dm2::%* o00 o01 o10 o11 o00 o01 o10 o11 m00 m01 m10 m11))
          (:world (dm2::%* o00 o01 o10 o11 m00 m01 m10 m11 o00 o01 010 011))))))
  out)

(int:define-op rotate ((in mat) (angle float)) (:out mat)
  (rotate! (mat 1) in angle))

(int:define-op get-scale! ((out dv2:vec) (in mat)) (:out dv2:vec)
  (dv2:with-components ((o out))
    (psetf ox (dv2:length (rotation-axis-to-vec2 in :x))
           oy (dv2:length (rotation-axis-to-vec2 in :y))))
  out)

(int:define-op get-scale ((in mat)) (:out dv2:vec)
  (get-scale! (dv2:vec) in))

(int:define-op set-scale! ((out mat) (in mat) (vec dv2:vec)) (:out mat)
  (with-components ((o out))
    (dv2:with-components ((v vec))
      (copy! out in)
      (psetf o00 vx o11 vy)))
  out)

(int:define-op set-scale ((in mat) (vec dv2:vec)) (:out mat)
  (set-scale! (copy in) in vec))

(int:define-op scale! ((out mat) (in mat) (vec dv2:vec)) (:out mat)
  (with-components ((o out) (m in))
    (dv2:with-components ((v vec))
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

(int:define-op scale ((in mat) (vec dv2:vec)) (:out mat)
  (scale! (mat 1) in vec))

(int:define-op *v3! ((out dv3:vec) (in mat) (vec dv3:vec)) (:out dv3:vec)
  (dv3:with-components ((v vec) (o out))
    (with-components ((m in))
      (psetf ox (cl:+ (cl:* m00 vx) (cl:* m01 vy) (cl:* m02 vz))
             oy (cl:+ (cl:* m10 vx) (cl:* m11 vy) (cl:* m12 vz))
             oz (cl:+ (cl:* m20 vx) (cl:* m21 vy) (cl:* m22 vz)))))
  out)

(int:define-op *v3 ((in mat) (vec dv3:vec)) (:out dv3:vec)
  (*v3! (dv3:vec) in vec))

(int:define-op transpose! ((out mat) (in mat)) (:out mat)
  (with-components ((o (copy! out in)))
    (rotatef o01 o10)
    (rotatef o02 o20)
    (rotatef o12 o21))
  out)

(int:define-op transpose ((in mat)) (:out mat)
  (transpose! (mat 1) in))

(int:define-op orthogonal-p ((in mat)) (:out boolean :inline nil)
  (~ (* in (transpose in)) +id+))

(defmacro %trace (m00 m11 m22)
  `(cl:+ ,m00 ,m11 ,m22))

(int:define-op trace ((in mat)) (:out double-float :speed nil)
  (with-components ((m in))
    (%trace m00 m11 m22)))

(int:define-op diagonal-p ((in mat)) (:out boolean)
  (with-components ((m in))
    (cl:= 0d0 m10 m20 m01 m21 m02 m12)))

(int:define-op main-diagonal! ((out dv3:vec) (in mat)) (:out dv3:vec)
  (with-components ((m in))
    (dv3:with-components ((v out))
      (psetf vx m00 vy m11 vz m22)))
  out)

(int:define-op main-diagonal ((in mat)) (:out dv3:vec)
  (main-diagonal! (dv3:vec) in))

(int:define-op anti-diagonal! ((out dv3:vec) (in mat)) (:out dv3:vec)
  (with-components ((m in))
    (dv3:with-components ((v out))
      (psetf vx m02 vy m11 vz m20)))
  out)

(int:define-op anti-diagonal ((in mat)) (:out dv3:vec)
  (anti-diagonal! (dv3:vec) in))
