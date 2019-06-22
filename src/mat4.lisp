(in-package #:cl-user)

(defpackage #:origin.mat4
  (:local-nicknames (#:v3 #:origin.vec3)
                    (#:v4 #:origin.vec4)
                    (#:m3 #:origin.mat3))
  (:use #:cl #:origin.internal)
  (:shadow
   #:=
   #:+
   #:-
   #:*
   #:random
   #:trace)
  (:export
   #:mat
   #:with-components
   #:with-elements
   #:+zero+
   #:+id+
   #:make
   #:zero!
   #:zero
   #:id!
   #:id
   #:id-p
   #:=
   #:~
   #:random!
   #:random
   #:copy!
   #:copy
   #:clamp!
   #:clamp
   #:+!
   #:+
   #:-!
   #:-
   #:*!
   #:*
   #:get-column!
   #:get-column
   #:set-column!
   #:set-column
   #:get-translation!
   #:get-translation
   #:set-translation!
   #:set-translation
   #:translate!
   #:translate
   #:copy-rotation!
   #:copy-rotation
   #:rotation-axis-to-vec3!
   #:rotation-axis-to-vec3
   #:rotation-axis-from-vec3!
   #:rotation-axis-from-vec3
   #:rotate!
   #:rotate
   #:get-scale!
   #:get-scale
   #:set-scale!
   #:set-scale
   #:scale!
   #:scale
   #:*v4!
   #:*v4
   #:transpose!
   #:transpose
   #:orthogonal-p
   #:orthonormalize!
   #:orthonormalize
   #:trace
   #:diagonal-p
   #:main-diagonal!
   #:main-diagonal
   #:anti-diagonal!
   #:anti-diagonal
   #:determinant
   #:invert-orthogonal!
   #:invert-orthogonal
   #:invert!
   #:invert
   #:set-view!
   #:set-view
   #:set-projection/orthographic!
   #:set-projection/orthographic
   #:set-projection/perspective!
   #:set-projection/perspective))

(in-package #:origin.mat4)

(deftype mat () '(simple-array single-float (16)))

(defstruct (matrix (:type (vector single-float))
                   (:constructor %make (m00 m01 m02 m03
                                        m10 m11 m12 m13
                                        m20 m21 m22 m23
                                        m30 m31 m32 m33))
                   (:conc-name nil)
                   (:predicate nil)
                   (:copier nil))
  (m00 0f0 :type single-float)
  (m10 0f0 :type single-float)
  (m20 0f0 :type single-float)
  (m30 0f0 :type single-float)
  (m01 0f0 :type single-float)
  (m11 0f0 :type single-float)
  (m21 0f0 :type single-float)
  (m31 0f0 :type single-float)
  (m02 0f0 :type single-float)
  (m12 0f0 :type single-float)
  (m22 0f0 :type single-float)
  (m32 0f0 :type single-float)
  (m03 0f0 :type single-float)
  (m13 0f0 :type single-float)
  (m23 0f0 :type single-float)
  (m33 0f0 :type single-float))

(defmacro with-components (((prefix matrix) &rest rest) &body body)
  `(with-accessors ((,prefix identity)
                    (,(make-accessor-symbol prefix "00") m00)
                    (,(make-accessor-symbol prefix "01") m01)
                    (,(make-accessor-symbol prefix "02") m02)
                    (,(make-accessor-symbol prefix "03") m03)
                    (,(make-accessor-symbol prefix "10") m10)
                    (,(make-accessor-symbol prefix "11") m11)
                    (,(make-accessor-symbol prefix "12") m12)
                    (,(make-accessor-symbol prefix "13") m13)
                    (,(make-accessor-symbol prefix "20") m20)
                    (,(make-accessor-symbol prefix "21") m21)
                    (,(make-accessor-symbol prefix "22") m22)
                    (,(make-accessor-symbol prefix "23") m23)
                    (,(make-accessor-symbol prefix "30") m30)
                    (,(make-accessor-symbol prefix "31") m31)
                    (,(make-accessor-symbol prefix "32") m32)
                    (,(make-accessor-symbol prefix "33") m33))
       ,matrix
     ,(if rest
          `(with-components ,rest ,@body)
          `(progn ,@body))))

(defmacro with-elements (((prefix m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22
                           m23 m30 m31 m32 m33)
                          &rest rest)
                         &body body)
  (let ((%m00 (make-accessor-symbol prefix "00"))
        (%m01 (make-accessor-symbol prefix "01"))
        (%m02 (make-accessor-symbol prefix "02"))
        (%m03 (make-accessor-symbol prefix "03"))
        (%m10 (make-accessor-symbol prefix "10"))
        (%m11 (make-accessor-symbol prefix "11"))
        (%m12 (make-accessor-symbol prefix "12"))
        (%m13 (make-accessor-symbol prefix "13"))
        (%m20 (make-accessor-symbol prefix "20"))
        (%m21 (make-accessor-symbol prefix "21"))
        (%m22 (make-accessor-symbol prefix "22"))
        (%m23 (make-accessor-symbol prefix "23"))
        (%m30 (make-accessor-symbol prefix "30"))
        (%m31 (make-accessor-symbol prefix "31"))
        (%m32 (make-accessor-symbol prefix "32"))
        (%m33 (make-accessor-symbol prefix "33")))
    `(let ((,%m00 ,m00) (,%m01 ,m01) (,%m02 ,m02) (,%m03 ,m03)
           (,%m10 ,m10) (,%m11 ,m11) (,%m12 ,m12) (,%m13 ,m13)
           (,%m20 ,m20) (,%m21 ,m21) (,%m22 ,m22) (,%m23 ,m23)
           (,%m30 ,m30) (,%m31 ,m31) (,%m32 ,m32) (,%m33 ,m33))
       (declare (ignorable ,%m00 ,%m01 ,%m02 ,%m03 ,%m10 ,%m11 ,%m12 ,%m13
                           ,%m20 ,%m21 ,%m22 ,%m23 ,%m30 ,%m31 ,%m32 ,%m33))
       ,(if rest
            `(with-elements ,rest ,@body)
            `(progn ,@body)))))

(au:define-constant +zero+
    (make-array 16 :element-type 'single-float
                   :initial-contents '(0f0 0f0 0f0 0f0
                                       0f0 0f0 0f0 0f0
                                       0f0 0f0 0f0 0f0
                                       0f0 0f0 0f0 0f0))
  :test #'equalp)

(au:define-constant +id+
    (make-array 16 :element-type 'single-float
                   :initial-contents '(1f0 0f0 0f0 0f0
                                       0f0 1f0 0f0 0f0
                                       0f0 0f0 1f0 0f0
                                       0f0 0f0 0f0 1f0))
  :test #'equalp)

(define-op make ((m00 real) (m01 real) (m02 real) (m03 real) (m10 real)
                 (m11 real) (m12 real) (m13 real) (m20 real)
                 (m21 real) (m22 real) (m23 real) (m30 real)
                 (m31 real) (m32 real) (m33 real))
    (:out mat)
  (%make (float m00 1f0) (float m01 1f0) (float m02 1f0) (float m03 1f0)
         (float m10 1f0) (float m11 1f0) (float m12 1f0) (float m13 1f0)
         (float m20 1f0) (float m21 1f0) (float m22 1f0) (float m23 1f0)
         (float m30 1f0) (float m31 1f0) (float m32 1f0)
         (float m33 1f0)))

(define-op zero! ((in mat)) (:out mat)
  (with-components ((m in))
    (psetf m00 0f0 m01 0f0 m02 0f0 m03 0f0
           m10 0f0 m11 0f0 m12 0f0 m13 0f0
           m20 0f0 m21 0f0 m22 0f0 m23 0f0
           m30 0f0 m31 0f0 m32 0f0 m33 0f0))
  in)

(define-op zero () (:out mat)
  (%make 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0))

(define-op id! ((in mat)) (:out mat)
  (with-components ((m in))
    (psetf m00 1f0 m01 0f0 m02 0f0 m03 0f0
           m10 0f0 m11 1f0 m12 0f0 m13 0f0
           m20 0f0 m21 0f0 m22 1f0 m23 0f0
           m30 0f0 m31 0f0 m32 0f0 m33 1f0))
  in)

(define-op id () (:out mat)
  (%make 1f0 0f0 0f0 0f0 0f0 1f0 0f0 0f0 0f0 0f0 1f0 0f0 0f0 0f0 0f0 1f0))

(define-op id-p ((in mat)) (:out boolean)
  (with-components ((m in))
    (and (cl:= 0f0 m01 m02 m03 m10 m12 m13 m20 m21 m23 m30 m31 m32)
         (cl:= 1f0 m00 m11 m22 m33))))

(define-op = ((in1 mat) (in2 mat)) (:out boolean)
  (with-components ((a in1) (b in2))
    (and (cl:= a00 b00) (cl:= a01 b01) (cl:= a02 b02) (cl:= a03 b03)
         (cl:= a10 b10) (cl:= a11 b11) (cl:= a12 b12) (cl:= a13 b13)
         (cl:= a20 b20) (cl:= a21 b21) (cl:= a22 b22) (cl:= a23 b23)
         (cl:= a30 b30) (cl:= a31 b31) (cl:= a32 b32) (cl:= a33 b33))))

(define-op ~ ((in1 mat) (in2 mat) &key (tolerance single-float 1e-7))
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

(define-op random! ((out mat) &key (min real 0.0) (max real 1.0)) (:out mat)
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

(define-op random (&key (min real 0.0) (max real 1.0)) (:out mat)
  (random! (zero) :min min :max max))

(define-op copy! ((out mat) (in mat)) (:out mat)
  (with-components ((o out) (m in))
    (psetf o00 m00 o01 m01 o02 m02 o03 m03
           o10 m10 o11 m11 o12 m12 o13 m13
           o20 m20 o21 m21 o22 m22 o23 m23
           o30 m30 o31 m31 o32 m32 o33 m33))
  out)

(define-op copy ((in mat)) (:out mat)
  (copy! (zero) in))

(define-op clamp! ((out mat) (in mat)
                   &key
                   (min single-float most-negative-single-float)
                   (max single-float most-positive-single-float))
    (:out mat :inline t)
  (with-components ((o out) (m in))
    (psetf o00 (au:clamp m00 min max)
           o01 (au:clamp m01 min max)
           o02 (au:clamp m02 min max)
           o03 (au:clamp m03 min max)
           o10 (au:clamp m10 min max)
           o11 (au:clamp m11 min max)
           o12 (au:clamp m12 min max)
           o13 (au:clamp m13 min max)
           o20 (au:clamp m20 min max)
           o21 (au:clamp m21 min max)
           o22 (au:clamp m22 min max)
           o23 (au:clamp m23 min max)
           o30 (au:clamp m30 min max)
           o31 (au:clamp m31 min max)
           o32 (au:clamp m32 min max)
           o33 (au:clamp m33 min max)))
  out)

(define-op clamp ((in mat)
                  &key
                  (min single-float most-negative-single-float)
                  (max single-float most-positive-single-float))
    (:out mat)
  (clamp! (zero) in :min min :max max))

(define-op +! ((out mat) (in1 mat) (in2 mat)) (:out mat)
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

(define-op + ((in1 mat) (in2 mat)) (:out mat)
  (+! (zero) in1 in2))

(define-op -! ((out mat) (in1 mat) (in2 mat)) (:out mat)
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

(define-op - ((in1 mat) (in2 mat)) (:out mat)
  (-! (zero) in1 in2))

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

(define-op *! ((out mat) (in1 mat) (in2 mat)) (:out mat)
  (with-components ((o out) (a in1) (b in2))
    (%* o00 o01 o02 o03 o10 o11 o12 o13 o20 o21 o22 o23 o30 o31 o32 o33
        a00 a01 a02 a03 a10 a11 a12 a13 a20 a21 a22 a23 a30 a31 a32 a33
        b00 b01 b02 b03 b10 b11 b12 b13 b20 b21 b22 b23 b30 b31 b32 b33))
  out)

(define-op * ((in1 mat) (in2 mat)) (:out mat)
  (*! (zero) in1 in2))

(define-op copy-rotation! ((out mat) (in mat)) (:out mat)
  (with-components ((o out) (m in))
    (psetf o00 m00 o01 m01 o02 m02
           o10 m10 o11 m11 o12 m12
           o20 m20 o21 m21 o22 m22))
  out)

(define-op copy-rotation ((in mat)) (:out mat)
  (copy-rotation! (id) in))

(define-op get-column! ((out v4:vec) (in mat) (index (integer 0 3)))
    (:out v4:vec)
  (with-components ((m in))
    (v4:with-components ((o out))
      (ecase index
        (0 (psetf ox m00 oy m10 oz m20 ow m30))
        (1 (psetf ox m01 oy m11 oz m21 ow m31))
        (2 (psetf ox m02 oy m12 oz m22 ow m32))
        (3 (psetf ox m03 oy m13 oz m23 ow m33)))))
  out)

(define-op get-column ((in mat) (index (integer 0 3))) (:out v4:vec)
  (get-column! (v4:zero) in index))

(define-op set-column! ((out mat) (in mat) (vec v4:vec) (index (integer 0 3)))
    (:out mat)
  (with-components ((o out))
    (v4:with-components ((v vec))
      (copy! out in)
      (ecase index
        (0 (psetf o00 vx o10 vy o20 vz o30 vw))
        (1 (psetf o01 vx o11 vy o21 vz o31 vw))
        (2 (psetf o02 vx o12 vy o22 vz o32 vw))
        (3 (psetf o03 vx o13 vy o23 vz o33 vw)))))
  out)

(define-op set-column ((in mat) (vec v4:vec) (index (integer 0 3))) (:out mat)
  (set-column! (id) in vec index))

(define-op get-translation! ((out v3:vec) (in mat)) (:out v3:vec)
  (with-components ((m in))
    (v3:with-components ((o out))
      (psetf ox m03 oy m13 oz m23)))
  out)

(define-op get-translation ((in mat)) (:out v3:vec)
  (get-translation! (v3:zero) in))

(define-op set-translation! ((out mat) (in mat) (vec v3:vec)) (:out mat)
  (with-components ((o out) (m in))
    (v3:with-components ((v vec))
      (copy-rotation! out in)
      (psetf o03 vx o13 vy o23 vz o33 m33)))
  out)

(define-op set-translation ((in mat) (vec v3:vec)) (:out mat)
  (set-translation! (copy in) in vec))

(define-op translate! ((out mat) (in mat) (vec v3:vec)) (:out mat)
  (with-components ((o out) (m in))
    (v3:with-components ((v vec))
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

(define-op translate ((in mat) (vec v3:vec)) (:out mat)
  (translate! (id) in vec))

(define-op rotation-axis-to-vec3! ((out v3:vec) (in mat) (axis keyword))
    (:out v3:vec)
  (v3:with-components ((v out))
    (with-components ((m in))
      (ecase axis
        (:x (psetf vx m00 vy m10 vz m20))
        (:y (psetf vx m01 vy m11 vz m21))
        (:z (psetf vx m02 vy m12 vz m22)))))
  out)

(define-op rotation-axis-to-vec3 ((in mat) (axis keyword)) (:out v3:vec)
  (rotation-axis-to-vec3! (v3:zero) in axis))

(define-op rotation-axis-from-vec3! ((in mat) (vec v3:vec) (axis keyword))
    (:out mat)
  (with-components ((m in))
    (v3:with-components ((v vec))
      (ecase axis
        (:x (psetf m00 vx m10 vy m20 vz))
        (:y (psetf m01 vx m11 vy m21 vz))
        (:z (psetf m02 vx m12 vy m22 vz)))))
  in)

(define-op rotation-axis-from-vec3 ((in mat) (vec v3:vec) (axis keyword))
    (:out mat)
  (rotation-axis-from-vec3! (copy in) vec axis))

(define-op rotate! ((out mat) (in mat) (vec v3:vec) &key (space keyword :local))
    (:out mat :inline nil)
  (macrolet ((rotate-angle (angle s c &body body)
               `(let ((,s (sin ,angle))
                      (,c (cos ,angle)))
                  ,@body
                  (ecase space
                    (:local (m3::%* o00 o01 o02 o10 o11 o12 o20 o21 o22
                                    o00 o01 o02 o10 o11 o12 o20 o21 o22
                                    m00 m01 m02 m10 m11 m12 m20 m21 m22))
                    (:world (m3::%* o00 o01 o02 o10 o11 o12 o20 o21 o22
                                    m00 m01 m02 m10 m11 m12 m20 m21 m22
                                    o00 o01 o02 o10 o11 o12 o20 o21 o22))))))
    (m3:with-elements ((m 1f0 0f0 0f0 0f0 1f0 0f0 0f0 0f0 1f0))
      (with-components ((o out))
        (v3:with-components ((v vec))
          (copy! out in)
          (rotate-angle vz s c
                        (psetf m00 c m01 (cl:- s) m10 s m11 c))
          (rotate-angle vx s c
                        (psetf m00 1f0 m01 0f0 m02 0f0
                               m10 0f0 m11 c m12 (cl:- s)
                               m20 0f0 m21 s m22 c))
          (rotate-angle vy s c
                        (psetf m00 c m01 0f0 m02 s
                               m10 0f0 m11 1f0 m12 0f0
                               m20 (cl:- s) m21 0f0 m22 c))))))
  out)

(define-op rotate ((in mat) (vec v3:vec)) (:out mat)
  (rotate! (id) in vec))

(define-op get-scale! ((out v3:vec) (in mat)) (:out v3:vec)
  (with-components ((m in))
    (v3:with-components ((o out))
      (psetf ox m00 oy m11 oz m22)))
  out)

(define-op get-scale ((in mat)) (:out v3:vec)
  (get-scale! (v3:zero) in))

(define-op set-scale! ((out mat) (in mat) (vec v3:vec)) (:out mat)
  (with-components ((o out))
    (v3:with-components ((v vec))
      (copy! out in)
      (psetf o00 vx o11 vy o22 vz)))
  out)

(define-op set-scale ((in mat) (vec v3:vec)) (:out mat)
  (set-scale! (copy in) in vec))

(define-op scale! ((out mat) (in mat) (vec v3:vec)) (:out mat)
  (with-components ((o out) (m in))
    (v3:with-components ((v vec))
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

(define-op scale ((in mat) (vec v3:vec)) (:out mat)
  (scale! (id) in vec))

(define-op *v4! ((out v4:vec) (in mat) (vec v4:vec)) (:out v4:vec)
  (v4:with-components ((v vec) (o out))
    (with-components ((m in))
      (psetf ox (cl:+ (cl:* m00 vx) (cl:* m01 vy) (cl:* m02 vz)
                      (cl:* m03 vw))
             oy (cl:+ (cl:* m10 vx) (cl:* m11 vy) (cl:* m12 vz)
                      (cl:* m13 vw))
             oz (cl:+ (cl:* m20 vx) (cl:* m21 vy) (cl:* m22 vz)
                      (cl:* m23 vw))
             ow (cl:+ (cl:* m30 vx) (cl:* m31 vy) (cl:* m32 vz)
                      (cl:* m33 vw)))))
  out)

(define-op *v4 ((in mat) (vec v4:vec)) (:out v4:vec)
  (*v4! (v4:zero) in vec))

(define-op transpose! ((out mat) (in mat)) (:out mat)
  (with-components ((o (copy! out in)))
    (rotatef o01 o10)
    (rotatef o02 o20)
    (rotatef o03 o30)
    (rotatef o12 o21)
    (rotatef o13 o31)
    (rotatef o23 o32))
  out)

(define-op transpose ((in mat)) (:out mat)
  (transpose! (id) in))

(define-op orthogonal-p ((in mat)) (:out boolean :inline nil)
  (~ (* in (transpose in)) +id+))

(define-op orthonormalize! ((out mat) (in mat)) (:out mat :inline nil)
  (let* ((x (rotation-axis-to-vec3 in :x))
         (y (rotation-axis-to-vec3 in :y))
         (z (rotation-axis-to-vec3 in :z)))
    (v3:normalize! x x)
    (v3:normalize! y (v3:- y (v3:scale x (v3:dot y x))))
    (v3:cross! z x y)
    (rotation-axis-from-vec3! out x :x)
    (rotation-axis-from-vec3! out y :y)
    (rotation-axis-from-vec3! out z :z))
  out)

(define-op orthonormalize ((in mat)) (:out mat)
  (orthonormalize! (id) in))

(defmacro %trace (m00 m11 m22 m33)
  `(cl:+ ,m00 ,m11 ,m22 ,m33))

(define-op trace ((in mat)) (:out single-float)
  (with-components ((m in))
    (%trace m00 m11 m22 m33)))

(define-op diagonal-p ((in mat)) (:out boolean)
  (with-components ((m in))
    (and (zerop m10)
         (zerop m20)
         (zerop m30)
         (zerop m01)
         (zerop m21)
         (zerop m31)
         (zerop m02)
         (zerop m12)
         (zerop m32)
         (zerop m03)
         (zerop m13)
         (zerop m23))))

(define-op main-diagonal! ((out v4:vec) (in mat)) (:out v4:vec)
  (with-components ((m in))
    (v4:with-components ((v out))
      (psetf vx m00 vy m11 vz m22 vw m33)))
  out)

(define-op main-diagonal ((in mat)) (:out v4:vec)
  (main-diagonal! (v4:zero) in))

(define-op anti-diagonal! ((out v4:vec) (in mat)) (:out v4:vec)
  (with-components ((m in))
    (v4:with-components ((v out))
      (psetf vx m03 vy m12 vz m21 vw m30)))
  out)

(define-op anti-diagonal ((in mat)) (:out v4:vec)
  (anti-diagonal! (v4:zero) in))

(define-op determinant ((in mat)) (:out single-float)
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

(define-op invert-orthogonal! ((out mat) (in mat)) (:out mat)
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

(define-op invert-orthogonal ((in mat)) (:out mat)
  (invert-orthogonal! (id) in))

(define-op invert! ((out mat) (in mat)) (:out mat :inline nil)
  (let ((determinant (determinant in)))
    (when (< (abs determinant) 1e-7)
      (error "Cannot invert a matrix with a determinant of zero."))
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
  out)

(define-op invert ((in mat)) (:out mat)
  (invert! (id) in))

(define-op set-view! ((out mat) (eye v3:vec) (target v3:vec) (up v3:vec))
    (:out mat :inline nil)
  (with-components ((o (id! out)))
    (v3:with-components ((e eye) (s target) (u up))
      (v3:with-elements ((a (cl:- sx ex) (cl:- sy ey) (cl:- sz ez)))
        (v3::%normalize ax ay az ax ay az)
        (psetf o20 ax o21 ay o22 az)
        (v3:with-elements ((b (cl:- (cl:* o21 uz) (cl:* o22 uy))
                              (cl:- (cl:* o22 ux) (cl:* o20 uz))
                              (cl:- (cl:* o20 uy) (cl:* o21 ux))))
          (v3::%normalize bx by bz bx by bz)
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

(define-op set-view ((eye v3:vec) (target v3:vec) (up v3:vec)) (:out mat)
  (set-view! (id) eye target up))

(define-op set-projection/orthographic! ((out mat) (left real) (right real)
                                         (bottom real) (top real) (near real)
                                         (far real))
    (:out mat :inline nil)
  (let ((right-left (float (cl:- right left) 1f0))
        (top-bottom (float (cl:- top bottom) 1f0))
        (far-near (float (cl:- far near) 1f0)))
    (with-components ((m (id! out)))
      (psetf m00 (/ 2f0 right-left)
             m03 (cl:- (/ (cl:+ right left) right-left))
             m11 (/ 2f0 top-bottom)
             m13 (cl:- (/ (cl:+ top bottom) top-bottom))
             m22 (/ -2f0 far-near)
             m23 (cl:- (/ (cl:+ far near) far-near))))
    out))

(define-op set-projection/orthographic ((left real) (right real) (bottom real)
                                        (top real) (near real) (far real))
    (:out mat)
  (set-projection/orthographic! (id) left right bottom top near far))

(define-op set-projection/perspective! ((out mat) (fov real) (aspect real)
                                        (near real) (far real))
    (:out mat)
  (let ((f (float (/ (tan (/ fov 2))) 1f0))
        (z (float (cl:- near far) 1f0)))
    (with-components ((m (zero! out)))
      (psetf m00 (/ f aspect)
             m11 f
             m22 (/ (cl:+ near far) z)
             m23 (/ (cl:* 2 near far) z)
             m32 -1f0)))
  out)

(define-op set-projection/perspective ((fov real) (aspect real) (near real)
                                       (far real))
    (:out mat)
  (set-projection/perspective! (id) fov aspect near far))
