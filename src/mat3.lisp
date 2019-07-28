(in-package #:cl-user)

(defpackage #:origin.mat3
  (:local-nicknames (#:a #:alexandria)
                    (#:v2 #:origin.vec2)
                    (#:v3 #:origin.vec3)
                    (#:m2 #:origin.mat2))
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
   #:normalize-rotation!
   #:normalize-rotation
   #:rotation-axis-to-vec2!
   #:rotation-axis-to-vec2
   #:rotation-axis-from-vec2!
   #:rotation-axis-from-vec2
   #:rotate!
   #:rotate
   #:get-scale!
   #:get-scale
   #:set-scale!
   #:set-scale
   #:scale!
   #:scale
   #:*v3!
   #:*v3
   #:transpose!
   #:transpose
   #:orthogonal-p
   #:trace
   #:diagonal-p
   #:main-diagonal!
   #:main-diagonal
   #:anti-diagonal!
   #:anti-diagonal))

(in-package #:origin.mat3)

(deftype mat () '(simple-array single-float (9)))

(defstruct (matrix (:type (vector single-float))
                   (:constructor %make (m00 m01 m02
                                        m10 m11 m12
                                        m20 m21 m22))
                   (:conc-name nil)
                   (:predicate nil)
                   (:copier nil))
  (m00 0f0 :type single-float)
  (m10 0f0 :type single-float)
  (m20 0f0 :type single-float)
  (m01 0f0 :type single-float)
  (m11 0f0 :type single-float)
  (m21 0f0 :type single-float)
  (m02 0f0 :type single-float)
  (m12 0f0 :type single-float)
  (m22 0f0 :type single-float))

(defmacro with-components (((prefix matrix) &rest rest) &body body)
  `(with-accessors ((,prefix identity)
                    (,(make-accessor-symbol prefix "00") m00)
                    (,(make-accessor-symbol prefix "01") m01)
                    (,(make-accessor-symbol prefix "02") m02)
                    (,(make-accessor-symbol prefix "10") m10)
                    (,(make-accessor-symbol prefix "11") m11)
                    (,(make-accessor-symbol prefix "12") m12)
                    (,(make-accessor-symbol prefix "20") m20)
                    (,(make-accessor-symbol prefix "21") m21)
                    (,(make-accessor-symbol prefix "22") m22))
       ,matrix
     ,(if rest
          `(with-components ,rest ,@body)
          `(progn ,@body))))

(defmacro with-elements (((prefix m00 m01 m02 m10 m11 m12 m20 m21 m22)
                          &rest rest)
                         &body body)
  (let ((%m00 (make-accessor-symbol prefix "00"))
        (%m01 (make-accessor-symbol prefix "01"))
        (%m02 (make-accessor-symbol prefix "02"))
        (%m10 (make-accessor-symbol prefix "10"))
        (%m11 (make-accessor-symbol prefix "11"))
        (%m12 (make-accessor-symbol prefix "12"))
        (%m20 (make-accessor-symbol prefix "20"))
        (%m21 (make-accessor-symbol prefix "21"))
        (%m22 (make-accessor-symbol prefix "22")))
    `(let ((,%m00 ,m00) (,%m01 ,m01) (,%m02 ,m02)
           (,%m10 ,m10) (,%m11 ,m11) (,%m12 ,m12)
           (,%m20 ,m20) (,%m21 ,m21) (,%m22 ,m22))
       (declare (ignorable ,%m00 ,%m01 ,%m02 ,%m10 ,%m11 ,%m12 ,%m20 ,%m21
                           ,%m22))
       ,(if rest
            `(with-elements ,rest ,@body)
            `(progn ,@body)))))

(a:define-constant +zero+
    (make-array 9 :element-type 'single-float
                  :initial-contents '(0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0))
  :test #'equalp)

(a:define-constant +id+
    (make-array 9 :element-type 'single-float
                  :initial-contents '(1f0 0f0 0f0 0f0 1f0 0f0 0f0 0f0 1f0))
  :test #'equalp)

(define-op make ((m00 real) (m01 real) (m02 real) (m10 real) (m11 real)
                 (m12 real) (m20 real) (m21 real) (m22 real))
    (:out mat)
  (%make (float m00 1f0) (float m01 1f0) (float m02 1f0)
         (float m10 1f0) (float m11 1f0) (float m12 1f0)
         (float m20 1f0) (float m21 1f0) (float m22 1f0)))

(define-op zero! ((in mat)) (:out mat)
  (with-components ((m in))
    (psetf m00 0f0 m01 0f0 m02 0f0
           m10 0f0 m11 0f0 m12 0f0
           m20 0f0 m21 0f0 m22 0f0))
  in)

(define-op zero () (:out mat)
  (%make 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0))

(define-op id! ((in mat)) (:out mat)
  (with-components ((m in))
    (psetf m00 1f0 m01 0f0 m02 0f0
           m10 0f0 m11 1f0 m12 0f0
           m20 0f0 m21 0f0 m22 1f0))
  in)

(define-op id () (:out mat)
  (%make 1f0 0f0 0f0 0f0 1f0 0f0 0f0 0f0 1f0))

(define-op id-p ((in mat)) (:out boolean)
  (with-components ((m in))
    (and (cl:= 0f0 m01 m02 m10 m12 m20 m21)
         (cl:= 1f0 m00 m11 m22))))

(define-op = ((in1 mat) (in2 mat)) (:out boolean)
  (with-components ((a in1) (b in2))
    (and (cl:= a00 b00) (cl:= a01 b01) (cl:= a02 b02)
         (cl:= a10 b10) (cl:= a11 b11) (cl:= a12 b12)
         (cl:= a20 b20) (cl:= a21 b21) (cl:= a22 b22))))

(define-op ~ ((in1 mat) (in2 mat) &key (tolerance single-float 1e-7))
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

(define-op random! ((out mat) &key (min real 0.0) (max real 1.0)) (:out mat)
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

(define-op random (&key (min real 0.0) (max real 1.0)) (:out mat)
  (random! (zero) :min min :max max))

(define-op copy! ((out mat) (in mat)) (:out mat)
  (with-components ((o out) (m in))
    (psetf o00 m00 o01 m01 o02 m02
           o10 m10 o11 m11 o12 m12
           o20 m20 o21 m21 o22 m22))
  out)

(define-op copy ((in mat)) (:out mat)
  (copy! (zero) in))

(define-op clamp! ((out mat) (in mat)
                   &key
                   (min single-float most-negative-single-float)
                   (max single-float most-positive-single-float))
    (:out mat :inline nil)
  (with-components ((o out) (m in))
    (psetf o00 (a:clamp m00 min max)
           o01 (a:clamp m01 min max)
           o02 (a:clamp m02 min max)
           o10 (a:clamp m10 min max)
           o11 (a:clamp m11 min max)
           o12 (a:clamp m12 min max)
           o20 (a:clamp m20 min max)
           o21 (a:clamp m21 min max)
           o22 (a:clamp m22 min max)))
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
           o01 (cl:+ a01 b01)
           o11 (cl:+ a11 b11)
           o21 (cl:+ a21 b21)
           o02 (cl:+ a02 b02)
           o12 (cl:+ a12 b12)
           o22 (cl:+ a22 b22)))
  out)

(define-op + ((in1 mat) (in2 mat)) (:out mat)
  (+! (zero) in1 in2))

(define-op -! ((out mat) (in1 mat) (in2 mat)) (:out mat)
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

(define-op - ((in1 mat) (in2 mat)) (:out mat)
  (-! (zero) in1 in2))

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

(define-op *! ((out mat) (in1 mat) (in2 mat)) (:out mat)
  (with-components ((o out) (a in1) (b in2))
    (%* o00 o01 o02 o10 o11 o12 o20 o21 o22
        a00 a01 a02 a10 a11 a12 a20 a21 a22
        b00 b01 b02 b10 b11 b12 b20 b21 b22))
  out)

(define-op * ((in1 mat) (in2 mat)) (:out mat)
  (*! (zero) in1 in2))

(define-op copy-rotation! ((out mat) (in mat)) (:out mat)
  (with-components ((o out) (m in))
    (psetf o00 m00 o01 m01
           o10 m10 o11 m11))
  out)

(define-op copy-rotation ((in mat)) (:out mat)
  (copy-rotation! (id) in))

(define-op normalize-rotation! ((out mat) (in mat)) (:out mat)
  (with-components ((o out) (m in))
    (v2::%normalize o00 o10 m00 m10)
    (v2::%normalize o01 o11 m01 m11)
    (v2::%normalize o02 o12 m02 m12))
  out)

(define-op normalize-rotation ((in mat)) (:out mat)
  (normalize-rotation! (copy in) in))

(define-op get-column! ((out v3:vec) (in mat) (index (integer 0 2)))
    (:out v3:vec)
  (with-components ((m in))
    (v3:with-components ((o out))
      (ecase index
        (0 (psetf ox m00 oy m10 oz m20))
        (1 (psetf ox m01 oy m11 oz m21))
        (2 (psetf ox m02 oy m12 oz m22)))))
  out)

(define-op get-column ((in mat) (index (integer 0 2))) (:out v3:vec)
  (get-column! (v3:zero) in index))

(define-op set-column! ((out mat) (in mat) (vec v3:vec) (index (integer 0 2)))
    (:out mat)
  (with-components ((o out))
    (v3:with-components ((v vec))
      (copy! out in)
      (ecase index
        (0 (psetf o00 vx o10 vy o20 vz))
        (1 (psetf o01 vx o11 vy o21 vz))
        (2 (psetf o02 vx o12 vy o22 vz)))))
  out)

(define-op set-column ((in mat) (vec v3:vec) (index (integer 0 2))) (:out mat)
  (set-column! (id) in vec index))

(define-op get-translation! ((out v2:vec) (in mat)) (:out v2:vec)
  (with-components ((m in))
    (v2:with-components ((o out))
      (psetf ox m02 oy m12)))
  out)

(define-op get-translation ((in mat)) (:out v2:vec)
  (get-translation! (v2:zero) in))

(define-op set-translation! ((out mat) (in mat) (vec v2:vec)) (:out mat)
  (with-components ((o out) (m in))
    (v2:with-components ((v vec))
      (copy-rotation! out in)
      (psetf o02 vx o12 vy o22 m22)))
  out)

(define-op set-translation ((in mat) (vec v2:vec)) (:out mat)
  (set-translation! (copy in) in vec))

(define-op translate! ((out mat) (in mat) (vec v2:vec)) (:out mat)
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

(define-op translate ((in mat) (vec v2:vec)) (:out mat)
  (translate! (id) in vec))

(define-op rotation-axis-to-vec2! ((out v2:vec) (in mat) (axis keyword))
    (:out v2:vec)
  (v2:with-components ((v out))
    (with-components ((m in))
      (ecase axis
        (:x (psetf vx m00 vy m10))
        (:y (psetf vx m01 vy m11)))))
  out)

(define-op rotation-axis-to-vec2 ((in mat) (axis keyword)) (:out v2:vec)
  (rotation-axis-to-vec2! (v2:zero) in axis))

(define-op rotation-axis-from-vec2! ((in mat) (vec v2:vec) (axis keyword))
    (:out mat)
  (with-components ((m in))
    (v2:with-components ((v vec))
      (ecase axis
        (:x (psetf m00 vx m10 vy))
        (:y (psetf m01 vx m11 vy)))))
  in)

(define-op rotation-axis-from-vec2 ((in mat) (vec v2:vec) (axis keyword))
    (:out mat)
  (rotation-axis-from-vec2! (copy in) vec axis))

(define-op rotate! ((out mat) (in mat) (angle float)
                    &key (space keyword :local))
    (:out mat)
  (m2:with-elements ((m 1f0 0f0 0f0 1f0))
    (with-components ((o out))
      (let* ((angle (float angle 1f0))
             (s (sin angle))
             (c (cos angle)))
        (copy! out in)
        (psetf m00 c m01 (cl:- s) m10 s m11 c)
        (ecase space
          (:local (m2::%* o00 o01 o10 o11 o00 o01 o10 o11 m00 m01 m10 m11))
          (:world (m2::%* o00 o01 o10 o11 m00 m01 m10 m11 o00 o01 010 011))))))
  out)

(define-op rotate ((in mat) (angle float)) (:out mat)
  (rotate! (id) in angle))

(define-op get-scale! ((out v2:vec) (in mat)) (:out v2:vec)
  (with-components ((m in))
    (v2:with-components ((o out))
      (psetf ox m00 oy m11)))
  out)

(define-op get-scale ((in mat)) (:out v2:vec)
  (get-scale! (v2:zero) in))

(define-op set-scale! ((out mat) (in mat) (vec v2:vec)) (:out mat)
  (with-components ((o out))
    (v2:with-components ((v vec))
      (copy! out in)
      (psetf o00 vx o11 vy)))
  out)

(define-op set-scale ((in mat) (vec v2:vec)) (:out mat)
  (set-scale! (copy in) in vec))

(define-op scale! ((out mat) (in mat) (vec v2:vec)) (:out mat)
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

(define-op scale ((in mat) (vec v2:vec)) (:out mat)
  (scale! (id) in vec))

(define-op *v3! ((out v3:vec) (in mat) (vec v3:vec)) (:out v3:vec)
  (v3:with-components ((v vec) (o out))
    (with-components ((m in))
      (psetf ox (cl:+ (cl:* m00 vx) (cl:* m01 vy) (cl:* m02 vz))
             oy (cl:+ (cl:* m10 vx) (cl:* m11 vy) (cl:* m12 vz))
             oz (cl:+ (cl:* m20 vx) (cl:* m21 vy) (cl:* m22 vz)))))
  out)

(define-op *v3 ((in mat) (vec v3:vec)) (:out v3:vec)
  (*v3! (v3:zero) in vec))

(define-op transpose! ((out mat) (in mat)) (:out mat)
  (with-components ((o (copy! out in)))
    (rotatef o01 o10)
    (rotatef o02 o20)
    (rotatef o12 o21))
  out)

(define-op transpose ((in mat)) (:out mat)
  (transpose! (id) in))

(define-op orthogonal-p ((in mat)) (:out boolean :inline nil)
  (~ (* in (transpose in)) +id+))

(defmacro %trace (m00 m11 m22)
  `(cl:+ ,m00 ,m11 ,m22))

(define-op trace ((in mat)) (:out single-float)
  (with-components ((m in))
    (%trace m00 m11 m22)))

(define-op diagonal-p ((in mat)) (:out boolean)
  (with-components ((m in))
    (and (zerop m10)
         (zerop m20)
         (zerop m01)
         (zerop m21)
         (zerop m02)
         (zerop m12))))

(define-op main-diagonal! ((out v3:vec) (in mat)) (:out v3:vec)
  (with-components ((m in))
    (v3:with-components ((v out))
      (psetf vx m00 vy m11 vz m22)))
  out)

(define-op main-diagonal ((in mat)) (:out v3:vec)
  (main-diagonal! (v3:zero) in))

(define-op anti-diagonal! ((out v3:vec) (in mat)) (:out v3:vec)
  (with-components ((m in))
    (v3:with-components ((v out))
      (psetf vx m02 vy m11 vz m20)))
  out)

(define-op anti-diagonal ((in mat)) (:out v3:vec)
  (anti-diagonal! (v3:zero) in))
