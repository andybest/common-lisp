(in-package #:net.mfiano.lisp.origin.dmat2)

;;; constructors

(int:define-op %mat (&rest (args double-float)) (:inline t :out mat)
  (make-array 4 :element-type 'double-float :initial-contents args))

(ss:defstore mat (&rest args))

(ss:defspecialization (mat :inline t) () mat
  (%mat 0d0 0d0 0d0 0d0))

(ss:defspecialization (mat :inline t) ((x real)) mat
  (%mat (float x 1d0) 0d0 0d0 (float x 1d0)))

(ss:defspecialization (mat :inline t) ((mat mat)) mat
  (with-components ((m mat))
    (%mat m00 m01 m10 m11)))

(ss:defspecialization (mat :inline t) ((mat net.mfiano.lisp.origin.dmat3:mat))
    mat
  (net.mfiano.lisp.origin.dmat3:with-components ((m mat))
    (%mat m00 m01 m10 m11)))

(ss:defspecialization (mat :inline t) ((mat net.mfiano.lisp.origin.dmat4:mat))
    mat
  (net.mfiano.lisp.origin.dmat4:with-components ((m mat))
    (%mat m00 m01 m10 m11)))

(ss:defspecialization (mat :inline t) ((a dv2:vec) (b dv2:vec)) mat
  (dv2:with-components ((a a) (b b))
    (%mat ax ay bx by)))

(ss:defspecialization (mat :inline t) ((a real) (b real) (c real) (d real)) mat
  (%mat (float a 1d0) (float b 1d0) (float c 1d0) (float d 1d0)))

(ss:defspecialization (mat :inline t) ((mat m2:mat)) mat
  (m2:with-components ((m mat))
    (%mat (float m00 1d0) (float m10 1d0) (float m01 1d0) (float m11 1d0))))

;;; constants

(u:define-constant +zero+ (%mat 0d0 0d0 0d0 0d0) :test #'equalp)

(u:define-constant +id+ (%mat 1d0 0d0 0d0 1d0) :test #'equalp)

;;; operators

(int:define-op zero! ((in mat)) (:out mat)
  (with-components ((m in))
    (psetf m00 0d0 m01 0d0 m10 0d0 m11 0d0))
  in)

(int:define-op zero-p ((in mat)) (:out boolean)
  (with-components ((m in))
    (cl:= 0d0 m00 m01 m10 m11)))

(int:define-op id! ((in mat)) (:out mat)
  (with-components ((m in))
    (psetf m00 1d0 m01 0d0 m10 0d0 m11 1d0))
  in)

(int:define-op id-p ((in mat)) (:out boolean)
  (with-components ((m in))
    (and (cl:= 0d0 m10 m01)
         (cl:= 1d0 m00 m11))))

(int:define-op = ((in1 mat) (in2 mat)) (:out boolean)
  (with-components ((a in1) (b in2))
    (and (cl:= a00 b00) (cl:= a01 b01)
         (cl:= a10 b10) (cl:= a11 b11))))

(int:define-op ~ ((in1 mat) (in2 mat) &key (tolerance double-float 1d-7))
    (:out boolean)
  (with-components ((a in1) (b in2))
    (and (cl:< (cl:abs (cl:- a00 b00)) tolerance)
         (cl:< (cl:abs (cl:- a01 b01)) tolerance)
         (cl:< (cl:abs (cl:- a10 b10)) tolerance)
         (cl:< (cl:abs (cl:- a11 b11)) tolerance))))

(int:define-op random! ((out mat)
                        &key (min double-float 0d0) (max double-float 1d0))
    (:out mat)
  (with-components ((o out))
    (psetf o00 (cl:+ min (cl:random (cl:- max min)))
           o01 (cl:+ min (cl:random (cl:- max min)))
           o10 (cl:+ min (cl:random (cl:- max min)))
           o11 (cl:+ min (cl:random (cl:- max min)))))
  out)

(int:define-op random (&key (min double-float 0d0) (max double-float 1d0))
    (:out mat)
  (random! (mat) :min min :max max))

(int:define-op copy! ((out mat) (in mat)) (:out mat)
  (with-components ((o out) (m in))
    (psetf o00 m00 o01 m01 o10 m10 o11 m11))
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
           o10 (u:clamp m10 min max)
           o11 (u:clamp m11 min max)))
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
           o01 (cl:+ a01 b01)
           o11 (cl:+ a11 b11)))
  out)

(int:define-op + ((in1 mat) (in2 mat)) (:out mat)
  (+! (mat) in1 in2))

(int:define-op -! ((out mat) (in1 mat) (in2 mat)) (:out mat)
  (with-components ((o out) (a in1) (b in2))
    (psetf o00 (cl:- a00 b00)
           o10 (cl:- a10 b10)
           o01 (cl:- a01 b01)
           o11 (cl:- a11 b11)))
  out)

(int:define-op - ((in1 mat) (in2 mat)) (:out mat)
  (-! (mat) in1 in2))

(defmacro %* (o00 o01 o10 o11 a00 a01 a10 a11 b00 b01 b10 b11)
  `(psetf ,o00 (cl:+ (cl:* ,a00 ,b00) (cl:* ,a01 ,b10))
          ,o10 (cl:+ (cl:* ,a10 ,b00) (cl:* ,a11 ,b10))
          ,o01 (cl:+ (cl:* ,a00 ,b01) (cl:* ,a01 ,b11))
          ,o11 (cl:+ (cl:* ,a10 ,b01) (cl:* ,a11 ,b11))))

(int:define-op *! ((out mat) (in1 mat) (in2 mat)) (:out mat)
  (with-components ((o out) (a in1) (b in2))
    (%* o00 o01 o10 o11 a00 a01 a10 a11 b00 b01 b10 b11))
  out)

(int:define-op * ((in1 mat) (in2 mat)) (:out mat)
  (*! (mat) in1 in2))

(int:define-op get-column! ((out dv2:vec) (in mat) (index (integer 0 1)))
    (:out dv2:vec)
  (with-components ((m in))
    (dv2:with-components ((o out))
      (ecase index
        (0 (psetf ox m00 oy m10))
        (1 (psetf ox m01 oy m11)))))
  out)

(int:define-op get-column ((in mat) (index (integer 0 1))) (:out dv2:vec)
  (get-column! (dv2:vec) in index))

(int:define-op set-column! ((out mat) (in mat) (vec dv2:vec)
                            (index (integer 0 1)))
    (:out mat)
  (with-components ((o out))
    (dv2:with-components ((v vec))
      (copy! out in)
      (ecase index
        (0 (psetf o00 vx o10 vy))
        (1 (psetf o01 vx o11 vy)))))
  out)

(int:define-op set-column ((in mat) (vec dv2:vec) (index (integer 0 1)))
    (:out mat)
  (set-column! (mat 1) in vec index))

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
    (:out mat :inline nil)
  (with-components ((m (mat 1)))
    (copy! out in)
    (when (cl:> (abs angle) 1d-7)
      (let ((s (sin angle))
            (c (cos angle)))
        (psetf m00 c m01 (cl:- s)
               m10 s m11 c)
        (ecase space
          (:local (*! out out m))
          (:world (*! out m out))))))
  out)

(int:define-op rotate ((in mat) (angle float)) (:out mat)
  (rotate! (mat 1) in angle))

(int:define-op get-scale! ((out dv2:vec) (in mat)) (:out dv2:vec)
  (with-components ((m in))
    (dv2:with-components ((o out))
      (psetf ox m00 oy m11)))
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
  (*! out (set-scale (mat 1) vec) in))

(int:define-op scale ((in mat) (vec dv2:vec)) (:out mat)
  (scale! (mat 1) in vec))

(int:define-op *v2! ((out dv2:vec) (in mat) (vec dv2:vec)) (:out dv2:vec)
  (dv2:with-components ((v vec) (o out))
    (with-components ((m in))
      (psetf ox (cl:+ (cl:* m00 vx) (cl:* m01 vy))
             oy (cl:+ (cl:* m10 vx) (cl:* m11 vy)))))
  out)

(int:define-op *v2 ((in mat) (vec dv2:vec)) (:out dv2:vec)
  (*v2! (dv2:vec) in vec))

(int:define-op transpose! ((out mat) (in mat)) (:out mat)
  (with-components ((o (copy! out in)))
    (rotatef o01 o10))
  out)

(int:define-op transpose ((in mat)) (:out mat)
  (transpose! (mat 1) in))

(int:define-op orthogonal-p ((in mat)) (:out boolean :inline nil)
  (~ (* in (transpose in)) +id+))

(int:define-op trace ((in mat)) (:out double-float :speed nil)
  (with-components ((m in))
    (cl:+ m00 m11)))

(int:define-op diagonal-p ((in mat)) (:out boolean)
  (with-components ((m in))
    (cl:= 0d0 m10 m01)))

(int:define-op main-diagonal! ((out dv2:vec) (in mat)) (:out dv2:vec)
  (with-components ((m in))
    (dv2:with-components ((v out))
      (psetf vx m00 vy m11)))
  out)

(int:define-op main-diagonal ((in mat)) (:out dv2:vec)
  (main-diagonal! (dv2:vec) in))

(int:define-op anti-diagonal! ((out dv2:vec) (in mat)) (:out dv2:vec)
  (with-components ((m in))
    (dv2:with-components ((v out))
      (psetf vx m01 vy m10)))
  out)

(int:define-op anti-diagonal ((in mat)) (:out dv2:vec)
  (anti-diagonal! (dv2:vec) in))
