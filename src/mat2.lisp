(in-package #:cl-user)

(defpackage #:origin.mat2
  (:local-nicknames (#:v2 #:origin.vec2))
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
   #:+zero+
   #:+id+
   #:make
   #:zero!
   #:zero
   #:random
   #:id!
   #:id
   #:id-p
   #:=
   #:~
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
   #:*v2!
   #:*v2
   #:transpose!
   #:transpose
   #:orthogonal-p
   #:trace
   #:diagonal-p
   #:main-diagonal!
   #:main-diagonal
   #:anti-diagonal!
   #:anti-diagonal))

(in-package #:origin.mat2)

(deftype mat () '(simple-array single-float (4)))

(defstruct (matrix (:type (vector single-float))
                   (:constructor %make (m00 m01 m10 m11))
                   (:conc-name nil)
                   (:predicate nil)
                   (:copier nil))
  (m00 0f0 :type single-float)
  (m10 0f0 :type single-float)
  (m01 0f0 :type single-float)
  (m11 0f0 :type single-float))

(defmacro with-components (((prefix matrix) &rest rest) &body body)
  `(with-accessors ((,prefix identity)
                    (,(make-accessor-symbol prefix "00") m00)
                    (,(make-accessor-symbol prefix "01") m01)
                    (,(make-accessor-symbol prefix "10") m10)
                    (,(make-accessor-symbol prefix "11") m11))
       ,matrix
     ,(if rest
          `(with-components ,rest ,@body)
          `(progn ,@body))))

(au:define-constant +zero+
    (make-array 4 :element-type 'single-float
                  :initial-contents '(0f0 0f0 0f0 0f0))
  :test #'equalp)

(au:define-constant +id+
    (make-array 4 :element-type 'single-float
                  :initial-contents '(1f0 0f0 0f0 1f0))
  :test #'equalp)

(define-op make ((m00 real) (m01 real) (m10 real) (m11 real)) (:out mat)
  (%make (float m00 1f0) (float m01 1f0) (float m10 1f0) (float m11 1f0)))

(define-op zero! ((in mat)) (:out mat)
  (with-components ((m in))
    (psetf m00 0f0 m01 0f0 m10 0f0 m11 0f0))
  in)

(define-op zero () (:out mat)
  (%make 0f0 0f0 0f0 0f0))

(define-op id! ((in mat)) (:out mat)
  (with-components ((m in))
    (psetf m00 1f0 m01 0f0 m10 0f0 m11 1f0))
  in)

(define-op id () (:out mat)
  (%make 1f0 0f0 0f0 1f0))

(define-op id-p ((in mat)) (:out boolean)
  (with-components ((m in))
    (and (cl:= 0f0 m10 m01)
         (cl:= 1f0 m00 m11))))

(define-op = ((in1 mat) (in2 mat)) (:out boolean)
  (with-components ((a in1) (b in2))
    (and (cl:= a00 b00) (cl:= a01 b01)
         (cl:= a10 b10) (cl:= a11 b11))))

(define-op ~ ((in1 mat) (in2 mat) &key (tolerance single-float 1e-7))
    (:out boolean)
  (with-components ((a in1) (b in2))
    (and
     (cl:< (cl:abs (cl:- a00 b00)) tolerance)
     (cl:< (cl:abs (cl:- a01 b01)) tolerance)
     (cl:< (cl:abs (cl:- a10 b10)) tolerance)
     (cl:< (cl:abs (cl:- a11 b11)) tolerance))))

(define-op random! ((out mat) &key (min real 0.0) (max real 1.0)) (:out mat)
  (with-components ((o out))
    (psetf o00 (cl:+ min (cl:random (cl:- max min)))
           o01 (cl:+ min (cl:random (cl:- max min)))
           o10 (cl:+ min (cl:random (cl:- max min)))
           o11 (cl:+ min (cl:random (cl:- max min)))))
  out)

(define-op random (&key (min real 0.0) (max real 1.0)) (:out mat)
  (random! (zero) :min min :max max))

(define-op copy! ((out mat) (in mat)) (:out mat)
  (with-components ((o out) (m in))
    (psetf o00 m00 o01 m01 o10 m10 o11 m11))
  out)

(define-op copy ((in mat)) (:out mat)
  (copy! (zero) in))

(define-op clamp! ((out mat) (in mat)
                   &key
                   (min single-float most-negative-single-float)
                   (max single-float most-positive-single-float))
    (:out mat)
  (with-components ((o out) (m in))
    (psetf o00 (au:clamp m00 min max)
           o01 (au:clamp m01 min max)
           o10 (au:clamp m10 min max)
           o11 (au:clamp m11 min max)))
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
           o01 (cl:+ a01 b01)
           o11 (cl:+ a11 b11)))
  out)

(define-op + ((in1 mat) (in2 mat)) (:out mat)
  (+! (zero) in1 in2))

(define-op -! ((out mat) (in1 mat) (in2 mat)) (:out mat)
  (with-components ((o out) (a in1) (b in2))
    (psetf o00 (cl:- a00 b00)
           o10 (cl:- a10 b10)
           o01 (cl:- a01 b01)
           o11 (cl:- a11 b11)))
  out)

(define-op - ((in1 mat) (in2 mat)) (:out mat)
  (-! (zero) in1 in2))

(define-op *! ((out mat) (in1 mat) (in2 mat)) (:out mat)
  (with-components ((o out) (a in1) (b in2))
    (psetf o00 (cl:+ (cl:* a00 b00) (cl:* a01 b10))
           o10 (cl:+ (cl:* a10 b00) (cl:* a11 b10))
           o01 (cl:+ (cl:* a00 b01) (cl:* a01 b11))
           o11 (cl:+ (cl:* a10 b01) (cl:* a11 b11))))
  out)

(define-op * ((in1 mat) (in2 mat)) (:out mat)
  (*! (zero) in1 in2))

(define-op get-column! ((out v2:vec) (in mat) (index (integer 0 1)))
    (:out v2:vec)
  (with-components ((m in))
    (v2:with-components ((o out))
      (ecase index
        (0 (psetf ox m00 oy m10))
        (1 (psetf ox m01 oy m11)))))
  out)

(define-op get-column ((in mat) (index (integer 0 1))) (:out v2:vec)
  (get-column! (v2:zero) in index))

(define-op set-column! ((out mat) (in mat) (vec v2:vec) (index (integer 0 1)))
    (:out mat)
  (with-components ((o out))
    (v2:with-components ((v vec))
      (copy! out in)
      (ecase index
        (0 (psetf o00 vx o10 vy))
        (1 (psetf o01 vx o11 vy)))))
  out)

(define-op set-column ((in mat) (vec v2:vec) (index (integer 0 1))) (:out mat)
  (set-column! (id) in vec index))

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

(define-op rotate! ((out mat) (in mat) (angle float) &key (space keyword :local))
    (:out mat :inline nil)
  (with-components ((m (id)))
    (copy! out in)
    (when (cl:> (abs angle) 1e-7)
      (let* ((angle (float angle 1f0))
             (s (sin angle))
             (c (cos angle)))
        (psetf m00 c m01 (cl:- s)
               m10 s m11 c)
        (ecase space
          (:local (*! out out m))
          (:world (*! out m out))))))
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
  (*! out (set-scale (id) vec) in))

(define-op scale ((in mat) (vec v2:vec)) (:out mat)
  (scale! (id) in vec))

(define-op *v2! ((out v2:vec) (in mat) (vec v2:vec)) (:out v2:vec)
  (v2:with-components ((v vec) (o out))
    (with-components ((m in))
      (psetf ox (cl:+ (cl:* m00 vx) (cl:* m01 vy))
             oy (cl:+ (cl:* m10 vx) (cl:* m11 vy)))))
  out)

(define-op *v2 ((in mat) (vec v2:vec)) (:out v2:vec)
  (*v2! (v2:zero) in vec))

(define-op transpose! ((out mat) (in mat)) (:out mat)
  (with-components ((o (copy! out in)))
    (rotatef o01 o10))
  out)

(define-op transpose ((in mat)) (:out mat)
  (transpose! (id) in))

(define-op orthogonal-p ((in mat)) (:out boolean :inline nil)
  (~ (* in (transpose in)) +id+))

(define-op trace ((in mat)) (:out single-float)
  (with-components ((m in))
    (cl:+ m00 m11)))

(define-op diagonal-p ((in mat)) (:out boolean)
  (with-components ((m in))
    (and (zerop m10)
         (zerop m01))))

(define-op main-diagonal! ((out v2:vec) (in mat)) (:out v2:vec)
  (with-components ((m in))
    (v2:with-components ((v out))
      (psetf vx m00 vy m11)))
  out)

(define-op main-diagonal ((in mat)) (:out v2:vec)
  (main-diagonal! (v2:zero) in))

(define-op anti-diagonal! ((out v2:vec) (in mat)) (:out v2:vec)
  (with-components ((m in))
    (v2:with-components ((v out))
      (psetf vx m01 vy m10)))
  out)

(define-op anti-diagonal ((in mat)) (:out v2:vec)
  (anti-diagonal! (v2:zero) in))
