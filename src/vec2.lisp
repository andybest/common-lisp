(in-package #:cl-user)

(defpackage #:origin.vec2
  (:use #:cl #:origin.internal)
  (:shadow
   #:=
   #:+
   #:-
   #:*
   #:/
   #:random
   #:length
   #:round
   #:abs
   #:<
   #:<=
   #:>
   #:>=
   #:min
   #:max
   #:expt
   #:sqrt
   #:floor
   #:ceiling
   #:mod
   #:sin
   #:cos
   #:tan
   #:asin
   #:acos
   #:atan)
  (:export
   #:vec
   #:x
   #:y
   #:with-components
   #:with-elements
   #:make
   #:+zero+
   #:zero!
   #:zero
   #:one!
   #:one
   #:zero-p
   #:random!
   #:random
   #:copy!
   #:copy
   #:sign!
   #:sign
   #:fract!
   #:fract
   #:clamp!
   #:clamp
   #:=
   #:~
   #:+!
   #:+
   #:-!
   #:-
   #:*!
   #:*
   #:/!
   #:/
   #:scale!
   #:scale
   #:dot
   #:length-squared
   #:length
   #:distance-squared
   #:distance
   #:normalize!
   #:normalize
   #:round!
   #:round
   #:abs!
   #:abs
   #:negate!
   #:negate
   #:angle
   #:direction=
   #:lerp!
   #:lerp
   #:<
   #:<=
   #:>
   #:>=
   #:min!
   #:min
   #:max!
   #:max
   #:radians!
   #:radians
   #:degrees!
   #:degrees
   #:expt!
   #:expt
   #:sqrt!
   #:sqrt
   #:floor!
   #:floor
   #:ceiling!
   #:ceiling
   #:mod!
   #:mod
   #:sin!
   #:sin
   #:cos!
   #:cos
   #:tan!
   #:tan
   #:asin!
   #:asin
   #:acos!
   #:acos
   #:atan!
   #:atan))

(in-package #:origin.vec2)

(deftype vec () '(simple-array single-float (2)))

(defstruct (vec (:type (vector single-float))
                (:constructor %make (x y))
                (:conc-name nil)
                (:predicate nil)
                (:copier nil))
  (x 0f0 :type single-float)
  (y 0f0 :type single-float))

(defmacro with-components (((prefix vec) &rest rest) &body body)
  `(with-accessors ((,prefix identity)
                    (,(make-accessor-symbol prefix 'x) x)
                    (,(make-accessor-symbol prefix 'y) y))
       ,vec
     ,(if rest
          `(with-components ,rest ,@body)
          `(progn ,@body))))

(defmacro with-elements (((prefix x y) &rest rest) &body body)
  (let ((%x (make-accessor-symbol prefix 'x))
        (%y (make-accessor-symbol prefix 'y)))
    `(let ((,%x ,x) (,%y ,y))
       (declare (ignorable ,%x ,%y))
       ,(if rest
            `(with-elements ,rest ,@body)
            `(progn ,@body)))))

(au:define-constant +zero+
    (make-array 2 :element-type 'single-float :initial-contents '(0f0 0f0))
  :test #'equalp)

(define-op make ((x real) (y real)) (:out vec)
  (%make (float x 1f0) (float y 1f0)))

(define-op zero! ((in vec)) (:out vec)
  (with-components ((v in))
    (psetf vx 0f0 vy 0f0))
  in)

(define-op zero () (:out vec)
  (make 0f0 0f0))

(define-op one! ((in vec)) (:out vec)
  (with-components ((v in))
    (psetf vx 1f0 vy 1f0))
  in)

(define-op one () (:out vec)
  (one! (zero)))

(define-op zero-p ((in vec)) (:out boolean)
  (with-components ((v in))
    (and (zerop vx)
         (zerop vy))))

(define-op random! ((out vec) &key (min real 0.0) (max real 1.0)) (:out vec)
  (with-components ((o out))
    (psetf ox (cl:+ min (cl:random (cl:- max min)))
           oy (cl:+ min (cl:random (cl:- max min)))))
  out)

(define-op random ((min real) (max real)) (:out vec)
  (random! (zero) :min min :max max))

(define-op copy! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox vx oy vy))
  out)

(define-op copy ((in vec)) (:out vec)
  (copy! (zero) in))

(define-op sign! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (signum vx)
           oy (signum vy)))
  out)

(define-op sign ((in vec)) (:out vec)
  (sign! (zero) in))

(define-op fract! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (cl:- vx (cl:floor vx))
           oy (cl:- vy (cl:floor vy))))
  out)

(define-op fract ((in vec)) (:out vec)
  (fract! (zero) in))

(define-op clamp! ((out vec) (in vec)
                   &key
                   (min single-float most-negative-single-float)
                   (max single-float most-positive-single-float))
    (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (au:clamp vx min max)
           oy (au:clamp vy min max)))
  out)

(define-op clamp ((in vec)
                  &key
                  (min single-float most-negative-single-float)
                  (max single-float most-positive-single-float))
    (:out vec)
  (clamp! (zero) in :min min :max max))

(define-op = ((in1 vec) (in2 vec)) (:out boolean)
  (with-components ((v1 in1) (v2 in2))
    (and (cl:= v1x v2x)
         (cl:= v1y v2y))))

(define-op ~ ((in1 vec) (in2 vec) &key (tolerance single-float 1e-7))
    (:out boolean)
  (with-components ((v1 in1) (v2 in2))
    (and (cl:< (cl:abs (cl:- v1x v2x)) tolerance)
         (cl:< (cl:abs (cl:- v1y v2y)) tolerance))))

(define-op +! ((out vec) (in1 vec) (in2 vec)) (:out vec)
  (with-components ((o out) (v1 in1) (v2 in2))
    (psetf ox (cl:+ v1x v2x)
           oy (cl:+ v1y v2y)))
  out)

(define-op + ((in1 vec) (in2 vec)) (:out vec)
  (+! (zero) in1 in2))

(define-op -! ((out vec) (in1 vec) (in2 vec)) (:out vec)
  (with-components ((o out) (v1 in1) (v2 in2))
    (psetf ox (cl:- v1x v2x)
           oy (cl:- v1y v2y)))
  out)

(define-op - ((in1 vec) (in2 vec)) (:out vec)
  (-! (zero) in1 in2))

(define-op *! ((out vec) (in1 vec) (in2 vec)) (:out vec)
  (with-components ((o out) (v1 in1) (v2 in2))
    (psetf ox (cl:* v1x v2x)
           oy (cl:* v1y v2y)))
  out)

(define-op * ((in1 vec) (in2 vec)) (:out vec)
  (*! (zero) in1 in2))

(define-op /! ((out vec) (in1 vec) (in2 vec)) (:out vec)
  (with-components ((o out) (v1 in1) (v2 in2))
    (psetf ox (if (zerop v2x) 0f0 (cl:/ v1x v2x))
           oy (if (zerop v2y) 0f0 (cl:/ v1y v2y))))
  out)

(define-op / ((in1 vec) (in2 vec)) (:out vec)
  (/! (zero) in1 in2))

(define-op scale! ((out vec) (in vec) (scalar single-float)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (cl:* vx scalar)
           oy (cl:* vy scalar)))
  out)

(define-op scale ((in vec) (scalar single-float)) (:out vec)
  (scale! (zero) in scalar))

(defmacro %dot (v1x v1y v2x v2y)
  `(cl:+ (cl:* ,v1x ,v2x) (cl:* ,v1y ,v2y)))

(define-op dot ((in1 vec) (in2 vec)) (:out single-float)
  (with-components ((v1 in1) (v2 in2))
    (%dot v1x v1y v2x v2y)))

(define-op length-squared ((in vec)) (:out single-float)
  (dot in in))

(define-op length ((in vec)) (:out single-float)
  (cl:sqrt (length-squared in)))

(define-op distance-squared ((in1 vec) (in2 vec)) (:out single-float)
  (length-squared (- in2 in1)))

(define-op distance ((in1 vec) (in2 vec)) (:out single-float)
  (cl:sqrt (distance-squared in1 in2)))

(define-op normalize! ((out vec) (in vec)) (:out vec)
  (let ((length (length in)))
    (unless (zerop length)
      (scale! out in (cl:/ length))))
  out)

(define-op normalize ((in vec)) (:out vec)
  (normalize! (zero) in))

(define-op round! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (fround vx)
           oy (fround vy)))
  out)

(define-op round ((in vec)) (:out vec)
  (round! (zero) in))

(define-op abs! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (cl:abs vx)
           oy (cl:abs vy)))
  out)

(define-op abs ((in vec)) (:out vec)
  (abs! (zero) in))

(define-op negate! ((out vec) (in vec)) (:out vec)
  (scale! out in -1f0))

(define-op negate ((in vec)) (:out vec)
  (negate! (zero) in))

(define-op angle ((in1 vec) (in2 vec)) (:out single-float)
  (let ((dot (dot in1 in2))
        (m*m (cl:* (length in1) (length in2))))
    (if (zerop m*m) 0f0 (cl:acos (cl:/ dot m*m)))))

(define-op direction= ((in1 vec) (in2 vec)) (:out boolean)
  (cl:>= (dot (normalize in1) (normalize in2)) (cl:- 1 1e-7)))

(define-op lerp! ((out vec) (in1 vec) (in2 vec) (factor single-float)) (:out vec)
  (with-components ((o out) (v1 in1) (v2 in2))
    (psetf ox (au:lerp factor v1x v2x)
           oy (au:lerp factor v1y v2y)))
  out)

(define-op lerp ((in1 vec) (in2 vec) (factor single-float)) (:out vec)
  (lerp! (zero) in1 in2 factor))

(define-op < ((in1 vec) (in2 vec)) (:out boolean)
  (with-components ((v1 in1) (v2 in2))
    (and (cl:< v1x v2x)
         (cl:< v1y v2y))))

(define-op <= ((in1 vec) (in2 vec)) (:out boolean)
  (with-components ((v1 in1) (v2 in2))
    (and (cl:<= v1x v2x)
         (cl:<= v1y v2y))))

(define-op > ((in1 vec) (in2 vec)) (:out boolean)
  (with-components ((v1 in1) (v2 in2))
    (and (cl:> v1x v2x)
         (cl:> v1y v2y))))

(define-op >= ((in1 vec) (in2 vec)) (:out boolean)
  (with-components ((v1 in1) (v2 in2))
    (and (cl:>= v1x v2x)
         (cl:>= v1y v2y))))

(define-op min! ((out vec) (in1 vec) (in2 vec)) (:out vec)
  (with-components ((o out) (v1 in1) (v2 in2))
    (psetf ox (cl:min v1x v2x)
           oy (cl:min v1y v2y)))
  out)

(define-op min ((in1 vec) (in2 vec)) (:out vec)
  (min! (zero) in1 in2))

(define-op max! ((out vec) (in1 vec) (in2 vec)) (:out vec)
  (with-components ((o out) (v1 in1) (v2 in2))
    (psetf ox (cl:max v1x v2x)
           oy (cl:max v1y v2y)))
  out)

(define-op max ((in1 vec) (in2 vec)) (:out vec)
  (max! (zero) in1 in2))

(define-op radians! ((out vec) (vec vec)) (:out vec)
  (with-components ((o out) (v vec))
    (let ((x (float (cl:/ pi 180) 1f0)))
      (psetf ox (cl:* vx x)
             oy (cl:* vy x)))
    out))

(define-op radians ((in vec)) (:out vec)
  (radians! (zero) in))

(define-op degrees! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (let ((x (float (cl:/ 180 pi) 1f0)))
      (psetf ox (cl:* vx x)
             oy (cl:* vy x))))
  out)

(define-op degrees ((in vec)) (:out vec)
  (degrees! (zero) in))

(define-op expt! ((out vec) (in vec) (power real)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (cl:expt vx power)
           oy (cl:expt vy power)))
  out)

(define-op expt ((in vec) (power real)) (:out vec)
  (expt! (zero) in power))

(define-op sqrt! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (cl:sqrt vx)
           oy (cl:sqrt vy)))
  out)

(define-op sqrt ((in vec)) (:out vec)
  (sqrt! (zero) in))

(define-op floor! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (float (cl:floor vx) 1f0)
           oy (float (cl:floor vy) 1f0)))
  out)

(define-op floor ((in vec)) (:out vec)
  (floor! (zero) in))

(define-op ceiling! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (float (cl:ceiling vx) 1f0)
           oy (float (cl:ceiling vy) 1f0)))
  out)

(define-op ceiling ((in vec)) (:out vec)
  (ceiling! (zero) in))

(define-op mod! ((out vec) (in vec) (divisor real)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (cl:mod vx divisor)
           oy (cl:mod vy divisor)))
  out)

(define-op mod ((in vec) (divisor real)) (:out vec)
  (mod! (zero) in divisor))

(define-op sin! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (cl:sin vx)
           oy (cl:sin vy)))
  out)

(define-op sin ((in vec)) (:out vec)
  (sin! (zero) in))

(define-op cos! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (cl:cos vx)
           oy (cl:cos vy)))
  out)

(define-op cos ((in vec)) (:out vec)
  (cos! (zero) in))

(define-op tan! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (cl:tan vx)
           oy (cl:tan vy)))
  out)

(define-op tan ((in vec)) (:out vec)
  (tan! (zero) in))

(define-op asin! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (cl:asin vx)
           oy (cl:asin vy)))
  out)

(define-op asin ((in vec)) (:out vec)
  (asin! (zero) in))

(define-op acos! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (cl:acos vx)
           oy (cl:acos vy)))
  out)

(define-op acos ((in vec)) (:out vec)
  (acos! (zero) in))

(define-op atan! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (cl:atan vx)
           oy (cl:atan vy)))
  out)

(define-op atan ((in vec)) (:out vec)
  (atan! (zero) in))
