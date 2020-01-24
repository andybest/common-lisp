(in-package #:origin.vec3)

;;; accessors

(define-op x ((vec vec)) (:out single-float)
  (aref vec 0))

(define-op (setf x) ((value single-float) (vec vec)) (:out single-float)
  (setf (aref vec 0) value))

(define-op y ((vec vec)) (:out single-float)
  (aref vec 1))

(define-op (setf y) ((value single-float) (vec vec)) (:out single-float)
  (setf (aref vec 1) value))

(define-op z ((vec vec)) (:out single-float)
  (aref vec 2))

(define-op (setf z) ((value single-float) (vec vec)) (:out single-float)
  (setf (aref vec 2) value))


;;; constructors

(define-op %vec (&rest (args single-float)) (:inline t :out vec)
  (make-array 3 :element-type 'single-float :initial-contents args))

(defstore vec (&rest args))

(defspecialization (vec :inline t) () vec
  (%vec 0f0 0f0 0f0))

(defspecialization (vec :inline t) ((x real)) vec
  (%vec (float x 1f0) (float x 1f0) (float x 1f0)))

(defspecialization (vec :inline t) ((xy v2:vec)) vec
  (v2:with-components ((v xy))
    (%vec vx vy 0f0)))

(defspecialization (vec :inline t) ((xyz vec)) vec
  (with-components ((v xyz))
    (%vec vx vy vz)))

(defspecialization (vec :inline t) ((xyzw v4:vec)) vec
  (v4:with-components ((v xyzw))
    (%vec vx vy vz)))

(defspecialization (vec :inline t) ((x real) (y real)) vec
  (%vec (float x 1f0) (float y 1f0) 0f0))

(defspecialization (vec :inline t) ((xy v2:vec) (z real)) vec
  (v2:with-components ((v xy))
    (%vec vx vy (float z 1f0))))

(defspecialization (vec :inline t) ((x real) (yz v2:vec)) vec
  (v2:with-components ((v yz))
    (%vec (float x 1f0) vx vy)))

(defspecialization (vec :inline t) ((x real) (y real) (z real)) vec
  (%vec (float x 1f0) (float y 1f0) (float z 1f0)))

;;; constants

(a:define-constant +zero+ (vec 0f0 0f0 0f0) :test #'equalp)

(a:define-constant +up+ (vec 0f0 1f0 0f0) :test #'equalp)

(a:define-constant +down+ (vec 0f0 -1f0 0f0) :test #'equalp)

(a:define-constant +left+ (vec -1f0 0f0 0f0) :test #'equalp)

(a:define-constant +right+ (vec 1f0 0f0 0f0) :test #'equalp)

(a:define-constant +forward+ (vec 0f0 0f0 1f0) :test #'equalp)

(a:define-constant +back+ (vec 0f0 0f0 -1f0) :test #'equalp)

;;; operators

(define-op zero! ((in vec)) (:out vec)
  (with-components ((v in))
    (psetf vx 0f0 vy 0f0 vz 0f0))
  in)

(define-op zero-p ((in vec)) (:out boolean)
  (with-components ((v in))
    (cl:= 0f0 vx vy vz)))

(define-op random! ((out vec)
                    &key (min single-float 0f0) (max single-float 1f0))
    (:out vec)
  (with-components ((o out))
    (psetf ox (cl:+ min (cl:random (cl:- max min)))
           oy (cl:+ min (cl:random (cl:- max min)))
           oz (cl:+ min (cl:random (cl:- max min)))))
  out)

(define-op random (&key (min single-float 0f0) (max single-float 1f0))
    (:out vec)
  (random! (vec) :min min :max max))

(define-op copy! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox vx oy vy oz vz))
  out)

(define-op copy ((in vec)) (:out vec)
  (copy! (vec) in))

(define-op sign! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (signum vx)
           oy (signum vy)
           oz (signum vz)))
  out)

(define-op sign ((in vec)) (:out vec)
  (sign! (vec) in))

(define-op fract! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (cl:- vx (ffloor vx))
           oy (cl:- vy (ffloor vy))
           oz (cl:- vz (ffloor vz))))
  out)

(define-op fract ((in vec)) (:out vec)
  (fract! (vec) in))

(define-op clamp! ((out vec) (in vec)
                   &key
                   (min single-float most-negative-single-float)
                   (max single-float most-positive-single-float))
    (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (a:clamp vx min max)
           oy (a:clamp vy min max)
           oz (a:clamp vz min max)))
  out)

(define-op clamp ((in vec)
                  &key
                  (min single-float most-negative-single-float)
                  (max single-float most-positive-single-float))
    (:out vec)
  (clamp! (vec) in :min min :max max))

(define-op = ((in1 vec) (in2 vec)) (:out boolean)
  (with-components ((v1 in1) (v2 in2))
    (and (cl:= v1x v2x)
         (cl:= v1y v2y)
         (cl:= v1z v2z))))

(define-op ~ ((in1 vec) (in2 vec) &key (tolerance single-float 1e-7))
    (:out boolean)
  (with-components ((v1 in1) (v2 in2))
    (and (cl:< (cl:abs (cl:- v1x v2x)) tolerance)
         (cl:< (cl:abs (cl:- v1y v2y)) tolerance)
         (cl:< (cl:abs (cl:- v1z v2z)) tolerance))))

(define-op +! ((out vec) (in1 vec) (in2 vec)) (:out vec)
  (with-components ((o out) (v1 in1) (v2 in2))
    (psetf ox (cl:+ v1x v2x)
           oy (cl:+ v1y v2y)
           oz (cl:+ v1z v2z)))
  out)

(define-op + ((in1 vec) (in2 vec)) (:out vec)
  (+! (vec) in1 in2))

(define-op -! ((out vec) (in1 vec) (in2 vec)) (:out vec)
  (with-components ((o out) (v1 in1) (v2 in2))
    (psetf ox (cl:- v1x v2x)
           oy (cl:- v1y v2y)
           oz (cl:- v1z v2z)))
  out)

(define-op - ((in1 vec) (in2 vec)) (:out vec)
  (-! (vec) in1 in2))

(define-op *! ((out vec) (in1 vec) (in2 vec)) (:out vec)
  (with-components ((o out) (v1 in1) (v2 in2))
    (psetf ox (cl:* v1x v2x)
           oy (cl:* v1y v2y)
           oz (cl:* v1z v2z)))
  out)

(define-op * ((in1 vec) (in2 vec)) (:out vec)
  (*! (vec) in1 in2))

(define-op /! ((out vec) (in1 vec) (in2 vec)) (:out vec)
  (with-components ((o out) (v1 in1) (v2 in2))
    (psetf ox (if (zerop v2x) 0f0 (cl:/ v1x v2x))
           oy (if (zerop v2y) 0f0 (cl:/ v1y v2y))
           oz (if (zerop v2z) 0f0 (cl:/ v1z v2z))))
  out)

(define-op / ((in1 vec) (in2 vec)) (:out vec)
  (/! (vec) in1 in2))

(defmacro %scale (ox oy oz x y z scalar)
  `(psetf ,ox (cl:* ,x ,scalar)
          ,oy (cl:* ,y ,scalar)
          ,oz (cl:* ,z ,scalar)))

(define-op scale! ((out vec) (in vec) (scalar single-float)) (:out vec)
  (with-components ((o out) (v in))
    (%scale ox oy oz vx vy vz scalar))
  out)

(define-op scale ((in vec) (scalar single-float)) (:out vec)
  (scale! (vec) in scalar))

(defmacro %dot (v1x v1y v1z v2x v2y v2z)
  `(cl:+ (cl:* ,v1x ,v2x) (cl:* ,v1y ,v2y) (cl:* ,v1z ,v2z)))

(define-op dot ((in1 vec) (in2 vec)) (:out single-float)
  (with-components ((v1 in1) (v2 in2))
    (%dot v1x v1y v1z v2x v2y v2z)))

(defmacro %length-squared (x y z)
  ;; NOTE: This is not using %DOT because using * instead of EXPT and SBCL 1.5.9
  ;; cannot correctly infer the type of the SQRT of the sum of squares as being
  ;; a single-float. This is because SBCL's memory model policy is "everything
  ;; is volatile", which is acceptable because two AREF calls to the same array
  ;; may infact produce different values when threading is involved.
  `(cl:+ (cl:expt ,x 2) (cl:expt ,y 2) (cl:expt ,z 2)))

(define-op length-squared ((in vec)) (:out single-float)
  (with-components ((v in))
    (%length-squared vx vy vz)))

(defmacro %length (x y z)
  `(cl:sqrt (%length-squared ,x ,y ,z)))

(define-op length ((in vec)) (:out single-float)
  (cl:sqrt (length-squared in)))

(define-op distance-squared ((in1 vec) (in2 vec)) (:out single-float)
  (length-squared (- in2 in1)))

(define-op distance ((in1 vec) (in2 vec)) (:out single-float)
  (cl:sqrt (distance-squared in1 in2)))

(defmacro %normalize (ox oy oz x y z)
  (a:with-gensyms (length)
    `(let ((,length (%length ,x ,y ,z)))
       (unless (zerop ,length)
         (%scale ,ox ,oy ,oz ,x ,y ,z (cl:/ ,length))))))

(define-op normalize! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (%normalize ox oy oz vx vy vz))
  out)

(define-op normalize ((in vec)) (:out vec)
  (normalize! (vec) in))

(define-op round! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (fround vx)
           oy (fround vy)
           oz (fround vz)))
  out)

(define-op round ((in vec)) (:out vec)
  (round! (vec) in))

(define-op abs! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (cl:abs vx)
           oy (cl:abs vy)
           oz (cl:abs vz)))
  out)

(define-op abs ((in vec)) (:out vec)
  (abs! (vec) in))

(define-op negate! ((out vec) (in vec)) (:out vec)
  (scale! out in -1f0))

(define-op negate ((in vec)) (:out vec)
  (negate! (vec) in))

(define-op cross! ((out vec) (in1 vec) (in2 vec)) (:out vec)
  (with-components ((o out) (v1 in1) (v2 in2))
    (psetf ox (cl:- (cl:* v1y v2z) (cl:* v1z v2y))
           oy (cl:- (cl:* v1z v2x) (cl:* v1x v2z))
           oz (cl:- (cl:* v1x v2y) (cl:* v1y v2x))))
  out)

(define-op cross ((in1 vec) (in2 vec)) (:out vec)
  (cross! (vec) in1 in2))

(define-op box ((in1 vec) (in2 vec) (in3 vec)) (:out single-float)
  (dot (cross in1 in2) in3))

(define-op angle ((in1 vec) (in2 vec)) (:out single-float :speed nil)
  (let ((dot (dot in1 in2))
        (m*m (cl:* (length in1) (length in2))))
    (if (zerop m*m) 0f0 (cl:acos (cl:/ dot m*m)))))

(define-op direction= ((in1 vec) (in2 vec)) (:out boolean)
  (cl:>= (dot (normalize in1) (normalize in2)) (cl:- 1 1e-7)))

(define-op parallel-p ((in1 vec) (in2 vec)) (:out boolean)
  (~ (cross in1 in2) +zero+))

(define-op lerp! ((out vec) (in1 vec) (in2 vec) (factor single-float))
    (:out vec)
  (with-components ((o out) (v1 in1) (v2 in2))
    (psetf ox (a:lerp factor v1x v2x)
           oy (a:lerp factor v1y v2y)
           oz (a:lerp factor v1z v2z)))
  out)

(define-op lerp ((in1 vec) (in2 vec) (factor single-float)) (:out vec)
  (lerp! (vec) in1 in2 factor))

(define-op < ((in1 vec) (in2 vec)) (:out boolean)
  (with-components ((v1 in1) (v2 in2))
    (and (cl:< v1x v2x)
         (cl:< v1y v2y)
         (cl:< v1z v2z))))

(define-op <= ((in1 vec) (in2 vec)) (:out boolean)
  (with-components ((v1 in1) (v2 in2))
    (and (cl:<= v1x v2x)
         (cl:<= v1y v2y)
         (cl:<= v1z v2z))))

(define-op > ((in1 vec) (in2 vec)) (:out boolean)
  (with-components ((v1 in1) (v2 in2))
    (and (cl:> v1x v2x)
         (cl:> v1y v2y)
         (cl:> v1z v2z))))

(define-op >= ((in1 vec) (in2 vec)) (:out boolean)
  (with-components ((v1 in1) (v2 in2))
    (and (cl:>= v1x v2x)
         (cl:>= v1y v2y)
         (cl:>= v1z v2z))))

(define-op min! ((out vec) (in1 vec) (in2 vec)) (:out vec)
  (with-components ((o out) (v1 in1) (v2 in2))
    (psetf ox (cl:min v1x v2x)
           oy (cl:min v1y v2y)
           oz (cl:min v1z v2z)))
  out)

(define-op min ((in1 vec) (in2 vec)) (:out vec)
  (min! (vec) in1 in2))

(define-op max! ((out vec) (in1 vec) (in2 vec)) (:out vec)
  (with-components ((o out) (v1 in1) (v2 in2))
    (psetf ox (cl:max v1x v2x)
           oy (cl:max v1y v2y)
           oz (cl:max v1z v2z)))
  out)

(define-op max ((in1 vec) (in2 vec)) (:out vec)
  (max! (vec) in1 in2))

(define-op radians! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (cl:* vx origin:+deg+)
           oy (cl:* vy origin:+deg+)
           oz (cl:* vz origin:+deg+))
    out))

(define-op radians ((in vec)) (:out vec)
  (radians! (vec) in))

(define-op degrees! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (cl:* vx origin:+rad+)
           oy (cl:* vy origin:+rad+)
           oz (cl:* vz origin:+rad+)))
  out)

(define-op degrees ((in vec)) (:out vec)
  (degrees! (vec) in))

(define-op expt! ((out vec) (in vec) (power real)) (:out vec :speed nil)
  (with-components ((o out) (v in))
    (psetf ox (cl:expt vx power)
           oy (cl:expt vy power)
           oz (cl:expt vz power)))
  out)

(define-op expt ((in vec) (power real)) (:out vec :speed nil)
  (expt! (vec) in power))

(define-op sqrt! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (check-type vx (single-float 0f0))
    (check-type vy (single-float 0f0))
    (check-type vz (single-float 0f0))
    (psetf ox (cl:sqrt (the (single-float 0f0) vx))
           oy (cl:sqrt (the (single-float 0f0) vy))
           oz (cl:sqrt (the (single-float 0f0) vz))))
  out)

(define-op sqrt ((in vec)) (:out vec)
  (sqrt! (vec) in))

(define-op floor! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (ffloor vx)
           oy (ffloor vy)
           oz (ffloor vz)))
  out)

(define-op floor ((in vec)) (:out vec)
  (floor! (vec) in))

(define-op ceiling! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (fceiling vx)
           oy (fceiling vy)
           oz (fceiling vz)))
  out)

(define-op ceiling ((in vec)) (:out vec)
  (ceiling! (vec) in))

(define-op mod! ((out vec) (in vec) (divisor real)) (:out vec :speed nil)
  (with-components ((o out) (v in))
    (psetf ox (cl:mod vx divisor)
           oy (cl:mod vy divisor)
           oz (cl:mod vz divisor)))
  out)

(define-op mod ((in vec) (divisor real)) (:out vec :speed nil)
  (mod! (vec) in divisor))

(define-op sin! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (cl:sin vx)
           oy (cl:sin vy)
           oz (cl:sin vz)))
  out)

(define-op sin ((in vec)) (:out vec)
  (sin! (vec) in))

(define-op cos! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (cl:cos vx)
           oy (cl:cos vy)
           oz (cl:cos vz)))
  out)

(define-op cos ((in vec)) (:out vec)
  (cos! (vec) in))

(define-op tan! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (cl:tan vx)
           oy (cl:tan vy)
           oz (cl:tan vz)))
  out)

(define-op tan ((in vec)) (:out vec)
  (tan! (vec) in))

(define-op asin! ((out vec) (in vec)) (:out vec :speed nil)
  (with-components ((o out) (v in))
    (psetf ox (cl:asin vx)
           oy (cl:asin vy)
           oz (cl:asin vz)))
  out)

(define-op asin ((in vec)) (:out vec :speed nil)
  (asin! (vec) in))

(define-op acos! ((out vec) (in vec)) (:out vec :speed nil)
  (with-components ((o out) (v in))
    (psetf ox (cl:acos vx)
           oy (cl:acos vy)
           oz (cl:acos vz)))
  out)

(define-op acos ((in vec)) (:out vec :speed nil)
  (acos! (vec) in))

(define-op atan! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (cl:atan vx)
           oy (cl:atan vy)
           oz (cl:atan vz)))
  out)

(define-op atan ((in vec)) (:out vec)
  (atan! (vec) in))
