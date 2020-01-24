(in-package #:origin.vec4)

;;; accessors

(define-op x ((vec vec)) (:out single-float)
  (aref vec 0))

(define-op (setf x) ((new-value single-float) (vec vec)) (:out single-float)
  (setf (aref vec 0) new-value))

(define-op y ((vec vec)) (:out single-float)
  (aref vec 1))

(define-op (setf y) ((new-value single-float) (vec vec)) (:out single-float)
  (setf (aref vec 1) new-value))

(define-op z ((vec vec)) (:out single-float)
  (aref vec 2))

(define-op (setf z) ((new-value single-float) (vec vec)) (:out single-float)
  (setf (aref vec 2) new-value))

(define-op w ((vec vec)) (:out single-float)
  (aref vec 3))

(define-op (setf w) ((new-value single-float) (vec vec)) (:out single-float)
  (setf (aref vec 3) new-value))


;;; constructors

(define-op %vec (&rest (args single-float)) (:inline t :out vec)
  (make-array 4 :element-type 'single-float :initial-contents args))

(defstore vec (&rest args))

(defspecialization (vec :inline t) () vec
  (%vec 0f0 0f0 0f0 0f0))

(defspecialization (vec :inline t) ((x real)) vec
  (%vec (float x 1f0) (float x 1f0) (float x 1f0) (float x 1f0)))

(defspecialization (vec :inline t) ((xy v2:vec)) vec
  (v2:with-components ((v xy))
    (%vec vx vy 0f0 0f0)))

(defspecialization (vec :inline t) ((xyz v3:vec)) vec
  (v3:with-components ((v xyz))
    (%vec vx vy vz 0f0)))

(defspecialization (vec :inline t) ((xyzw vec)) vec
  (with-components ((v xyzw))
    (%vec vx vy vz vw)))

(defspecialization (vec :inline t) ((x real) (y real)) vec
  (%vec (float x 1f0) (float y 1f0) 0f0 0f0))

(defspecialization (vec :inline t) ((xy v2:vec) (z real)) vec
  (v2:with-components ((v xy))
    (%vec vx vy (float z 1f0) 0f0)))

(defspecialization (vec :inline t) ((x real) (yz v2:vec)) vec
  (v2:with-components ((v yz))
    (%vec (float x 1f0) vx vy 0f0)))

(defspecialization (vec :inline t) ((xy v2:vec) (zw v2:vec)) vec
  (v2:with-components ((v1 xy) (v2 zw))
    (%vec v1x v1y v2x v2y)))

(defspecialization (vec :inline t) ((x real) (yzw v3:vec)) vec
  (v3:with-components ((v yzw))
    (%vec (float x 1f0) vx vy vz)))

(defspecialization (vec :inline t) ((xyz v3:vec) (w real)) vec
  (v3:with-components ((v xyz))
    (%vec vx vy vz (float w 1f0))))

(defspecialization (vec :inline t) ((x real) (y real) (z real)) vec
  (%vec (float x 1f0) (float y 1f0) (float z 1f0) 0f0))

(defspecialization (vec :inline t) ((xy v2:vec) (z real) (w real)) vec
  (v2:with-components ((v xy))
    (%vec vx vy (float z 1f0) (float w 1f0))))

(defspecialization (vec :inline t) ((x real) (y real) (zw v2:vec)) vec
  (v2:with-components ((v zw))
    (%vec (float x 1f0) (float y 1f0) vx vy)))

(defspecialization (vec :inline t) ((x real) (yz v2:vec) (w real)) vec
  (v2:with-components ((v yz))
    (%vec (float x 1f0) vx vy (float w 1f0))))

(defspecialization (vec :inline t) ((x real) (y real) (z real) (w real)) vec
  (%vec (float x 1f0) (float y 1f0) (float z 1f0) (float w 1f0)))

(defspecialization (vec :inline t) ((wxyz q:quat)) vec
  (q:with-components ((q wxyz))
    (%vec qw qx qy qz)))

;;; constants

(a:define-constant +zero+ (vec 0f0 0f0 0f0 0f0) :test #'equalp)

;;; operators

(define-op zero! ((in vec)) (:out vec)
  (with-components ((v in))
    (psetf vx 0f0 vy 0f0 vz 0f0 vw 0f0))
  in)

(define-op zero () (:out vec)
  (vec 0f0 0f0 0f0 0f0))

(define-op one! ((in vec)) (:out vec)
  (with-components ((v in))
    (psetf vx 1f0 vy 1f0 vz 1f0 vw 1f0))
  in)

(define-op one () (:out vec)
  (one! (zero)))

(define-op zero-p ((in vec)) (:out boolean)
  (with-components ((v in))
    (cl:= 0f0 vx vy vz)))

(define-op random! ((out vec)
                    &key (min single-float 0f0) (max single-float 1f0))
    (:out vec)
  (with-components ((o out))
    (psetf ox (cl:+ min (cl:random (cl:- max min)))
           oy (cl:+ min (cl:random (cl:- max min)))
           oz (cl:+ min (cl:random (cl:- max min)))
           ow (cl:+ min (cl:random (cl:- max min)))))
  out)

(define-op random (&key (min single-float 0f0) (max single-float 1f0))
    (:out vec)
  (random! (zero) :min min :max max))

(define-op copy! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox vx oy vy oz vz ow vw))
  out)

(define-op copy ((in vec)) (:out vec)
  (copy! (zero) in))

(define-op sign! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (signum vx)
           oy (signum vy)
           oz (signum vz)
           ow (signum vw)))
  out)

(define-op sign ((in vec)) (:out vec)
  (sign! (zero) in))

(define-op fract! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (cl:- vx (ffloor vx))
           oy (cl:- vy (ffloor vy))
           oz (cl:- vz (ffloor vz))
           ow (cl:- vw (ffloor vw))))
  out)

(define-op fract ((in vec)) (:out vec)
  (fract! (zero) in))

(define-op clamp! ((out vec) (in vec)
                   &key
                   (min single-float most-negative-single-float)
                   (max single-float most-positive-single-float))
    (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (a:clamp vx min max)
           oy (a:clamp vy min max)
           oz (a:clamp vz min max)
           ow (a:clamp vw min max)))
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
         (cl:= v1y v2y)
         (cl:= v1z v2z)
         (cl:= v1w v2w))))

(define-op ~ ((in1 vec) (in2 vec) &key (tolerance single-float 1e-7))
    (:out boolean)
  (with-components ((v1 in1) (v2 in2))
    (and (cl:< (cl:abs (cl:- v1x v2x)) tolerance)
         (cl:< (cl:abs (cl:- v1y v2y)) tolerance)
         (cl:< (cl:abs (cl:- v1z v2z)) tolerance)
         (cl:< (cl:abs (cl:- v1w v2w)) tolerance))))

(define-op +! ((out vec) (in1 vec) (in2 vec)) (:out vec)
  (declare (optimize speed (space 0) (debug 0)))
  (with-components ((o out) (v1 in1) (v2 in2))
    (psetf ox (cl:+ v1x v2x)
           oy (cl:+ v1y v2y)
           oz (cl:+ v1z v2z)
           ow (cl:+ v1w v2w)))
  out)

(define-op + ((in1 vec) (in2 vec)) (:out vec)
  (+! (zero) in1 in2))

(define-op -! ((out vec) (in1 vec) (in2 vec)) (:out vec)
  (with-components ((o out) (v1 in1) (v2 in2))
    (psetf ox (cl:- v1x v2x)
           oy (cl:- v1y v2y)
           oz (cl:- v1z v2z)
           ow (cl:- v1w v2w)))
  out)

(define-op - ((in1 vec) (in2 vec)) (:out vec)
  (-! (zero) in1 in2))

(define-op *! ((out vec) (in1 vec) (in2 vec)) (:out vec)
  (with-components ((o out) (v1 in1) (v2 in2))
    (psetf ox (cl:* v1x v2x)
           oy (cl:* v1y v2y)
           oz (cl:* v1z v2z)
           ow (cl:* v1w v2w)))
  out)

(define-op * ((in1 vec) (in2 vec)) (:out vec)
  (*! (zero) in1 in2))

(define-op /! ((out vec) (in1 vec) (in2 vec)) (:out vec)
  (with-components ((o out) (v1 in1) (v2 in2))
    (psetf ox (if (zerop v2x) 0f0 (cl:/ v1x v2x))
           oy (if (zerop v2y) 0f0 (cl:/ v1y v2y))
           oz (if (zerop v2z) 0f0 (cl:/ v1z v2z))
           ow (if (zerop v2w) 0f0 (cl:/ v1w v2w))))
  out)

(define-op / ((in1 vec) (in2 vec)) (:out vec)
  (/! (zero) in1 in2))

(defmacro %scale (ox oy oz ow x y z w scalar)
  `(psetf ,ox (cl:* ,x ,scalar)
          ,oy (cl:* ,y ,scalar)
          ,oz (cl:* ,z ,scalar)
          ,ow (cl:* ,w ,scalar)))

(define-op scale! ((out vec) (in vec) (scalar single-float)) (:out vec)
  (with-components ((o out) (v in))
    (%scale ox oy oz ow vx vy vz vw scalar))
  out)

(define-op scale ((in vec) (scalar single-float)) (:out vec)
  (scale! (zero) in scalar))

(defmacro %dot (v1x v1y v1z v1w v2x v2y v2z v2w)
  `(cl:+ (cl:* ,v1x ,v2x) (cl:* ,v1y ,v2y) (cl:* ,v1z ,v2z) (cl:* ,v1w ,v2w)))

(define-op dot ((in1 vec) (in2 vec)) (:out single-float)
  (with-components ((v1 in1) (v2 in2))
    (%dot v1x v1y v1z v1w v2x v2y v2z v2w)))

(defmacro %length-squared (x y z w)
  ;; NOTE: This is not using %DOT because using * instead of EXPT and SBCL 1.5.9
  ;; cannot correctly infer the type of the SQRT of the sum of squares as being
  ;; a single-float. This is because SBCL's memory model policy is "everything
  ;; is volatile", which is acceptable because two AREF calls to the same array
  ;; may infact produce different values when threading is involved.
  `(cl:+ (cl:expt ,x 2) (cl:expt ,y 2) (cl:expt ,z 2) (cl:expt ,w 2)))

(define-op length-squared ((in vec)) (:out single-float)
  (with-components ((v in))
    (%length-squared vx vy vz vw)))

(defmacro %length (x y z w)
  `(cl:sqrt (%length-squared ,x ,y ,z ,w)))

(define-op length ((in vec)) (:out single-float)
  (cl:sqrt (length-squared in)))

(define-op distance-squared ((in1 vec) (in2 vec)) (:out single-float)
  (length-squared (- in2 in1)))

(define-op distance ((in1 vec) (in2 vec)) (:out single-float)
  (cl:sqrt (distance-squared in1 in2)))

(defmacro %normalize (ox oy oz ow x y z w)
  (a:with-gensyms (length)
    `(let ((,length (%length ,x ,y ,z ,w)))
       (unless (zerop ,length)
         (%scale ,ox ,oy ,oz ,ow ,x ,y ,z ,w (cl:/ ,length))))))

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
           oy (fround vy)
           oz (fround vz)
           ow (fround vw)))
  out)

(define-op round ((in vec)) (:out vec)
  (round! (zero) in))

(define-op abs! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (cl:abs vx)
           oy (cl:abs vy)
           oz (cl:abs vz)
           ow (cl:abs vw)))
  out)

(define-op abs ((in vec)) (:out vec)
  (abs! (zero) in))

(define-op negate! ((out vec) (in vec)) (:out vec)
  (scale! out in -1f0))

(define-op negate ((in vec)) (:out vec)
  (negate! (zero) in))

(define-op angle ((in1 vec) (in2 vec)) (:out single-float :speed nil)
  (let ((dot (dot in1 in2))
        (m*m (cl:* (length in1) (length in2))))
    (if (zerop m*m) 0f0 (cl:acos (cl:/ dot m*m)))))

(define-op lerp! ((out vec) (in1 vec) (in2 vec) (factor single-float))
    (:out vec)
  (with-components ((o out) (v1 in1) (v2 in2))
    (psetf ox (a:lerp factor v1x v2x)
           oy (a:lerp factor v1y v2y)
           oz (a:lerp factor v1z v2z)
           ow (a:lerp factor v1w v2w)))
  out)

(define-op lerp ((in1 vec) (in2 vec) (factor single-float)) (:out vec)
  (lerp! (zero) in1 in2 factor))

(define-op < ((in1 vec) (in2 vec)) (:out boolean)
  (with-components ((v1 in1) (v2 in2))
    (and (cl:< v1x v2x)
         (cl:< v1y v2y)
         (cl:< v1z v2z)
         (cl:< v1w v2w))))

(define-op <= ((in1 vec) (in2 vec)) (:out boolean)
  (with-components ((v1 in1) (v2 in2))
    (and (cl:<= v1x v2x)
         (cl:<= v1y v2y)
         (cl:<= v1z v2z)
         (cl:<= v1w v2w))))

(define-op > ((in1 vec) (in2 vec)) (:out boolean)
  (with-components ((v1 in1) (v2 in2))
    (and (cl:> v1x v2x)
         (cl:> v1y v2y)
         (cl:> v1z v2z)
         (cl:> v1w v2w))))

(define-op >= ((in1 vec) (in2 vec)) (:out boolean)
  (with-components ((v1 in1) (v2 in2))
    (and (cl:>= v1x v2x)
         (cl:>= v1y v2y)
         (cl:>= v1z v2z)
         (cl:>= v1w v2w))))

(define-op min! ((out vec) (in1 vec) (in2 vec)) (:out vec)
  (with-components ((o out) (v1 in1) (v2 in2))
    (psetf ox (cl:min v1x v2x)
           oy (cl:min v1y v2y)
           oz (cl:min v1z v2z)
           ow (cl:min v1w v2w)))
  out)

(define-op min ((in1 vec) (in2 vec)) (:out vec)
  (min! (zero) in1 in2))

(define-op max! ((out vec) (in1 vec) (in2 vec)) (:out vec)
  (with-components ((o out) (v1 in1) (v2 in2))
    (psetf ox (cl:max v1x v2x)
           oy (cl:max v1y v2y)
           oz (cl:max v1z v2z)
           ow (cl:max v1w v2w)))
  out)

(define-op max ((in1 vec) (in2 vec)) (:out vec)
  (max! (zero) in1 in2))

(define-op radians! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (cl:* vx origin:+deg+)
           oy (cl:* vy origin:+deg+)
           oz (cl:* vz origin:+deg+)
           ow (cl:* vw origin:+deg+))
    out))

(define-op radians ((in vec)) (:out vec)
  (radians! (zero) in))

(define-op degrees! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (cl:* vx origin:+rad+)
           oy (cl:* vy origin:+rad+)
           oz (cl:* vz origin:+rad+)
           ow (cl:* vw origin:+rad+)))
  out)

(define-op degrees ((in vec)) (:out vec)
  (degrees! (zero) in))

(define-op expt! ((out vec) (in vec) (power real)) (:out vec :speed nil)
  (with-components ((o out) (v in))
    (psetf ox (cl:expt vx power)
           oy (cl:expt vy power)
           oz (cl:expt vz power)
           ow (cl:expt vw power)))
  out)

(define-op expt ((in vec) (power real)) (:out vec :speed nil)
  (expt! (zero) in power))

(define-op sqrt! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (check-type vx (single-float 0f0))
    (check-type vy (single-float 0f0))
    (check-type vz (single-float 0f0))
    (check-type vw (single-float 0f0))
    (psetf ox (cl:sqrt (the (single-float 0f0) vx))
           oy (cl:sqrt (the (single-float 0f0) vy))
           oz (cl:sqrt (the (single-float 0f0) vz))
           ow (cl:sqrt (the (single-float 0f0) vw))))
  out)

(define-op sqrt ((in vec)) (:out vec)
  (sqrt! (zero) in))

(define-op floor! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (ffloor vx)
           oy (ffloor vy)
           oz (ffloor vz)
           ow (ffloor vw)))
  out)

(define-op floor ((in vec)) (:out vec)
  (floor! (zero) in))

(define-op ceiling! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (fceiling vx)
           oy (fceiling vy)
           oz (fceiling vz)
           ow (fceiling vw)))
  out)

(define-op ceiling ((in vec)) (:out vec)
  (ceiling! (zero) in))

(define-op mod! ((out vec) (in vec) (divisor real)) (:out vec :speed nil)
  (with-components ((o out) (v in))
    (psetf ox (cl:mod vx divisor)
           oy (cl:mod vy divisor)
           oz (cl:mod vz divisor)
           ow (cl:mod vw divisor)))
  out)

(define-op mod ((in vec) (divisor real)) (:out vec :speed nil)
  (mod! (zero) in divisor))

(define-op sin! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (cl:sin vx)
           oy (cl:sin vy)
           oz (cl:sin vz)
           ow (cl:sin vw)))
  out)

(define-op sin ((in vec)) (:out vec)
  (sin! (zero) in))

(define-op cos! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (cl:cos vx)
           oy (cl:cos vy)
           oz (cl:cos vz)
           ow (cl:cos vw)))
  out)

(define-op cos ((in vec)) (:out vec)
  (cos! (zero) in))

(define-op tan! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (cl:tan vx)
           oy (cl:tan vy)
           oz (cl:tan vz)
           ow (cl:tan vw)))
  out)

(define-op tan ((in vec)) (:out vec)
  (tan! (zero) in))

(define-op asin! ((out vec) (in vec)) (:out vec :speed nil)
  (with-components ((o out) (v in))
    (psetf ox (cl:asin vx)
           oy (cl:asin vy)
           oz (cl:asin vz)
           ow (cl:asin vw)))
  out)

(define-op asin ((in vec)) (:out vec :speed nil)
  (asin! (zero) in))

(define-op acos! ((out vec) (in vec)) (:out vec :speed nil)
  (with-components ((o out) (v in))
    (psetf ox (cl:acos vx)
           oy (cl:acos vy)
           oz (cl:acos vz)
           ow (cl:acos vw)))
  out)

(define-op acos ((in vec)) (:out vec :speed nil)
  (acos! (zero) in))

(define-op atan! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (cl:atan vx)
           oy (cl:atan vy)
           oz (cl:atan vz)
           ow (cl:atan vw)))
  out)

(define-op atan ((in vec)) (:out vec)
  (atan! (zero) in))
