(in-package #:net.mfiano.lisp.origin.vec3)

;;; accessors

(int:define-op x ((vec vec)) (:out single-float)
  (aref vec 0))

(int:define-op (setf x) ((value single-float) (vec vec)) (:out single-float)
  (setf (aref vec 0) value))

(int:define-op y ((vec vec)) (:out single-float)
  (aref vec 1))

(int:define-op (setf y) ((value single-float) (vec vec)) (:out single-float)
  (setf (aref vec 1) value))

(int:define-op z ((vec vec)) (:out single-float)
  (aref vec 2))

(int:define-op (setf z) ((value single-float) (vec vec)) (:out single-float)
  (setf (aref vec 2) value))

;;; constructors

(int:define-op %vec (&rest (args single-float)) (:inline t :out vec)
  (make-array 3 :element-type 'single-float :initial-contents args))

(ss:defstore vec (&rest args))

(ss:defspecialization (vec :inline t) () vec
  (%vec 0f0 0f0 0f0))

(ss:defspecialization (vec :inline t) ((x real)) vec
  (%vec (float x 1f0) (float x 1f0) (float x 1f0)))

(ss:defspecialization (vec :inline t) ((xy v2:vec)) vec
  (v2:with-components ((v xy))
    (%vec vx vy 0f0)))

(ss:defspecialization (vec :inline t) ((xyz vec)) vec
  (with-components ((v xyz))
    (%vec vx vy vz)))

(ss:defspecialization (vec :inline t) ((xyzw net.mfiano.lisp.origin.vec4:vec))
    vec
  (net.mfiano.lisp.origin.vec4:with-components ((v xyzw))
    (%vec vx vy vz)))

(ss:defspecialization (vec :inline t) ((x real) (y real)) vec
  (%vec (float x 1f0) (float y 1f0) 0f0))

(ss:defspecialization (vec :inline t) ((xy v2:vec) (z real)) vec
  (v2:with-components ((v xy))
    (%vec vx vy (float z 1f0))))

(ss:defspecialization (vec :inline t) ((x real) (yz v2:vec)) vec
  (v2:with-components ((v yz))
    (%vec (float x 1f0) vx vy)))

(ss:defspecialization (vec :inline t) ((x real) (y real) (z real)) vec
  (%vec (float x 1f0) (float y 1f0) (float z 1f0)))

(ss:defspecialization (vec :inline t) ((xyz net.mfiano.lisp.origin.dvec3:vec))
    vec
  (net.mfiano.lisp.origin.dvec3:with-components ((v xyz))
    (%vec (float vx 1f0) (float vy 1f0) (float vz 1f0))))

;;; constants

(u:define-constant +zero+ (%vec 0f0 0f0 0f0) :test #'equalp)

(u:define-constant +up+ (%vec 0f0 1f0 0f0) :test #'equalp)

(u:define-constant +down+ (%vec 0f0 -1f0 0f0) :test #'equalp)

(u:define-constant +left+ (%vec -1f0 0f0 0f0) :test #'equalp)

(u:define-constant +right+ (%vec 1f0 0f0 0f0) :test #'equalp)

(u:define-constant +forward+ (%vec 0f0 0f0 1f0) :test #'equalp)

(u:define-constant +back+ (%vec 0f0 0f0 -1f0) :test #'equalp)

;;; operators

(int:define-op zero! ((in vec)) (:out vec)
  (with-components ((v in))
    (psetf vx 0f0 vy 0f0 vz 0f0))
  in)

(int:define-op zero-p ((in vec)) (:out boolean)
  (with-components ((v in))
    (cl:= 0f0 vx vy vz)))

(int:define-op random! ((out vec) (min single-float) (max single-float))
    (:out vec)
  (with-components ((o out))
    (psetf ox (cl:+ min (cl:random (cl:- max min)))
           oy (cl:+ min (cl:random (cl:- max min)))
           oz (cl:+ min (cl:random (cl:- max min)))))
  out)

(int:define-op random ((min single-float) (max single-float)) (:out vec)
  (random! (vec) min max))

(int:define-op copy! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox vx oy vy oz vz))
  out)

(int:define-op copy ((in vec)) (:out vec)
  (copy! (vec) in))

(int:define-op sign! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (signum vx)
           oy (signum vy)
           oz (signum vz)))
  out)

(int:define-op sign ((in vec)) (:out vec)
  (sign! (vec) in))

(int:define-op fract! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (cl:- vx (ffloor vx))
           oy (cl:- vy (ffloor vy))
           oz (cl:- vz (ffloor vz))))
  out)

(int:define-op fract ((in vec)) (:out vec)
  (fract! (vec) in))

(int:define-op clamp! ((out vec) (in vec)
                       &key
                       (min single-float most-negative-single-float)
                       (max single-float most-positive-single-float))
    (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (u:clamp vx min max)
           oy (u:clamp vy min max)
           oz (u:clamp vz min max)))
  out)

(int:define-op clamp ((in vec)
                      &key
                      (min single-float most-negative-single-float)
                      (max single-float most-positive-single-float))
    (:out vec)
  (clamp! (vec) in :min min :max max))

(int:define-op = ((in1 vec) (in2 vec)
                  &key (rel single-float 1e-7) (abs single-float rel))
    (:out boolean)
  (with-components ((v1 in1) (v2 in2))
    (and (cl:<= (cl:abs (cl:- v1x v2x))
                (cl:max abs (cl:* rel (cl:max (cl:abs v1x) (cl:abs v2x)))))
         (cl:<= (cl:abs (cl:- v1y v2y))
                (cl:max abs (cl:* rel (cl:max (cl:abs v1y) (cl:abs v2y)))))
         (cl:<= (cl:abs (cl:- v1z v2z))
                (cl:max abs (cl:* rel (cl:max (cl:abs v1z) (cl:abs v2z))))))))

(int:define-op +! ((out vec) (in1 vec) (in2 vec)) (:out vec)
  (with-components ((o out) (v1 in1) (v2 in2))
    (psetf ox (cl:+ v1x v2x)
           oy (cl:+ v1y v2y)
           oz (cl:+ v1z v2z)))
  out)

(int:define-op + ((in1 vec) (in2 vec)) (:out vec)
  (+! (vec) in1 in2))

(int:define-op -! ((out vec) (in1 vec) (in2 vec)) (:out vec)
  (with-components ((o out) (v1 in1) (v2 in2))
    (psetf ox (cl:- v1x v2x)
           oy (cl:- v1y v2y)
           oz (cl:- v1z v2z)))
  out)

(int:define-op - ((in1 vec) (in2 vec)) (:out vec)
  (-! (vec) in1 in2))

(int:define-op *! ((out vec) (in1 vec) (in2 vec)) (:out vec)
  (with-components ((o out) (v1 in1) (v2 in2))
    (psetf ox (cl:* v1x v2x)
           oy (cl:* v1y v2y)
           oz (cl:* v1z v2z)))
  out)

(int:define-op * ((in1 vec) (in2 vec)) (:out vec)
  (*! (vec) in1 in2))

(int:define-op /! ((out vec) (in1 vec) (in2 vec)) (:out vec)
  (with-components ((o out) (v1 in1) (v2 in2))
    (psetf ox (if (zerop v2x) 0f0 (cl:/ v1x v2x))
           oy (if (zerop v2y) 0f0 (cl:/ v1y v2y))
           oz (if (zerop v2z) 0f0 (cl:/ v1z v2z))))
  out)

(int:define-op / ((in1 vec) (in2 vec)) (:out vec)
  (/! (vec) in1 in2))

(defmacro %scale (ox oy oz x y z scalar)
  `(psetf ,ox (cl:* ,x ,scalar)
          ,oy (cl:* ,y ,scalar)
          ,oz (cl:* ,z ,scalar)))

(int:define-op scale! ((out vec) (in vec) (scalar single-float)) (:out vec)
  (with-components ((o out) (v in))
    (%scale ox oy oz vx vy vz scalar))
  out)

(int:define-op scale ((in vec) (scalar single-float)) (:out vec)
  (scale! (vec) in scalar))

(int:define-op invert! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (if (zerop vx) 0f0 (cl:/ vx))
           oy (if (zerop vy) 0f0 (cl:/ vy))
           oz (if (zerop vz) 0f0 (cl:/ vz)))
    out))

(int:define-op invert ((in vec)) (:out vec)
  (invert! (vec) in))

(defmacro %dot (v1x v1y v1z v2x v2y v2z)
  `(cl:+ (cl:* ,v1x ,v2x) (cl:* ,v1y ,v2y) (cl:* ,v1z ,v2z)))

(int:define-op dot ((in1 vec) (in2 vec)) (:out single-float)
  (with-components ((v1 in1) (v2 in2))
    (%dot v1x v1y v1z v2x v2y v2z)))

(defmacro %length-squared (x y z)
  ;; NOTE: This is not using %DOT because using * instead of EXPT and SBCL 1.5.9
  ;; cannot correctly infer the type of the SQRT of the sum of squares as being
  ;; a single-float. This is because SBCL's memory model policy is "everything
  ;; is volatile", which is acceptable because two AREF calls to the same array
  ;; may infact produce different values when threading is involved.
  `(cl:+ (cl:expt ,x 2) (cl:expt ,y 2) (cl:expt ,z 2)))

(int:define-op length-squared ((in vec)) (:out single-float)
  (with-components ((v in))
    (%length-squared vx vy vz)))

(defmacro %length (x y z)
  `(cl:sqrt (%length-squared ,x ,y ,z)))

(int:define-op length ((in vec)) (:out single-float)
  (cl:sqrt (length-squared in)))

(int:define-op distance-squared ((in1 vec) (in2 vec)) (:out single-float)
  (length-squared (- in2 in1)))

(int:define-op distance ((in1 vec) (in2 vec)) (:out single-float)
  (cl:sqrt (distance-squared in1 in2)))

(defmacro %normalize (ox oy oz x y z)
  (u:with-gensyms (length)
    `(let ((,length (%length ,x ,y ,z)))
       (unless (zerop ,length)
         (%scale ,ox ,oy ,oz ,x ,y ,z (cl:/ ,length))))))

(int:define-op normalize! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (%normalize ox oy oz vx vy vz))
  out)

(int:define-op normalize ((in vec)) (:out vec)
  (normalize! (vec) in))

(int:define-op round! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (fround vx)
           oy (fround vy)
           oz (fround vz)))
  out)

(int:define-op round ((in vec)) (:out vec)
  (round! (vec) in))

(int:define-op abs! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (cl:abs vx)
           oy (cl:abs vy)
           oz (cl:abs vz)))
  out)

(int:define-op abs ((in vec)) (:out vec)
  (abs! (vec) in))

(int:define-op negate! ((out vec) (in vec)) (:out vec)
  (scale! out in -1f0))

(int:define-op negate ((in vec)) (:out vec)
  (negate! (vec) in))

(int:define-op cross! ((out vec) (in1 vec) (in2 vec)) (:out vec)
  (with-components ((o out) (v1 in1) (v2 in2))
    (psetf ox (cl:- (cl:* v1y v2z) (cl:* v1z v2y))
           oy (cl:- (cl:* v1z v2x) (cl:* v1x v2z))
           oz (cl:- (cl:* v1x v2y) (cl:* v1y v2x))))
  out)

(int:define-op cross ((in1 vec) (in2 vec)) (:out vec)
  (cross! (vec) in1 in2))

(int:define-op box ((in1 vec) (in2 vec) (in3 vec)) (:out single-float)
  (dot (cross in1 in2) in3))

(int:define-op angle ((in1 vec) (in2 vec)) (:out single-float)
  (let ((dot (dot in1 in2))
        (m*m (cl:* (length in1) (length in2))))
    (if (zerop m*m)
        0f0
        (cl:acos (the (single-float -1f0 1f0) (cl:/ dot m*m))))))

(int:define-op direction= ((in1 vec) (in2 vec)) (:out boolean)
  (cl:>= (dot (normalize in1) (normalize in2)) (cl:- 1 1e-7)))

(int:define-op parallel-p ((in1 vec) (in2 vec)) (:out boolean)
  (= (cross in1 in2) +zero+))

(int:define-op lerp! ((out vec) (in1 vec) (in2 vec) (factor single-float))
    (:out vec)
  (with-components ((o out) (v1 in1) (v2 in2))
    (psetf ox (u:lerp factor v1x v2x)
           oy (u:lerp factor v1y v2y)
           oz (u:lerp factor v1z v2z)))
  out)

(int:define-op lerp ((in1 vec) (in2 vec) (factor single-float)) (:out vec)
  (lerp! (vec) in1 in2 factor))

(int:define-op < ((in1 vec) (in2 vec)) (:out boolean)
  (with-components ((v1 in1) (v2 in2))
    (and (cl:< v1x v2x)
         (cl:< v1y v2y)
         (cl:< v1z v2z))))

(int:define-op <= ((in1 vec) (in2 vec)) (:out boolean)
  (with-components ((v1 in1) (v2 in2))
    (and (cl:<= v1x v2x)
         (cl:<= v1y v2y)
         (cl:<= v1z v2z))))

(int:define-op > ((in1 vec) (in2 vec)) (:out boolean)
  (with-components ((v1 in1) (v2 in2))
    (and (cl:> v1x v2x)
         (cl:> v1y v2y)
         (cl:> v1z v2z))))

(int:define-op >= ((in1 vec) (in2 vec)) (:out boolean)
  (with-components ((v1 in1) (v2 in2))
    (and (cl:>= v1x v2x)
         (cl:>= v1y v2y)
         (cl:>= v1z v2z))))

(int:define-op min! ((out vec) (in1 vec) (in2 vec)) (:out vec)
  (with-components ((o out) (v1 in1) (v2 in2))
    (psetf ox (cl:min v1x v2x)
           oy (cl:min v1y v2y)
           oz (cl:min v1z v2z)))
  out)

(int:define-op min ((in1 vec) (in2 vec)) (:out vec)
  (min! (vec) in1 in2))

(int:define-op max! ((out vec) (in1 vec) (in2 vec)) (:out vec)
  (with-components ((o out) (v1 in1) (v2 in2))
    (psetf ox (cl:max v1x v2x)
           oy (cl:max v1y v2y)
           oz (cl:max v1z v2z)))
  out)

(int:define-op max ((in1 vec) (in2 vec)) (:out vec)
  (max! (vec) in1 in2))

(int:define-op radians! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (cl:* vx const:+deg+)
           oy (cl:* vy const:+deg+)
           oz (cl:* vz const:+deg+))
    out))

(int:define-op radians ((in vec)) (:out vec)
  (radians! (vec) in))

(int:define-op degrees! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (cl:* vx const:+rad+)
           oy (cl:* vy const:+rad+)
           oz (cl:* vz const:+rad+)))
  out)

(int:define-op degrees ((in vec)) (:out vec)
  (degrees! (vec) in))

(int:define-op expt! ((out vec) (in vec) (power real)) (:out vec :speed nil)
  (with-components ((o out) (v in))
    (psetf ox (cl:expt vx power)
           oy (cl:expt vy power)
           oz (cl:expt vz power)))
  out)

(int:define-op expt ((in vec) (power real)) (:out vec :speed nil)
  (expt! (vec) in power))

(int:define-op sqrt! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (check-type vx (single-float 0f0))
    (check-type vy (single-float 0f0))
    (check-type vz (single-float 0f0))
    (psetf ox (cl:sqrt (the (single-float 0f0) vx))
           oy (cl:sqrt (the (single-float 0f0) vy))
           oz (cl:sqrt (the (single-float 0f0) vz))))
  out)

(int:define-op sqrt ((in vec)) (:out vec)
  (sqrt! (vec) in))

(int:define-op floor! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (ffloor vx)
           oy (ffloor vy)
           oz (ffloor vz)))
  out)

(int:define-op floor ((in vec)) (:out vec)
  (floor! (vec) in))

(int:define-op ceiling! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (fceiling vx)
           oy (fceiling vy)
           oz (fceiling vz)))
  out)

(int:define-op ceiling ((in vec)) (:out vec)
  (ceiling! (vec) in))

(int:define-op mod! ((out vec) (in vec) (divisor single-float)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (nth-value 1 (ffloor vx divisor))
           oy (nth-value 1 (ffloor vy divisor))
           oz (nth-value 1 (ffloor vz divisor))))
  out)

(int:define-op mod ((in vec) (divisor real)) (:out vec)
  (mod! (vec) in divisor))

(int:define-op sin! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (cl:sin vx)
           oy (cl:sin vy)
           oz (cl:sin vz)))
  out)

(int:define-op sin ((in vec)) (:out vec)
  (sin! (vec) in))

(int:define-op cos! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (cl:cos vx)
           oy (cl:cos vy)
           oz (cl:cos vz)))
  out)

(int:define-op cos ((in vec)) (:out vec)
  (cos! (vec) in))

(int:define-op tan! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (cl:tan vx)
           oy (cl:tan vy)
           oz (cl:tan vz)))
  out)

(int:define-op tan ((in vec)) (:out vec)
  (tan! (vec) in))

(int:define-op asin! ((out vec) (in vec)) (:out vec :speed nil)
  (with-components ((o out) (v in))
    (psetf ox (cl:asin vx)
           oy (cl:asin vy)
           oz (cl:asin vz)))
  out)

(int:define-op asin ((in vec)) (:out vec :speed nil)
  (asin! (vec) in))

(int:define-op acos! ((out vec) (in vec)) (:out vec :speed nil)
  (with-components ((o out) (v in))
    (psetf ox (cl:acos vx)
           oy (cl:acos vy)
           oz (cl:acos vz)))
  out)

(int:define-op acos ((in vec)) (:out vec :speed nil)
  (acos! (vec) in))

(int:define-op atan! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (cl:atan vx)
           oy (cl:atan vy)
           oz (cl:atan vz)))
  out)

(int:define-op atan ((in vec)) (:out vec)
  (atan! (vec) in))

(int:define-op make-velocity! ((out vec) (axis vec) (rate single-float))
    (:out vec)
  "`AXIS` is a vec3 of any length. `RATE` is in units/second. Returns a velocity
vec3 following the right hand rule whose direction is parallel to `AXIS` and
magnitude is `RATE`. Destructively modifies `OUT`."
  (copy! out axis)
  (normalize! out out)
  (scale! out out rate))

(int:define-op make-velocity ((axis vec) (rate single-float)) (:out vec)
  "`AXIS` is a vec3 of any length. `RATE` is in units/second. Returns a velocity
vec3 following the right hand rule whose direction is parallel to `AXIS` and
magnitude is `RATE`. Allocates a fresh vec3."
  (make-velocity! (vec) axis rate))
