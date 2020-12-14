(in-package #:net.mfiano.lisp.origin.dvec4)

;;; accessors

(int:define-op x ((vec vec)) (:out double-float :speed nil)
  (aref vec 0))

(int:define-op (setf x) ((value double-float) (vec vec))
    (:out double-float :speed nil)
  (setf (aref vec 0) value))

(int:define-op y ((vec vec)) (:out double-float :speed nil)
  (aref vec 1))

(int:define-op (setf y) ((value double-float) (vec vec))
    (:out double-float :speed nil)
  (setf (aref vec 1) value))

(int:define-op z ((vec vec)) (:out double-float :speed nil)
  (aref vec 2))

(int:define-op (setf z) ((value double-float) (vec vec))
    (:out double-float :speed nil)
  (setf (aref vec 2) value))

(int:define-op w ((vec vec)) (:out double-float :speed nil)
  (aref vec 3))

(int:define-op (setf w) ((value double-float) (vec vec))
    (:out double-float :speed nil)
  (setf (aref vec 3) value))

;;; constructors

(int:define-op %vec (&rest (args double-float)) (:inline t :out vec)
  (make-array 4 :element-type 'double-float :initial-contents args))

(ss:defstore vec (&rest args))

(ss:defspecialization (vec :inline t) () vec
  (%vec 0d0 0d0 0d0 0d0))

(ss:defspecialization (vec :inline t) ((x real)) vec
  (%vec (float x 1d0) (float x 1d0) (float x 1d0) (float x 1d0)))

(ss:defspecialization (vec :inline t) ((xy dv2:vec)) vec
  (dv2:with-components ((v xy))
    (%vec vx vy 0d0 0d0)))

(ss:defspecialization (vec :inline t) ((xyz dv3:vec)) vec
  (dv3:with-components ((v xyz))
    (%vec vx vy vz 0d0)))

(ss:defspecialization (vec :inline t) ((xyzw vec)) vec
  (with-components ((v xyzw))
    (%vec vx vy vz vw)))

(ss:defspecialization (vec :inline t) ((x real) (y real)) vec
  (%vec (float x 1d0) (float y 1d0) 0d0 0d0))

(ss:defspecialization (vec :inline t) ((xy dv2:vec) (z real)) vec
  (dv2:with-components ((v xy))
    (%vec vx vy (float z 1d0) 0d0)))

(ss:defspecialization (vec :inline t) ((x real) (yz dv2:vec)) vec
  (dv2:with-components ((v yz))
    (%vec (float x 1d0) vx vy 0d0)))

(ss:defspecialization (vec :inline t) ((xy dv2:vec) (zw dv2:vec)) vec
  (dv2:with-components ((v1 xy) (v2 zw))
    (%vec v1x v1y v2x v2y)))

(ss:defspecialization (vec :inline t) ((x real) (yzw dv3:vec)) vec
  (dv3:with-components ((v yzw))
    (%vec (float x 1d0) vx vy vz)))

(ss:defspecialization (vec :inline t) ((xyz dv3:vec) (w real)) vec
  (dv3:with-components ((v xyz))
    (%vec vx vy vz (float w 1d0))))

(ss:defspecialization (vec :inline t) ((x real) (y real) (z real)) vec
  (%vec (float x 1d0) (float y 1d0) (float z 1d0) 0d0))

(ss:defspecialization (vec :inline t) ((xy dv2:vec) (z real) (w real)) vec
  (dv2:with-components ((v xy))
    (%vec vx vy (float z 1d0) (float w 1d0))))

(ss:defspecialization (vec :inline t) ((x real) (y real) (zw dv2:vec)) vec
  (dv2:with-components ((v zw))
    (%vec (float x 1d0) (float y 1d0) vx vy)))

(ss:defspecialization (vec :inline t) ((x real) (yz dv2:vec) (w real)) vec
  (dv2:with-components ((v yz))
    (%vec (float x 1d0) vx vy (float w 1d0))))

(ss:defspecialization (vec :inline t) ((x real) (y real) (z real) (w real)) vec
  (%vec (float x 1d0) (float y 1d0) (float z 1d0) (float w 1d0)))

(ss:defspecialization (vec :inline t) ((wxyz net.mfiano.lisp.origin.dquat:quat))
    vec
  (net.mfiano.lisp.origin.dquat:with-components ((q wxyz))
    (%vec qw qx qy qz)))

(ss:defspecialization (vec :inline t) ((xyzw v4:vec)) vec
  (v4:with-components ((v xyzw))
    (%vec (float vx 1d0) (float vy 1d0) (float vz 1d0) (float vw 1d0))))

;;; constants

(u:define-constant +zero+ (%vec 0d0 0d0 0d0 0d0) :test #'equalp)

;;; operators

(int:define-op zero! ((in vec)) (:out vec)
  (with-components ((v in))
    (setf vx 0d0 vy 0d0 vz 0d0 vw 0d0))
  in)

(int:define-op zero-p ((in vec)) (:out boolean)
  (with-components ((v in))
    (cl:= 0d0 vx vy vz)))

(int:define-op random! ((out vec) (min double-float) (max double-float))
    (:out vec)
  (let ((diff (cl:- max min)))
    (with-components ((o out))
      (setf ox (cl:+ min (cl:random diff))
            oy (cl:+ min (cl:random diff))
            oz (cl:+ min (cl:random diff))
            ow (cl:+ min (cl:random diff)))))
  out)

(int:define-op random ((min double-float) (max double-float)) (:out vec)
  (random! (vec) min max))

(int:define-op copy! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (setf ox vx oy vy oz vz ow vw))
  out)

(int:define-op copy ((in vec)) (:out vec)
  (copy! (vec) in))

(int:define-op sign! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (setf ox (signum vx)
          oy (signum vy)
          oz (signum vz)
          ow (signum vw)))
  out)

(int:define-op sign ((in vec)) (:out vec)
  (sign! (vec) in))

(int:define-op fract! ((out vec) (in vec)) (:out vec :speed nil)
  (with-components ((o out) (v in))
    (setf ox (cl:- vx (ffloor vx))
          oy (cl:- vy (ffloor vy))
          oz (cl:- vz (ffloor vz))
          ow (cl:- vw (ffloor vw))))
  out)

(int:define-op fract ((in vec)) (:out vec :speed nil)
  (fract! (vec) in))

(int:define-op clamp! ((out vec) (in vec)
                       &key
                       (min double-float most-negative-double-float)
                       (max double-float most-positive-double-float))
    (:out vec)
  (with-components ((o out) (v in))
    (setf ox (u:clamp vx min max)
          oy (u:clamp vy min max)
          oz (u:clamp vz min max)
          ow (u:clamp vw min max)))
  out)

(int:define-op clamp ((in vec)
                      &key
                      (min double-float most-negative-double-float)
                      (max double-float most-positive-double-float))
    (:out vec)
  (clamp! (vec) in :min min :max max))

(int:define-op = ((in1 vec) (in2 vec)
                  &key (rel double-float 1d-7) (abs double-float rel))
    (:out boolean)
  (with-components ((v1 in1) (v2 in2))
    (and (cl:<= (cl:abs (cl:- v1x v2x))
                (cl:max abs (cl:* rel (cl:max (cl:abs v1x) (cl:abs v2x)))))
         (cl:<= (cl:abs (cl:- v1y v2y))
                (cl:max abs (cl:* rel (cl:max (cl:abs v1y) (cl:abs v2y)))))
         (cl:<= (cl:abs (cl:- v1z v2z))
                (cl:max abs (cl:* rel (cl:max (cl:abs v1z) (cl:abs v2z)))))
         (cl:<= (cl:abs (cl:- v1w v2w))
                (cl:max abs (cl:* rel (cl:max (cl:abs v1w) (cl:abs v2w))))))))

(int:define-op +! ((out vec) (in1 vec) (in2 vec)) (:out vec)
  (with-components ((o out) (v1 in1) (v2 in2))
    (setf ox (cl:+ v1x v2x)
          oy (cl:+ v1y v2y)
          oz (cl:+ v1z v2z)
          ow (cl:+ v1w v2w)))
  out)

(int:define-op + ((in1 vec) (in2 vec)) (:out vec)
  (+! (vec) in1 in2))

(int:define-op -! ((out vec) (in1 vec) (in2 vec)) (:out vec)
  (with-components ((o out) (v1 in1) (v2 in2))
    (setf ox (cl:- v1x v2x)
          oy (cl:- v1y v2y)
          oz (cl:- v1z v2z)
          ow (cl:- v1w v2w)))
  out)

(int:define-op - ((in1 vec) (in2 vec)) (:out vec)
  (-! (vec) in1 in2))

(int:define-op *! ((out vec) (in1 vec) (in2 vec)) (:out vec)
  (with-components ((o out) (v1 in1) (v2 in2))
    (setf ox (cl:* v1x v2x)
          oy (cl:* v1y v2y)
          oz (cl:* v1z v2z)
          ow (cl:* v1w v2w)))
  out)

(int:define-op * ((in1 vec) (in2 vec)) (:out vec)
  (*! (vec) in1 in2))

(int:define-op /! ((out vec) (in1 vec) (in2 vec)) (:out vec)
  (with-components ((o out) (v1 in1) (v2 in2))
    (setf ox (if (zerop v2x) 0d0 (cl:/ v1x v2x))
          oy (if (zerop v2y) 0d0 (cl:/ v1y v2y))
          oz (if (zerop v2z) 0d0 (cl:/ v1z v2z))
          ow (if (zerop v2w) 0d0 (cl:/ v1w v2w))))
  out)

(int:define-op / ((in1 vec) (in2 vec)) (:out vec)
  (/! (vec) in1 in2))

(defmacro %scale (ox oy oz ow x y z w scalar)
  `(setf ,ox (cl:* ,x ,scalar)
         ,oy (cl:* ,y ,scalar)
         ,oz (cl:* ,z ,scalar)
         ,ow (cl:* ,w ,scalar)))

(int:define-op scale! ((out vec) (in vec) (scalar double-float)) (:out vec)
  (with-components ((o out) (v in))
    (%scale ox oy oz ow vx vy vz vw scalar))
  out)

(int:define-op scale ((in vec) (scalar double-float)) (:out vec)
  (scale! (vec) in scalar))

(int:define-op invert! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (setf ox (if (zerop vx) 0d0 (cl:/ vx))
          oy (if (zerop vy) 0d0 (cl:/ vy))
          oz (if (zerop vz) 0d0 (cl:/ vz))
          ow (if (zerop vw) 0d0 (cl:/ vw)))
    out))

(int:define-op invert ((in vec)) (:out vec)
  (invert! (vec) in))

(defmacro %dot (v1x v1y v1z v1w v2x v2y v2z v2w)
  `(cl:+ (cl:* ,v1x ,v2x) (cl:* ,v1y ,v2y) (cl:* ,v1z ,v2z) (cl:* ,v1w ,v2w)))

(int:define-op dot ((in1 vec) (in2 vec)) (:out double-float :speed nil)
  (with-components ((v1 in1) (v2 in2))
    (%dot v1x v1y v1z v1w v2x v2y v2z v2w)))

(defmacro %length-squared (x y z w)
  ;; NOTE: This is not using %DOT because using * instead of EXPT and SBCL 1.5.9
  ;; cannot correctly infer the type of the SQRT of the sum of squares as being
  ;; a single-float. This is because SBCL's memory model policy is "everything
  ;; is volatile", which is acceptable because two AREF calls to the same array
  ;; may infact produce different values when threading is involved.
  `(cl:+ (cl:expt ,x 2) (cl:expt ,y 2) (cl:expt ,z 2) (cl:expt ,w 2)))

(int:define-op length-squared ((in vec)) (:out double-float :speed nil)
  (with-components ((v in))
    (%length-squared vx vy vz vw)))

(defmacro %length (x y z w)
  `(cl:sqrt (%length-squared ,x ,y ,z ,w)))

(int:define-op length ((in vec)) (:out double-float :speed nil)
  (cl:sqrt (length-squared in)))

(int:define-op distance-squared ((in1 vec) (in2 vec))
    (:out double-float :speed nil)
  (length-squared (- in2 in1)))

(int:define-op distance ((in1 vec) (in2 vec)) (:out double-float :speed nil)
  (cl:sqrt (distance-squared in1 in2)))

(defmacro %normalize (ox oy oz ow x y z w)
  (u:with-gensyms (length inv-length)
    `(let ((,length (%length ,x ,y ,z ,w)))
       (unless (zerop ,length)
         (let ((,inv-length (cl:/ ,length)))
           (%scale ,ox ,oy ,oz ,ow ,x ,y ,z ,w ,inv-length))))))

(int:define-op normalize! ((out vec) (in vec)) (:out vec)
  (let ((length (length in)))
    (unless (zerop length)
      (scale! out in (cl:/ length))))
  out)

(int:define-op normalize ((in vec)) (:out vec)
  (normalize! (vec) in))

(int:define-op round! ((out vec) (in vec)) (:out vec :speed nil)
  (with-components ((o out) (v in))
    (setf ox (fround vx)
          oy (fround vy)
          oz (fround vz)
          ow (fround vw)))
  out)

(int:define-op round ((in vec)) (:out vec :speed nil)
  (round! (vec) in))

(int:define-op abs! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (setf ox (cl:abs vx)
          oy (cl:abs vy)
          oz (cl:abs vz)
          ow (cl:abs vw)))
  out)

(int:define-op abs ((in vec)) (:out vec)
  (abs! (vec) in))

(int:define-op negate! ((out vec) (in vec)) (:out vec)
  (scale! out in -1d0))

(int:define-op negate ((in vec)) (:out vec)
  (negate! (vec) in))

(int:define-op angle ((in1 vec) (in2 vec)) (:out double-float :speed nil)
  (let ((dot (dot in1 in2))
        (m*m (cl:* (length in1) (length in2))))
    (if (zerop m*m) 0d0 (cl:acos (cl:/ dot m*m)))))

(int:define-op lerp! ((out vec) (in1 vec) (in2 vec) (factor double-float))
    (:out vec)
  (with-components ((o out) (v1 in1) (v2 in2))
    (setf ox (u:lerp factor v1x v2x)
          oy (u:lerp factor v1y v2y)
          oz (u:lerp factor v1z v2z)
          ow (u:lerp factor v1w v2w)))
  out)

(int:define-op lerp ((in1 vec) (in2 vec) (factor double-float)) (:out vec)
  (lerp! (vec) in1 in2 factor))

(int:define-op < ((in1 vec) (in2 vec)) (:out boolean)
  (with-components ((v1 in1) (v2 in2))
    (and (cl:< v1x v2x)
         (cl:< v1y v2y)
         (cl:< v1z v2z)
         (cl:< v1w v2w))))

(int:define-op <= ((in1 vec) (in2 vec)) (:out boolean)
  (with-components ((v1 in1) (v2 in2))
    (and (cl:<= v1x v2x)
         (cl:<= v1y v2y)
         (cl:<= v1z v2z)
         (cl:<= v1w v2w))))

(int:define-op > ((in1 vec) (in2 vec)) (:out boolean)
  (with-components ((v1 in1) (v2 in2))
    (and (cl:> v1x v2x)
         (cl:> v1y v2y)
         (cl:> v1z v2z)
         (cl:> v1w v2w))))

(int:define-op >= ((in1 vec) (in2 vec)) (:out boolean)
  (with-components ((v1 in1) (v2 in2))
    (and (cl:>= v1x v2x)
         (cl:>= v1y v2y)
         (cl:>= v1z v2z)
         (cl:>= v1w v2w))))

(int:define-op min! ((out vec) (in1 vec) (in2 vec)) (:out vec)
  (with-components ((o out) (v1 in1) (v2 in2))
    (setf ox (cl:min v1x v2x)
          oy (cl:min v1y v2y)
          oz (cl:min v1z v2z)
          ow (cl:min v1w v2w)))
  out)

(int:define-op min ((in1 vec) (in2 vec)) (:out vec)
  (min! (vec) in1 in2))

(int:define-op max! ((out vec) (in1 vec) (in2 vec)) (:out vec)
  (with-components ((o out) (v1 in1) (v2 in2))
    (setf ox (cl:max v1x v2x)
          oy (cl:max v1y v2y)
          oz (cl:max v1z v2z)
          ow (cl:max v1w v2w)))
  out)

(int:define-op max ((in1 vec) (in2 vec)) (:out vec)
  (max! (vec) in1 in2))

(int:define-op radians! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (setf ox (cl:* vx const:+deg/double+)
          oy (cl:* vy const:+deg/double+)
          oz (cl:* vz const:+deg/double+)
          ow (cl:* vw const:+deg/double+))
    out))

(int:define-op radians ((in vec)) (:out vec)
  (radians! (vec) in))

(int:define-op degrees! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (setf ox (cl:* vx const:+rad/double+)
          oy (cl:* vy const:+rad/double+)
          oz (cl:* vz const:+rad/double+)
          ow (cl:* vw const:+rad/double+)))
  out)

(int:define-op degrees ((in vec)) (:out vec)
  (degrees! (vec) in))

(int:define-op expt! ((out vec) (in vec) (power real)) (:out vec :speed nil)
  (with-components ((o out) (v in))
    (setf ox (cl:expt vx power)
          oy (cl:expt vy power)
          oz (cl:expt vz power)
          ow (cl:expt vw power)))
  out)

(int:define-op expt ((in vec) (power real)) (:out vec :speed nil)
  (expt! (vec) in power))

(int:define-op sqrt! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (setf ox (cl:sqrt (the (double-float 0d0) vx))
          oy (cl:sqrt (the (double-float 0d0) vy))
          oz (cl:sqrt (the (double-float 0d0) vz))
          ow (cl:sqrt (the (double-float 0d0) vw))))
  out)

(int:define-op sqrt ((in vec)) (:out vec)
  (sqrt! (vec) in))

(int:define-op floor! ((out vec) (in vec)) (:out vec :speed nil)
  (with-components ((o out) (v in))
    (setf ox (ffloor vx)
          oy (ffloor vy)
          oz (ffloor vz)
          ow (ffloor vw)))
  out)

(int:define-op floor ((in vec)) (:out vec :speed nil)
  (floor! (vec) in))

(int:define-op ceiling! ((out vec) (in vec)) (:out vec :speed nil)
  (with-components ((o out) (v in))
    (setf ox (fceiling vx)
          oy (fceiling vy)
          oz (fceiling vz)
          ow (fceiling vw)))
  out)

(int:define-op ceiling ((in vec)) (:out vec :speed nil)
  (ceiling! (vec) in))

(int:define-op mod! ((out vec) (in vec) (divisor real)) (:out vec :speed nil)
  (with-components ((o out) (v in))
    (setf ox (cl:mod vx divisor)
          oy (cl:mod vy divisor)
          oz (cl:mod vz divisor)
          ow (cl:mod vw divisor)))
  out)

(int:define-op mod ((in vec) (divisor real)) (:out vec :speed nil)
  (mod! (vec) in divisor))

(int:define-op sin! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (setf ox (cl:sin vx)
          oy (cl:sin vy)
          oz (cl:sin vz)
          ow (cl:sin vw)))
  out)

(int:define-op sin ((in vec)) (:out vec)
  (sin! (vec) in))

(int:define-op cos! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (setf ox (cl:cos vx)
          oy (cl:cos vy)
          oz (cl:cos vz)
          ow (cl:cos vw)))
  out)

(int:define-op cos ((in vec)) (:out vec)
  (cos! (vec) in))

(int:define-op tan! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (setf ox (cl:tan vx)
          oy (cl:tan vy)
          oz (cl:tan vz)
          ow (cl:tan vw)))
  out)

(int:define-op tan ((in vec)) (:out vec)
  (tan! (vec) in))

(int:define-op asin! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (setf ox (cl:asin (the (double-float -1d0 1d0) vx))
          oy (cl:asin (the (double-float -1d0 1d0) vy))
          oz (cl:asin (the (double-float -1d0 1d0) vz))
          ow (cl:asin (the (double-float -1d0 1d0) vw))))
  out)

(int:define-op asin ((in vec)) (:out vec)
  (asin! (vec) in))

(int:define-op acos! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (setf ox (cl:acos (the (double-float -1d0 1d0) vx))
          oy (cl:acos (the (double-float -1d0 1d0) vy))
          oz (cl:acos (the (double-float -1d0 1d0) vz))
          ow (cl:acos (the (double-float -1d0 1d0) vw))))
  out)

(int:define-op acos ((in vec)) (:out vec)
  (acos! (vec) in))

(int:define-op atan! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (setf ox (cl:atan vx)
          oy (cl:atan vy)
          oz (cl:atan vz)
          ow (cl:atan vw)))
  out)

(int:define-op atan ((in vec)) (:out vec)
  (atan! (vec) in))
