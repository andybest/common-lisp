(in-package #:net.mfiano.lisp.origin.dvec2)

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

;;; constructors

(int:define-op %vec (&rest (args double-float)) (:inline t :out vec)
  (make-array 2 :element-type 'double-float :initial-contents args))

(ss:defstore vec (&rest args))

(ss:defspecialization (vec :inline t) () vec
  (%vec 0d0 0d0))

(ss:defspecialization (vec :inline t) ((x real)) vec
  (%vec (float x 1d0) (float x 1d0)))

(ss:defspecialization (vec :inline t) ((xy vec)) vec
  (with-components ((v xy))
    (%vec vx vy)))

(ss:defspecialization (vec :inline t) ((xyz net.mfiano.lisp.origin.dvec3:vec))
    vec
  (net.mfiano.lisp.origin.dvec3:with-components ((v xyz))
    (%vec vx vy)))

(ss:defspecialization (vec :inline t) ((xyzw net.mfiano.lisp.origin.dvec4:vec))
    vec
  (net.mfiano.lisp.origin.dvec4:with-components ((v xyzw))
    (%vec vx vy)))

(ss:defspecialization (vec :inline t) ((x real) (y real)) vec
  (%vec (float x 1d0) (float y 1d0)))

(ss:defspecialization (vec :inline t) ((xy v2:vec)) vec
  (v2:with-components ((v xy))
    (%vec (float vx 1d0) (float vy 1d0))))

;;; constants

(u:define-constant +zero+ (%vec 0d0 0d0) :test #'equalp)

;;; operators

(int:define-op zero! ((in vec)) (:out vec)
  (with-components ((v in))
    (psetf vx 0d0 vy 0d0))
  in)

(int:define-op zero-p ((in vec)) (:out boolean)
  (with-components ((v in))
    (cl:= 0d0 vx vy)))

(int:define-op random! ((out vec)
                        &key (min double-float 0d0) (max double-float 1d0))
    (:out vec)
  (with-components ((o out))
    (psetf ox (cl:+ min (cl:random (cl:- max min)))
           oy (cl:+ min (cl:random (cl:- max min)))))
  out)

(int:define-op random ((min double-float) (max double-float))
    (:out vec)
  (random! (vec) :min min :max max))

(int:define-op copy! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox vx oy vy))
  out)

(int:define-op copy ((in vec)) (:out vec)
  (copy! (vec) in))

(int:define-op sign! ((out vec) (in vec)) (:out vec :speed nil)
  (with-components ((o out) (v in))
    (psetf ox (signum vx)
           oy (signum vy)))
  out)

(int:define-op sign ((in vec)) (:out vec :speed nil)
  (sign! (vec) in))

(int:define-op fract! ((out vec) (in vec)) (:out vec :speed nil)
  (with-components ((o out) (v in))
    (psetf ox (cl:- vx (ffloor vx))
           oy (cl:- vy (ffloor vy))))
  out)

(int:define-op fract ((in vec)) (:out vec :speed nil)
  (fract! (vec) in))

(int:define-op clamp! ((out vec) (in vec)
                       &key
                       (min double-float most-negative-double-float)
                       (max double-float most-positive-double-float))
    (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (u:clamp vx min max)
           oy (u:clamp vy min max)))
  out)

(int:define-op clamp ((in vec)
                      &key
                      (min double-float most-negative-double-float)
                      (max double-float most-positive-double-float))
    (:out vec)
  (clamp! (vec) in :min min :max max))

(int:define-op = ((in1 vec) (in2 vec)) (:out boolean)
  (with-components ((v1 in1) (v2 in2))
    (and (cl:= v1x v2x)
         (cl:= v1y v2y))))

(int:define-op ~ ((in1 vec) (in2 vec) &key (tolerance double-float 1d-7))
    (:out boolean :speed nil)
  (with-components ((v1 in1) (v2 in2))
    (and (cl:< (cl:abs (cl:- v1x v2x)) tolerance)
         (cl:< (cl:abs (cl:- v1y v2y)) tolerance))))

(int:define-op +! ((out vec) (in1 vec) (in2 vec)) (:out vec)
  (with-components ((o out) (v1 in1) (v2 in2))
    (psetf ox (cl:+ v1x v2x)
           oy (cl:+ v1y v2y)))
  out)

(int:define-op + ((in1 vec) (in2 vec)) (:out vec)
  (+! (vec) in1 in2))

(int:define-op -! ((out vec) (in1 vec) (in2 vec)) (:out vec)
  (with-components ((o out) (v1 in1) (v2 in2))
    (psetf ox (cl:- v1x v2x)
           oy (cl:- v1y v2y)))
  out)

(int:define-op - ((in1 vec) (in2 vec)) (:out vec)
  (-! (vec) in1 in2))

(int:define-op *! ((out vec) (in1 vec) (in2 vec)) (:out vec)
  (with-components ((o out) (v1 in1) (v2 in2))
    (psetf ox (cl:* v1x v2x)
           oy (cl:* v1y v2y)))
  out)

(int:define-op * ((in1 vec) (in2 vec)) (:out vec)
  (*! (vec) in1 in2))

(int:define-op /! ((out vec) (in1 vec) (in2 vec)) (:out vec)
  (with-components ((o out) (v1 in1) (v2 in2))
    (psetf ox (if (zerop v2x) 0d0 (cl:/ v1x v2x))
           oy (if (zerop v2y) 0d0 (cl:/ v1y v2y))))
  out)

(int:define-op / ((in1 vec) (in2 vec)) (:out vec)
  (/! (vec) in1 in2))

(defmacro %scale (ox oy x y scalar)
  `(psetf ,ox (cl:* ,x ,scalar)
          ,oy (cl:* ,y ,scalar)))

(int:define-op scale! ((out vec) (in vec) (scalar double-float)) (:out vec)
  (with-components ((o out) (v in))
    (%scale ox oy vx vy scalar))
  out)

(int:define-op scale ((in vec) (scalar double-float)) (:out vec)
  (scale! (vec) in scalar))

(int:define-op invert! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (if (zerop vx) 0d0 (cl:/ vx))
           oy (if (zerop vy) 0d0 (cl:/ vy)))
    out))

(int:define-op invert ((in vec)) (:out vec)
  (invert! (vec) in))

(defmacro %dot (v1x v1y v2x v2y)
  `(cl:+ (cl:* ,v1x ,v2x) (cl:* ,v1y ,v2y)))

(int:define-op dot ((in1 vec) (in2 vec)) (:out double-float :speed nil)
  (with-components ((v1 in1) (v2 in2))
    (%dot v1x v1y v2x v2y)))

(defmacro %length-squared (x y)
  ;; NOTE: This is not using %DOT because using * instead of EXPT and SBCL 1.5.9
  ;; cannot correctly infer the type of the SQRT of the sum of squares as being
  ;; a single-float. This is because SBCL's memory model policy is "everything
  ;; is volatile", which is acceptable because two AREF calls to the same array
  ;; may infact produce different values when threading is involved.
  `(cl:+ (cl:expt ,x 2) (cl:expt ,y 2)))

(int:define-op length-squared ((in vec)) (:out double-float :speed nil)
  (with-components ((v in))
    (%length-squared vx vy)))

(defmacro %length (x y)
  `(cl:sqrt (%length-squared ,x ,y)))

(int:define-op length ((in vec)) (:out double-float :speed nil)
  (cl:sqrt (length-squared in)))

(int:define-op distance-squared ((in1 vec) (in2 vec))
    (:out double-float :speed nil)
  (length-squared (- in2 in1)))

(int:define-op distance ((in1 vec) (in2 vec)) (:out double-float :speed nil)
  (cl:sqrt (distance-squared in1 in2)))

(defmacro %normalize (ox oy x y)
  (u:with-gensyms (length)
    `(let ((,length (%length ,x ,y)))
       (unless (zerop ,length)
         (%scale ,ox ,oy ,x ,y (cl:/ ,length))))))

(int:define-op normalize! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (%normalize ox oy vx vy))
  out)

(int:define-op normalize ((in vec)) (:out vec)
  (normalize! (vec) in))

(int:define-op round! ((out vec) (in vec)) (:out vec :speed nil)
  (with-components ((o out) (v in))
    (psetf ox (fround vx)
           oy (fround vy)))
  out)

(int:define-op round ((in vec)) (:out vec :speed nil)
  (round! (vec) in))

(int:define-op abs! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (cl:abs vx)
           oy (cl:abs vy)))
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

(int:define-op direction= ((in1 vec) (in2 vec)) (:out boolean)
  (cl:>= (dot (normalize in1) (normalize in2)) (cl:- 1 1d-7)))

(int:define-op lerp! ((out vec) (in1 vec) (in2 vec) (factor double-float))
    (:out vec)
  (with-components ((o out) (v1 in1) (v2 in2))
    (psetf ox (u:lerp factor v1x v2x)
           oy (u:lerp factor v1y v2y)))
  out)

(int:define-op lerp ((in1 vec) (in2 vec) (factor double-float)) (:out vec)
  (lerp! (vec) in1 in2 factor))

(int:define-op < ((in1 vec) (in2 vec)) (:out boolean)
  (with-components ((v1 in1) (v2 in2))
    (and (cl:< v1x v2x)
         (cl:< v1y v2y))))

(int:define-op <= ((in1 vec) (in2 vec)) (:out boolean)
  (with-components ((v1 in1) (v2 in2))
    (and (cl:<= v1x v2x)
         (cl:<= v1y v2y))))

(int:define-op > ((in1 vec) (in2 vec)) (:out boolean)
  (with-components ((v1 in1) (v2 in2))
    (and (cl:> v1x v2x)
         (cl:> v1y v2y))))

(int:define-op >= ((in1 vec) (in2 vec)) (:out boolean)
  (with-components ((v1 in1) (v2 in2))
    (and (cl:>= v1x v2x)
         (cl:>= v1y v2y))))

(int:define-op min! ((out vec) (in1 vec) (in2 vec)) (:out vec)
  (with-components ((o out) (v1 in1) (v2 in2))
    (psetf ox (cl:min v1x v2x)
           oy (cl:min v1y v2y)))
  out)

(int:define-op min ((in1 vec) (in2 vec)) (:out vec)
  (min! (vec) in1 in2))

(int:define-op max! ((out vec) (in1 vec) (in2 vec)) (:out vec)
  (with-components ((o out) (v1 in1) (v2 in2))
    (psetf ox (cl:max v1x v2x)
           oy (cl:max v1y v2y)))
  out)

(int:define-op max ((in1 vec) (in2 vec)) (:out vec)
  (max! (vec) in1 in2))

(int:define-op radians! ((out vec) (vec vec)) (:out vec)
  (with-components ((o out) (v vec))
    (psetf ox (cl:* vx const:+deg/double+)
           oy (cl:* vy const:+deg/double+))
    out))

(int:define-op radians ((in vec)) (:out vec)
  (radians! (vec) in))

(int:define-op degrees! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (cl:* vx const:+rad/double+)
           oy (cl:* vy const:+rad/double+)))
  out)

(int:define-op degrees ((in vec)) (:out vec)
  (degrees! (vec) in))

(int:define-op expt! ((out vec) (in vec) (power real)) (:out vec :speed nil)
  (with-components ((o out) (v in))
    (psetf ox (cl:expt vx power)
           oy (cl:expt vy power)))
  out)

(int:define-op expt ((in vec) (power real)) (:out vec :speed nil)
  (expt! (vec) in power))

(int:define-op sqrt! ((out vec) (in vec)) (:out vec :speed nil)
  (with-components ((o out) (v in))
    (check-type vx (double-float 0d0))
    (check-type vy (double-float 0d0))
    (psetf ox (cl:sqrt (the (double-float 0d0) vx))
           oy (cl:sqrt (the (double-float 0d0) vy))))
  out)

(int:define-op sqrt ((in vec)) (:out vec :speed nil)
  (sqrt! (vec) in))

(int:define-op floor! ((out vec) (in vec)) (:out vec :speed nil)
  (with-components ((o out) (v in))
    (psetf ox (ffloor vx)
           oy (ffloor vy)))
  out)

(int:define-op floor ((in vec)) (:out vec :speed nil)
  (floor! (vec) in))

(int:define-op ceiling! ((out vec) (in vec)) (:out vec :speed nil)
  (with-components ((o out) (v in))
    (psetf ox (fceiling vx)
           oy (fceiling vy)))
  out)

(int:define-op ceiling ((in vec)) (:out vec :speed nil)
  (ceiling! (vec) in))

(int:define-op mod! ((out vec) (in vec) (divisor real)) (:out vec :speed nil)
  (with-components ((o out) (v in))
    (psetf ox (cl:mod vx divisor)
           oy (cl:mod vy divisor)))
  out)

(int:define-op mod ((in vec) (divisor real)) (:out vec :speed nil)
  (mod! (vec) in divisor))

(int:define-op sin! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (cl:sin vx)
           oy (cl:sin vy)))
  out)

(int:define-op sin ((in vec)) (:out vec)
  (sin! (vec) in))

(int:define-op cos! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (cl:cos vx)
           oy (cl:cos vy)))
  out)

(int:define-op cos ((in vec)) (:out vec)
  (cos! (vec) in))

(int:define-op tan! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (cl:tan vx)
           oy (cl:tan vy)))
  out)

(int:define-op tan ((in vec)) (:out vec)
  (tan! (vec) in))

(int:define-op asin! ((out vec) (in vec)) (:out vec :speed nil)
  (with-components ((o out) (v in))
    (psetf ox (cl:asin vx)
           oy (cl:asin vy)))
  out)

(int:define-op asin ((in vec)) (:out vec :speed nil)
  (asin! (vec) in))

(int:define-op acos! ((out vec) (in vec)) (:out vec :speed nil)
  (with-components ((o out) (v in))
    (psetf ox (cl:acos vx)
           oy (cl:acos vy)))
  out)

(int:define-op acos ((in vec)) (:out vec :speed nil)
  (acos! (vec) in))

(int:define-op atan! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (cl:atan vx)
           oy (cl:atan vy)))
  out)

(int:define-op atan ((in vec)) (:out vec)
  (atan! (vec) in))
