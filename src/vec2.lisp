(in-package #:origin.vec2)

(deftype vec () '(simple-array single-float (2)))

(define-op x ((vec vec)) (:out single-float)
  (aref vec 0))

(define-op (setf x) ((new-value single-float) (vec vec)) (:out single-float)
  (setf (aref vec 0) new-value))

(define-op y ((vec vec)) (:out single-float)
  (aref vec 1))

(define-op (setf y) ((new-value single-float) (vec vec)) (:out single-float)
  (setf (aref vec 1) new-value))

(defmacro with-components (((prefix vec) &rest rest) &body body)
  (a:once-only (vec)
    `(symbol-macrolet ((,prefix ,vec)
                       (,(make-accessor-symbol prefix "X") (aref ,vec 0))
                       (,(make-accessor-symbol prefix "Y") (aref ,vec 1)))
       ,(if rest
            `(with-components ,rest ,@body)
            `(progn ,@body)))))

(defmacro with-elements (((prefix x y) &rest rest) &body body)
  (let ((%x (make-accessor-symbol prefix "X"))
        (%y (make-accessor-symbol prefix "Y")))
    `(let ((,%x ,x) (,%y ,y))
       (declare (ignorable ,%x ,%y))
       ,(if rest
            `(with-elements ,rest ,@body)
            `(progn ,@body)))))

(define-op vec ((x single-float) (y single-float)) (:out vec)
  (let ((vec (make-array 2 :element-type 'single-float :initial-element 0f0)))
    (with-components ((v vec))
      (psetf vx x vy y))
    vec))

(a:define-constant +zero+ (vec 0f0 0f0) :test #'equalp)

(define-op zero! ((in vec)) (:out vec)
  (with-components ((v in))
    (psetf vx 0f0 vy 0f0))
  in)

(define-op zero () (:out vec)
  (vec 0f0 0f0))

(define-op one! ((in vec)) (:out vec)
  (with-components ((v in))
    (psetf vx 1f0 vy 1f0))
  in)

(define-op one () (:out vec)
  (one! (zero)))

(define-op zero-p ((in vec)) (:out boolean)
  (with-components ((v in))
    (cl:= 0f0 vx vy)))

(define-op random! ((out vec)
                    &key (min single-float 0f0) (max single-float 1f0))
    (:out vec)
  (with-components ((o out))
    (psetf ox (cl:+ min (cl:random (cl:- max min)))
           oy (cl:+ min (cl:random (cl:- max min)))))
  out)

(define-op random ((min single-float) (max single-float))
    (:out vec)
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
    (psetf ox (cl:- vx (ffloor vx))
           oy (cl:- vy (ffloor vy))))
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
           oy (a:clamp vy min max)))
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

(defmacro %scale (ox oy x y scalar)
  `(psetf ,ox (cl:* ,x ,scalar)
          ,oy (cl:* ,y ,scalar)))

(define-op scale! ((out vec) (in vec) (scalar single-float)) (:out vec)
  (with-components ((o out) (v in))
    (%scale ox oy vx vy scalar))
  out)

(define-op scale ((in vec) (scalar single-float)) (:out vec)
  (scale! (zero) in scalar))

(defmacro %dot (v1x v1y v2x v2y)
  `(cl:+ (cl:* ,v1x ,v2x) (cl:* ,v1y ,v2y)))

(define-op dot ((in1 vec) (in2 vec)) (:out single-float)
  (with-components ((v1 in1) (v2 in2))
    (%dot v1x v1y v2x v2y)))

(defmacro %length-squared (x y)
  ;; NOTE: This is not using %DOT because using * instead of EXPT and SBCL 1.5.9
  ;; cannot correctly infer the type of the SQRT of the sum of squares as being
  ;; a single-float. This is because SBCL's memory model policy is "everything
  ;; is volatile", which is acceptable because two AREF calls to the same array
  ;; may infact produce different values when threading is involved.
  `(cl:+ (cl:expt ,x 2) (cl:expt ,y 2)))

(define-op length-squared ((in vec)) (:out single-float)
  (with-components ((v in))
    (%length-squared vx vy)))

(defmacro %length (x y)
  `(cl:sqrt (%length-squared ,x ,y)))

(define-op length ((in vec)) (:out single-float)
  (cl:sqrt (length-squared in)))

(define-op distance-squared ((in1 vec) (in2 vec)) (:out single-float)
  (length-squared (- in2 in1)))

(define-op distance ((in1 vec) (in2 vec)) (:out single-float)
  (cl:sqrt (distance-squared in1 in2)))

(defmacro %normalize (ox oy x y)
  (a:with-gensyms (length)
    `(let ((,length (%length ,x ,y)))
       (unless (zerop ,length)
         (%scale ,ox ,oy ,x ,y (cl:/ ,length))))))

(define-op normalize! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (%normalize ox oy vx vy))
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

(define-op angle ((in1 vec) (in2 vec)) (:out single-float :speed nil)
  (let ((dot (dot in1 in2))
        (m*m (cl:* (length in1) (length in2))))
    (if (zerop m*m) 0f0 (cl:acos (cl:/ dot m*m)))))

(define-op direction= ((in1 vec) (in2 vec)) (:out boolean)
  (cl:>= (dot (normalize in1) (normalize in2)) (cl:- 1 1e-7)))

(define-op lerp! ((out vec) (in1 vec) (in2 vec) (factor single-float))
    (:out vec)
  (with-components ((o out) (v1 in1) (v2 in2))
    (psetf ox (a:lerp factor v1x v2x)
           oy (a:lerp factor v1y v2y)))
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
    (psetf ox (cl:* vx origin:+deg+)
           oy (cl:* vy origin:+deg+))
    out))

(define-op radians ((in vec)) (:out vec)
  (radians! (zero) in))

(define-op degrees! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (cl:* vx origin:+rad+)
           oy (cl:* vy origin:+rad+)))
  out)

(define-op degrees ((in vec)) (:out vec)
  (degrees! (zero) in))

(define-op expt! ((out vec) (in vec) (power real)) (:out vec :speed nil)
  (with-components ((o out) (v in))
    (psetf ox (cl:expt vx power)
           oy (cl:expt vy power)))
  out)

(define-op expt ((in vec) (power real)) (:out vec :speed nil)
  (expt! (zero) in power))

(define-op sqrt! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (check-type vx (single-float 0f0))
    (check-type vy (single-float 0f0))
    (psetf ox (cl:sqrt (the (single-float 0f0) vx))
           oy (cl:sqrt (the (single-float 0f0) vy))))
  out)

(define-op sqrt ((in vec)) (:out vec)
  (sqrt! (zero) in))

(define-op floor! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (ffloor vx)
           oy (ffloor vy)))
  out)

(define-op floor ((in vec)) (:out vec)
  (floor! (zero) in))

(define-op ceiling! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (fceiling vx)
           oy (fceiling vy)))
  out)

(define-op ceiling ((in vec)) (:out vec)
  (ceiling! (zero) in))

(define-op mod! ((out vec) (in vec) (divisor real)) (:out vec :speed nil)
  (with-components ((o out) (v in))
    (psetf ox (cl:mod vx divisor)
           oy (cl:mod vy divisor)))
  out)

(define-op mod ((in vec) (divisor real)) (:out vec :speed nil)
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

(define-op asin! ((out vec) (in vec)) (:out vec :speed nil)
  (with-components ((o out) (v in))
    (psetf ox (cl:asin vx)
           oy (cl:asin vy)))
  out)

(define-op asin ((in vec)) (:out vec :speed nil)
  (asin! (zero) in))

(define-op acos! ((out vec) (in vec)) (:out vec :speed nil)
  (with-components ((o out) (v in))
    (psetf ox (cl:acos vx)
           oy (cl:acos vy)))
  out)

(define-op acos ((in vec)) (:out vec :speed nil)
  (acos! (zero) in))

(define-op atan! ((out vec) (in vec)) (:out vec)
  (with-components ((o out) (v in))
    (psetf ox (cl:atan vx)
           oy (cl:atan vy)))
  out)

(define-op atan ((in vec)) (:out vec)
  (atan! (zero) in))
