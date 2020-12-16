(in-package #:net.mfiano.lisp.origin.dvec3)

;;; accessors

(u:fn-> x (vec) u:f64)
(u:defun-inline x (vec)
  (aref vec 0))

(u:fn-> (setf x) (u:f64 vec) u:f64)
(u:defun-inline (setf x) (value vec)
  (setf (aref vec 0) value))

(u:fn-> y (vec) u:f64)
(u:defun-inline y (vec)
  (aref vec 1))

(u:fn-> (setf y) (u:f64 vec) u:f64)
(u:defun-inline (setf y) (value vec)
  (setf (aref vec 1) value))

(u:fn-> z (vec) u:f64)
(u:defun-inline z (vec)
  (aref vec 2))

(u:fn-> (setf z) (u:f64 vec) u:f64)
(u:defun-inline (setf z) (value vec)
  (setf (aref vec 2) value))

;;; constructors

(u:fn-> %vec (&rest u:f64) vec)
(u:eval-always
  (u:defun-inline %vec (&rest args)
    (declare (optimize speed))
    (make-array 3 :element-type 'double-float :initial-contents args)))

(ss:defstore vec (&rest args))

(ss:defspecialization (vec :inline t) () vec
  (%vec 0d0 0d0 0d0))

(ss:defspecialization (vec :inline t) ((x real)) vec
  (%vec (float x 1d0) (float x 1d0) (float x 1d0)))

(ss:defspecialization (vec :inline t) ((xy dv2:vec)) vec
  (dv2:with-components ((v xy))
    (%vec vx vy 0d0)))

(ss:defspecialization (vec :inline t) ((xyz vec)) vec
  (with-components ((v xyz))
    (%vec vx vy vz)))

(ss:defspecialization (vec :inline t) ((xyzw net.mfiano.lisp.origin.dvec4:vec))
    vec
  (net.mfiano.lisp.origin.dvec4:with-components ((v xyzw))
    (%vec vx vy vz)))

(ss:defspecialization (vec :inline t) ((x real) (y real)) vec
  (%vec (float x 1d0) (float y 1d0) 0d0))

(ss:defspecialization (vec :inline t) ((xy dv2:vec) (z real)) vec
  (dv2:with-components ((v xy))
    (%vec vx vy (float z 1d0))))

(ss:defspecialization (vec :inline t) ((x real) (yz dv2:vec)) vec
  (dv2:with-components ((v yz))
    (%vec (float x 1d0) vx vy)))

(ss:defspecialization (vec :inline t) ((x real) (y real) (z real)) vec
  (%vec (float x 1d0) (float y 1d0) (float z 1d0)))

(ss:defspecialization (vec :inline t) ((xyz v3:vec)) vec
  (v3:with-components ((v xyz))
    (%vec (float vx 1d0) (float vy 1d0) (float vz 1d0))))

;; ;;; constants

(u:define-constant +zero+ (%vec 0d0 0d0 0d0) :test #'equalp)

(u:define-constant +up+ (%vec 0d0 1d0 0d0) :test #'equalp)

(u:define-constant +down+ (%vec 0d0 -1d0 0d0) :test #'equalp)

(u:define-constant +left+ (%vec -1d0 0d0 0d0) :test #'equalp)

(u:define-constant +right+ (%vec 1d0 0d0 0d0) :test #'equalp)

(u:define-constant +forward+ (%vec 0d0 0d0 1d0) :test #'equalp)

(u:define-constant +back+ (%vec 0d0 0d0 -1d0) :test #'equalp)

;; ;;; operators

(u:fn-> = (vec vec &key (:rel u:f64) (:abs u:f64)) boolean)
(u:defun-inline = (vec1 vec2 &key (rel 1d-7) (abs rel))
  (declare (optimize speed))
  (with-components ((v1 vec1) (v2 vec2))
    (and (com:= v1x v2x rel abs)
         (com:= v1y v2y rel abs)
         (com:= v1z v2z rel abs))))

(u:fn-> zero! (vec) vec)
(u:defun-inline zero! (vec)
  (declare (optimize speed))
  (with-components ((v vec))
    (setf vx 0d0 vy 0d0 vz 0d0))
  vec)

(u:fn-> zero () vec)
(u:defun-inline zero ()
  (declare (optimize speed))
  (%vec 0d0 0d0 0d0))

(u:fn-> zero-p (vec) boolean)
(u:defun-inline zero-p (vec)
  (declare (optimize speed))
  (= vec +zero+))

(u:fn-> random! (vec u:f64 u:f64) vec)
(u:defun-inline random! (out min max)
  (declare (optimize speed))
  (let ((diff (cl:- max min)))
    (with-components ((o out))
      (setf ox (cl:+ min (cl:random diff))
            oy (cl:+ min (cl:random diff))
            oz (cl:+ min (cl:random diff)))))
  out)

(u:fn-> random (u:f64 u:f64) vec)
(u:defun-inline random (min max)
  (declare (optimize speed))
  (random! (zero) min max))

(u:fn-> copy! (vec vec) vec)
(u:defun-inline copy! (out vec)
  (declare (optimize speed))
  (with-components ((o out) (v vec))
    (setf ox vx oy vy oz vz))
  out)

(u:fn-> copy (vec) vec)
(u:defun-inline copy (vec)
  (declare (optimize speed))
  (copy! (zero) vec))

(u:fn-> sign! (vec vec) vec)
(u:defun-inline sign! (out vec)
  (declare (optimize speed))
  (with-components ((o out) (v vec))
    (psetf ox (signum vx)
           oy (signum vy)
           oz (signum vz)))
  out)

(u:fn-> sign (vec) vec)
(u:defun-inline sign (vec)
  (declare (optimize speed))
  (sign! (zero) vec))

(u:fn-> fract! (vec vec) vec)
(u:defun-inline fract! (out vec)
  (with-components ((o out) (v vec))
    (psetf ox (cl:- vx (ffloor vx))
           oy (cl:- vy (ffloor vy))
           oz (cl:- vz (ffloor vz))))
  out)

(u:fn-> fract (vec) vec)
(u:defun-inline fract (vec)
  (fract! (zero) vec))

(u:fn-> clamp! (vec vec u:f64 u:f64) vec)
(u:defun-inline clamp! (out vec min max)
  (declare (optimize speed))
  (with-components ((o out) (v vec))
    (psetf ox (u:clamp vx min max)
           oy (u:clamp vy min max)
           oz (u:clamp vz min max)))
  out)

(u:fn-> clamp (vec u:f64 u:f64) vec)
(u:defun-inline clamp (vec min max)
  (declare (optimize speed))
  (clamp! (zero) vec min max))

(u:fn-> +! (vec vec vec) vec)
(u:defun-inline +! (out vec1 vec2)
  (declare (optimize speed))
  (with-components ((o out) (v1 vec1) (v2 vec2))
    (psetf ox (cl:+ v1x v2x)
           oy (cl:+ v1y v2y)
           oz (cl:+ v1z v2z)))
  out)

(u:fn-> + (vec vec) vec)
(u:defun-inline + (vec1 vec2)
  (declare (optimize speed))
  (+! (zero) vec1 vec2))

(u:fn-> -! (vec vec vec) vec)
(u:defun-inline -! (out vec1 vec2)
  (declare (optimize speed))
  (with-components ((o out) (v1 vec1) (v2 vec2))
    (psetf ox (cl:- v1x v2x)
           oy (cl:- v1y v2y)
           oz (cl:- v1z v2z)))
  out)

(u:fn-> - (vec vec) vec)
(u:defun-inline - (vec1 vec2)
  (declare (optimize speed))
  (-! (zero) vec1 vec2))

(u:fn-> *! (vec vec vec) vec)
(u:defun-inline *! (out vec1 vec2)
  (declare (optimize speed))
  (with-components ((o out) (v1 vec1) (v2 vec2))
    (psetf ox (cl:* v1x v2x)
           oy (cl:* v1y v2y)
           oz (cl:* v1z v2z)))
  out)

(u:fn-> * (vec vec) vec)
(u:defun-inline * (vec1 vec2)
  (declare (optimize speed))
  (*! (zero) vec1 vec2))

(u:fn-> /! (vec vec vec) vec)
(u:defun-inline /! (out vec1 vec2)
  (declare (optimize speed))
  (with-components ((o out) (v1 vec1) (v2 vec2))
    (psetf ox (if (zerop v2x) 0d0 (cl:/ v1x v2x))
           oy (if (zerop v2y) 0d0 (cl:/ v1y v2y))
           oz (if (zerop v2z) 0d0 (cl:/ v1z v2z))))
  out)

(u:fn-> / (vec vec) vec)
(u:defun-inline / (vec1 vec2)
  (declare (optimize speed))
  (/! (zero) vec1 vec2))

(defmacro %scale (ox oy oz x y z scalar)
  `(psetf ,ox (cl:* ,x ,scalar)
          ,oy (cl:* ,y ,scalar)
          ,oz (cl:* ,z ,scalar)))

(u:fn-> scale! (vec vec u:f64) vec)
(u:defun-inline scale! (out vec scalar)
  (declare (optimize speed))
  (with-components ((o out) (v vec))
    (%scale ox oy oz vx vy vz scalar))
  out)

(u:fn-> scale (vec u:f64) vec)
(u:defun-inline scale (vec scalar)
  (declare (optimize speed))
  (scale! (zero) vec scalar))

(u:fn-> invert! (vec vec) vec)
(u:defun-inline invert! (out vec)
  (declare (optimize speed))
  (with-components ((o out) (v vec))
    (psetf ox (if (zerop vx) 0d0 (cl:/ vx))
           oy (if (zerop vy) 0d0 (cl:/ vy))
           oz (if (zerop vz) 0d0 (cl:/ vz)))
    out))

(u:fn-> invert (vec) vec)
(u:defun-inline invert (vec)
  (declare (optimize speed))
  (invert! (zero) vec))

(defmacro %dot (v1x v1y v1z v2x v2y v2z)
  `(cl:+ (cl:* ,v1x ,v2x) (cl:* ,v1y ,v2y) (cl:* ,v1z ,v2z)))

(u:fn-> dot (vec vec) u:f64)
(u:defun-inline dot (vec1 vec2)
  (with-components ((v1 vec1) (v2 vec2))
    (%dot v1x v1y v1z v2x v2y v2z)))

(defmacro %length-squared (x y z)
  ;; NOTE: This is not using %DOT because using * instead of EXPT and SBCL 1.5.9
  ;; cannot correctly infer the type of the SQRT of the sum of squares as being
  ;; a single-float. This is because SBCL's memory model policy is "everything
  ;; is volatile", which is acceptable because two AREF calls to the same array
  ;; may infact produce different values when threading is involved.
  `(cl:+ (cl:expt ,x 2) (cl:expt ,y 2) (cl:expt ,z 2)))

(u:fn-> length-squared (vec) u:f64)
(u:defun-inline length-squared (vec)
  (with-components ((v vec))
    (%length-squared vx vy vz)))

(defmacro %length (x y z)
  `(cl:sqrt (%length-squared ,x ,y ,z)))

(u:fn-> length (vec) u:f64)
(u:defun-inline length (vec)
  (cl:sqrt (length-squared vec)))

(u:fn-> distance-squared (vec vec) u:f64)
(u:defun-inline distance-squared (vec1 vec2)
  (length-squared (- vec2 vec1)))

(u:fn-> distance (vec vec) u:f64)
(u:defun-inline distance (vec1 vec2)
  (cl:sqrt (distance-squared vec1 vec2)))

(defmacro %normalize (ox oy oz x y z)
  (u:with-gensyms (length inv-length)
    `(let ((,length (%length ,x ,y ,z)))
       (unless (zerop ,length)
         (let ((,inv-length (cl:/ ,length)))
           (%scale ,ox ,oy ,oz ,x ,y ,z ,inv-length))))))

(u:fn-> normalize! (vec vec) vec)
(u:defun-inline normalize! (out vec)
  (declare (optimize speed))
  (with-components ((o out) (v vec))
    (%normalize ox oy oz vx vy vz))
  out)

(u:fn-> normalize (vec) vec)
(u:defun-inline normalize (vec)
  (declare (optimize speed))
  (normalize! (zero) vec))

(u:fn-> round! (vec vec) vec)
(u:defun-inline round! (out vec)
  (with-components ((o out) (v vec))
    (psetf ox (fround vx)
           oy (fround vy)
           oz (fround vz)))
  out)

(u:fn-> round (vec) vec)
(u:defun-inline round (vec)
  (round! (zero) vec))

(u:fn-> abs! (vec vec) vec)
(u:defun-inline abs! (out vec)
  (declare (optimize speed))
  (with-components ((o out) (v vec))
    (psetf ox (cl:abs vx)
           oy (cl:abs vy)
           oz (cl:abs vz)))
  out)

(u:fn-> abs (vec) vec)
(u:defun-inline abs (vec)
  (declare (optimize speed))
  (abs! (zero) vec))

(u:fn-> negate! (vec vec) vec)
(u:defun-inline negate! (out vec)
  (declare (optimize speed))
  (scale! out vec -1d0))

(u:fn-> negate (vec) vec)
(u:defun-inline negate (vec)
  (declare (optimize speed))
  (negate! (zero) vec))

(u:fn-> cross! (vec vec vec) vec)
(u:defun-inline cross! (out vec1 vec2)
  (declare (optimize speed))
  (with-components ((o out) (v1 vec1) (v2 vec2))
    (psetf ox (cl:- (cl:* v1y v2z) (cl:* v1z v2y))
           oy (cl:- (cl:* v1z v2x) (cl:* v1x v2z))
           oz (cl:- (cl:* v1x v2y) (cl:* v1y v2x))))
  out)

(u:fn-> cross (vec vec) vec)
(u:defun-inline cross (vec1 vec2)
  (declare (optimize speed))
  (cross! (zero) vec1 vec2))

(u:fn-> angle (vec vec) u:f64)
(u:defun-inline angle (vec1 vec2)
  (let ((dot (dot vec1 vec2))
        (m*m (cl:* (length vec1) (length vec2))))
    (if (zerop m*m)
        0d0
        (cl:acos (the (double-float -1d0 1d0) (cl:/ dot m*m))))))

(u:fn-> direction= (vec vec) boolean)
(u:defun-inline direction= (vec1 vec2)
  (declare (optimize speed))
  (cl:>= (dot (normalize vec1) (normalize vec2)) (cl:- 1 1d-7)))

(u:fn-> parallel-p (vec vec) boolean)
(u:defun-inline parallel-p (vec1 vec2)
  (= (cross vec1 vec2) +zero+))

(u:fn-> lerp! (vec vec vec u:f64) vec)
(u:defun-inline lerp! (out vec1 vec2 factor)
  (declare (optimize speed))
  (with-components ((o out) (v1 vec1) (v2 vec2))
    (psetf ox (u:lerp factor v1x v2x)
           oy (u:lerp factor v1y v2y)
           oz (u:lerp factor v1z v2z)))
  out)

(u:fn-> lerp (vec vec u:f64) vec)
(u:defun-inline lerp (vec1 vec2 factor)
  (declare (optimize speed))
  (lerp! (zero) vec1 vec2 factor))

(u:fn-> < (vec vec) boolean)
(u:defun-inline < (vec1 vec2)
  (declare (optimize speed))
  (with-components ((v1 vec1) (v2 vec2))
    (and (cl:< v1x v2x)
         (cl:< v1y v2y)
         (cl:< v1z v2z))))

(u:fn-> <= (vec vec) boolean)
(u:defun-inline <= (vec1 vec2)
  (declare (optimize speed))
  (with-components ((v1 vec1) (v2 vec2))
    (and (cl:<= v1x v2x)
         (cl:<= v1y v2y)
         (cl:<= v1z v2z))))

(u:fn-> > (vec vec) boolean)
(u:defun-inline > (vec1 vec2)
  (declare (optimize speed))
  (with-components ((v1 vec1) (v2 vec2))
    (and (cl:> v1x v2x)
         (cl:> v1y v2y)
         (cl:> v1z v2z))))

(u:fn-> >= (vec vec) boolean)
(u:defun-inline >= (vec1 vec2)
  (declare (optimize speed))
  (with-components ((v1 vec1) (v2 vec2))
    (and (cl:>= v1x v2x)
         (cl:>= v1y v2y)
         (cl:>= v1z v2z))))

(u:fn-> min! (vec vec vec) vec)
(u:defun-inline min! (out vec1 vec2)
  (declare (optimize speed))
  (with-components ((o out) (v1 vec1) (v2 vec2))
    (psetf ox (cl:min v1x v2x)
           oy (cl:min v1y v2y)
           oz (cl:min v1z v2z)))
  out)

(u:fn-> min (vec vec) vec)
(u:defun-inline min (vec1 vec2)
  (declare (optimize speed))
  (min! (zero) vec1 vec2))

(u:fn-> max! (vec vec vec) vec)
(u:defun-inline max! (out vec1 vec2)
  (declare (optimize speed))
  (with-components ((o out) (v1 vec1) (v2 vec2))
    (psetf ox (cl:max v1x v2x)
           oy (cl:max v1y v2y)
           oz (cl:max v1z v2z)))
  out)

(u:fn-> max (vec vec) vec)
(u:defun-inline max (vec1 vec2)
  (declare (optimize speed))
  (max! (zero) vec1 vec2))

(u:fn-> radians! (vec vec) vec)
(u:defun-inline radians! (out vec)
  (declare (optimize speed))
  (with-components ((o out) (v vec))
    (psetf ox (cl:* vx com:+deg/double+)
           oy (cl:* vy com:+deg/double+)
           oz (cl:* vz com:+deg/double+)))
  out)

(u:fn-> radians (vec) vec)
(u:defun-inline radians (vec)
  (declare (optimize speed))
  (radians! (zero) vec))

(u:fn-> degrees! (vec vec) vec)
(u:defun-inline degrees! (out vec)
  (declare (optimize speed))
  (with-components ((o out) (v vec))
    (psetf ox (cl:* vx com:+rad/double+)
           oy (cl:* vy com:+rad/double+)
           oz (cl:* vz com:+rad/double+)))
  out)

(u:fn-> degrees (vec) vec)
(u:defun-inline degrees (vec)
  (declare (optimize speed))
  (degrees! (zero) vec))

(u:fn-> expt! (vec vec real) vec)
(u:defun-inline expt! (out vec power)
  (with-components ((o out) (v vec))
    (psetf ox (cl:expt vx power)
           oy (cl:expt vy power)
           oz (cl:expt vz power)))
  out)

(u:fn-> expt (vec real) vec)
(u:defun-inline expt (vec power)
  (expt! (zero) vec power))

(u:fn-> sqrt! (vec vec) vec)
(u:defun-inline sqrt! (out vec)
  (declare (optimize speed))
  (with-components ((o out) (v vec))
    (psetf ox (cl:sqrt (the (double-float 0d0) vx))
           oy (cl:sqrt (the (double-float 0d0) vy))
           oz (cl:sqrt (the (double-float 0d0) vz))))
  out)

(u:fn-> sqrt (vec) vec)
(u:defun-inline sqrt (vec)
  (declare (optimize speed))
  (sqrt! (zero) vec))

(u:fn-> floor! (vec vec) vec)
(u:defun-inline floor! (out vec)
  (with-components ((o out) (v vec))
    (psetf ox (ffloor vx)
           oy (ffloor vy)
           oz (ffloor vz)))
  out)

(u:fn-> floor (vec) vec)
(u:defun-inline floor (vec)
  (floor! (zero) vec))

(u:fn-> ceiling! (vec vec) vec)
(u:defun-inline ceiling! (out vec)
  (with-components ((o out) (v vec))
    (psetf ox (fceiling vx)
           oy (fceiling vy)
           oz (fceiling vz)))
  out)

(u:fn-> ceiling (vec) vec)
(u:defun-inline ceiling (vec)
  (ceiling! (zero) vec))

(u:fn-> mod! (vec vec u:f64) vec)
(u:defun-inline mod! (out vec divisor)
  (with-components ((o out) (v vec))
    (psetf ox (nth-value 1 (ffloor vx divisor))
           oy (nth-value 1 (ffloor vy divisor))
           oz (nth-value 1 (ffloor vz divisor))))
  out)

(u:fn-> mod (vec real) vec)
(u:defun-inline mod (vec divisor)
  (mod! (zero) vec divisor))

(u:fn-> sin! (vec vec) vec)
(u:defun-inline sin! (out vec)
  (declare (optimize speed))
  (with-components ((o out) (v vec))
    (psetf ox (cl:sin vx)
           oy (cl:sin vy)
           oz (cl:sin vz)))
  out)

(u:fn-> sin (vec) vec)
(u:defun-inline sin (vec)
  (declare (optimize speed))
  (sin! (zero) vec))

(u:fn-> cos! (vec vec) vec)
(u:defun-inline cos! (out vec)
  (declare (optimize speed))
  (with-components ((o out) (v vec))
    (psetf ox (cl:cos vx)
           oy (cl:cos vy)
           oz (cl:cos vz)))
  out)

(u:fn-> cos (vec) vec)
(u:defun-inline cos (vec)
  (declare (optimize speed))
  (cos! (zero) vec))

(u:fn-> tan! (vec vec) vec)
(u:defun-inline tan! (out vec)
  (declare (optimize speed))
  (with-components ((o out) (v vec))
    (psetf ox (cl:tan vx)
           oy (cl:tan vy)
           oz (cl:tan vz)))
  out)

(u:fn-> tan (vec) vec)
(u:defun-inline tan (vec)
  (declare (optimize speed))
  (tan! (zero) vec))

(u:fn-> asin! (vec vec) vec)
(u:defun-inline asin! (out vec)
  (declare (optimize speed))
  (with-components ((o out) (v vec))
    (setf ox (cl:asin (the (double-float -1d0 1d0) vx))
          oy (cl:asin (the (double-float -1d0 1d0) vy))
          oz (cl:asin (the (double-float -1d0 1d0) vz))))
  out)

(u:fn-> asin (vec) vec)
(u:defun-inline asin (vec)
  (declare (optimize speed))
  (asin! (zero) vec))

(u:fn-> acos! (vec vec) vec)
(u:defun-inline acos! (out vec)
  (declare (optimize speed))
  (with-components ((o out) (v vec))
    (setf ox (cl:acos (the (double-float -1d0 1d0) vx))
          oy (cl:acos (the (double-float -1d0 1d0) vy))
          oz (cl:acos (the (double-float -1d0 1d0) vz))))
  out)

(u:fn-> acos (vec) vec)
(u:defun-inline acos (vec)
  (declare (optimize speed))
  (acos! (zero) vec))

(u:fn-> atan! (vec vec) vec)
(u:defun-inline atan! (out vec)
  (declare (optimize speed))
  (with-components ((o out) (v vec))
    (psetf ox (cl:atan vx)
           oy (cl:atan vy)
           oz (cl:atan vz)))
  out)

(u:fn-> atan (vec) vec)
(u:defun-inline atan (vec)
  (declare (optimize speed))
  (atan! (zero) vec))

(u:fn-> velocity! (vec vec u:f64) vec)
(u:defun-inline velocity! (vec axis rate)
  (declare (optimize speed))
  (copy! vec axis)
  (normalize! vec vec)
  (scale! vec vec rate))

(u:fn-> velocity (vec u:f64) vec)
(u:defun-inline velocity (axis rate)
  (declare (optimize speed))
  (velocity! (zero) axis rate))
