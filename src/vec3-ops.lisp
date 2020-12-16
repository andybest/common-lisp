(in-package #:net.mfiano.lisp.origin.vec3)

;;; accessors

(u:fn-> x (vec) u:f32)
(u:defun-inline x (vec)
  (declare (optimize speed))
  (aref vec 0))

(u:fn-> (setf x) (u:f32 vec) u:f32)
(u:defun-inline (setf x) (value vec)
  (declare (optimize speed))
  (setf (aref vec 0) value))

(u:fn-> y (vec) u:f32)
(u:defun-inline y (vec)
  (declare (optimize speed))
  (aref vec 1))

(u:fn-> (setf y) (u:f32 vec) u:f32)
(u:defun-inline (setf y) (value vec)
  (declare (optimize speed))
  (setf (aref vec 1) value))

(u:fn-> z (vec) u:f32)
(u:defun-inline z (vec)
  (declare (optimize speed))
  (aref vec 2))

(u:fn-> (setf z) (u:f32 vec) u:f32)
(u:defun-inline (setf z) (value vec)
  (declare (optimize speed))
  (setf (aref vec 2) value))

;;; constructors

(u:fn-> %vec (&rest u:f32) vec)
(u:eval-always
  (u:defun-inline %vec (&rest args)
    (declare (optimize speed))
    (make-array 3 :element-type 'single-float :initial-contents args)))

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

(u:fn-> = (vec vec &key (:rel u:f32) (:abs u:f32)) boolean)
(u:defun-inline = (vec1 vec2 &key (rel 1e-7) (abs rel))
  (declare (optimize speed))
  (with-components ((v1 vec1) (v2 vec2))
    (and (int:= v1x v2x rel abs)
         (int:= v1y v2y rel abs)
         (int:= v1z v2z rel abs))))

(u:fn-> zero! (vec) vec)
(u:defun-inline zero! (vec)
  (declare (optimize speed))
  (with-components ((v vec))
    (setf vx 0f0 vy 0f0 vz 0f0))
  vec)

(u:fn-> zero-p (vec) boolean)
(u:defun-inline zero-p (vec)
  (declare (optimize speed))
  (= vec +zero+))

(u:fn-> random! (vec u:f32 u:f32) vec)
(u:defun-inline random! (out min max)
  (declare (optimize speed))
  (let ((diff (cl:- max min)))
    (with-components ((o out))
      (setf ox (cl:+ min (cl:random diff))
            oy (cl:+ min (cl:random diff))
            oz (cl:+ min (cl:random diff)))))
  out)

(u:fn-> random (u:f32 u:f32) vec)
(u:defun-inline random (min max)
  (declare (optimize speed))
  (random! (vec) min max))

(u:fn-> copy! (vec vec) vec)
(u:defun-inline copy! (out vec)
  (declare (optimize speed))
  (with-components ((o out) (v vec))
    (setf ox vx oy vy oz vz))
  out)

(u:fn-> copy (vec) vec)
(u:defun-inline copy (vec)
  (declare (optimize speed))
  (copy! (vec) vec))

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
  (sign! (vec) vec))

(u:fn-> fract! (vec vec) vec)
(u:defun-inline fract! (out vec)
  (declare (optimize speed))
  (with-components ((o out) (v vec))
    (psetf ox (cl:- vx (ffloor vx))
           oy (cl:- vy (ffloor vy))
           oz (cl:- vz (ffloor vz))))
  out)

(u:fn-> fract (vec) vec)
(u:defun-inline fract (vec)
  (declare (optimize speed))
  (fract! (vec) vec))

(u:fn-> clamp! (vec vec u:f32 u:f32) vec)
(u:defun-inline clamp! (out vec min max)
  (declare (optimize speed))
  (with-components ((o out) (v vec))
    (psetf ox (u:clamp vx min max)
           oy (u:clamp vy min max)
           oz (u:clamp vz min max)))
  out)

(u:fn-> clamp (vec u:f32 u:f32) vec)
(u:defun-inline clamp (vec min max)
  (declare (optimize speed))
  (clamp! (vec) vec min max))

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
  (+! (vec) vec1 vec2))

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
  (-! (vec) vec1 vec2))

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
  (*! (vec) vec1 vec2))

(u:fn-> /! (vec vec vec) vec)
(u:defun-inline /! (out vec1 vec2)
  (declare (optimize speed))
  (with-components ((o out) (v1 vec1) (v2 vec2))
    (psetf ox (if (zerop v2x) 0f0 (cl:/ v1x v2x))
           oy (if (zerop v2y) 0f0 (cl:/ v1y v2y))
           oz (if (zerop v2z) 0f0 (cl:/ v1z v2z))))
  out)

(u:fn-> / (vec vec) vec)
(u:defun-inline / (vec1 vec2)
  (declare (optimize speed))
  (/! (vec) vec1 vec2))

(defmacro %scale (ox oy oz x y z scalar)
  `(psetf ,ox (cl:* ,x ,scalar)
          ,oy (cl:* ,y ,scalar)
          ,oz (cl:* ,z ,scalar)))

(u:fn-> scale! (vec vec u:f32) vec)
(u:defun-inline scale! (out vec scalar)
  (declare (optimize speed))
  (with-components ((o out) (v vec))
    (%scale ox oy oz vx vy vz scalar))
  out)

(u:fn-> scale (vec u:f32) vec)
(u:defun-inline scale (vec scalar)
  (declare (optimize speed))
  (scale! (vec) vec scalar))

(u:fn-> invert! (vec vec) vec)
(u:defun-inline invert! (out vec)
  (declare (optimize speed))
  (with-components ((o out) (v vec))
    (psetf ox (if (zerop vx) 0f0 (cl:/ vx))
           oy (if (zerop vy) 0f0 (cl:/ vy))
           oz (if (zerop vz) 0f0 (cl:/ vz)))
    out))

(u:fn-> invert (vec) vec)
(u:defun-inline invert (vec)
  (declare (optimize speed))
  (invert! (vec) vec))

(defmacro %dot (v1x v1y v1z v2x v2y v2z)
  `(cl:+ (cl:* ,v1x ,v2x) (cl:* ,v1y ,v2y) (cl:* ,v1z ,v2z)))

(u:fn-> dot (vec vec) u:f32)
(u:defun-inline dot (vec1 vec2)
  (declare (optimize speed))
  (with-components ((v1 vec1) (v2 vec2))
    (%dot v1x v1y v1z v2x v2y v2z)))

(defmacro %length-squared (x y z)
  ;; NOTE: This is not using %DOT because using * instead of EXPT and SBCL 1.5.9
  ;; cannot correctly infer the type of the SQRT of the sum of squares as being
  ;; a single-float. This is because SBCL's memory model policy is "everything
  ;; is volatile", which is acceptable because two AREF calls to the same array
  ;; may infact produce different values when threading is involved.
  `(cl:+ (cl:expt ,x 2) (cl:expt ,y 2) (cl:expt ,z 2)))

(u:fn-> length-squared (vec) u:f32)
(u:defun-inline length-squared (vec)
  (declare (optimize speed))
  (with-components ((v vec))
    (%length-squared vx vy vz)))

(defmacro %length (x y z)
  `(cl:sqrt (%length-squared ,x ,y ,z)))

(u:fn-> length (vec) u:f32)
(u:defun-inline length (vec)
  (declare (optimize speed))
  (cl:sqrt (length-squared vec)))

(u:fn-> distance-squared (vec vec) u:f32)
(u:defun-inline distance-squared (vec1 vec2)
  (declare (optimize speed))
  (length-squared (- vec2 vec1)))

(u:fn-> distance (vec vec) u:f32)
(u:defun-inline distance (vec1 vec2)
  (declare (optimize speed))
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
  (normalize! (vec) vec))

(u:fn-> round! (vec vec) vec)
(u:defun-inline round! (out vec)
  (declare (optimize speed))
  (with-components ((o out) (v vec))
    (psetf ox (fround vx)
           oy (fround vy)
           oz (fround vz)))
  out)

(u:fn-> round (vec) vec)
(u:defun-inline round (vec)
  (declare (optimize speed))
  (round! (vec) vec))

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
  (abs! (vec) vec))

(u:fn-> negate! (vec vec) vec)
(u:defun-inline negate! (out vec)
  (declare (optimize speed))
  (scale! out vec -1f0))

(u:fn-> negate (vec) vec)
(u:defun-inline negate (vec)
  (declare (optimize speed))
  (negate! (vec) vec))

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
  (cross! (vec) vec1 vec2))

(u:fn-> angle (vec vec) u:f32)
(u:defun-inline angle (vec1 vec2)
  (declare (optimize speed))
  (let ((dot (dot vec1 vec2))
        (m*m (cl:* (length vec1) (length vec2))))
    (if (zerop m*m)
        0f0
        (cl:acos (the (single-float -1f0 1f0) (cl:/ dot m*m))))))

(u:fn-> direction= (vec vec) boolean)
(u:defun-inline direction= (vec1 vec2)
  (declare (optimize speed))
  (cl:>= (dot (normalize vec1) (normalize vec2)) (cl:- 1 1e-7)))

(u:fn-> parallel-p (vec vec) boolean)
(u:defun-inline parallel-p (vec1 vec2)
  (declare (optimize speed))
  (= (cross vec1 vec2) +zero+))

(u:fn-> lerp! (vec vec vec u:f32) vec)
(u:defun-inline lerp! (out vec1 vec2 factor)
  (declare (optimize speed))
  (with-components ((o out) (v1 vec1) (v2 vec2))
    (psetf ox (u:lerp factor v1x v2x)
           oy (u:lerp factor v1y v2y)
           oz (u:lerp factor v1z v2z)))
  out)

(u:fn-> lerp (vec vec u:f32) vec)
(u:defun-inline lerp (vec1 vec2 factor)
  (declare (optimize speed))
  (lerp! (vec) vec1 vec2 factor))

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
  (min! (vec) vec1 vec2))

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
  (max! (vec) vec1 vec2))

(u:fn-> radians! (vec vec) vec)
(u:defun-inline radians! (out vec)
  (declare (optimize speed))
  (with-components ((o out) (v vec))
    (psetf ox (cl:* vx const:+deg+)
           oy (cl:* vy const:+deg+)
           oz (cl:* vz const:+deg+)))
  out)

(u:fn-> radians (vec) vec)
(u:defun-inline radians (vec)
  (declare (optimize speed))
  (radians! (vec) vec))

(u:fn-> degrees! (vec vec) vec)
(u:defun-inline degrees! (out vec)
  (declare (optimize speed))
  (with-components ((o out) (v vec))
    (psetf ox (cl:* vx const:+rad+)
           oy (cl:* vy const:+rad+)
           oz (cl:* vz const:+rad+)))
  out)

(u:fn-> degrees (vec) vec)
(u:defun-inline degrees (vec)
  (declare (optimize speed))
  (degrees! (vec) vec))

(u:fn-> expt! (vec vec real) vec)
(u:defun-inline expt! (out vec power)
  (with-components ((o out) (v vec))
    (psetf ox (cl:expt vx power)
           oy (cl:expt vy power)
           oz (cl:expt vz power)))
  out)

(u:fn-> expt (vec real) vec)
(u:defun-inline expt (vec power)
  (expt! (vec) vec power))

(u:fn-> sqrt! (vec vec) vec)
(u:defun-inline sqrt! (out vec)
  (declare (optimize speed))
  (with-components ((o out) (v vec))
    (psetf ox (cl:sqrt (the (single-float 0f0) vx))
           oy (cl:sqrt (the (single-float 0f0) vy))
           oz (cl:sqrt (the (single-float 0f0) vz))))
  out)

(u:fn-> sqrt (vec) vec)
(u:defun-inline sqrt (vec)
  (declare (optimize speed))
  (sqrt! (vec) vec))

(u:fn-> floor! (vec vec) vec)
(u:defun-inline floor! (out vec)
  (declare (optimize speed))
  (with-components ((o out) (v vec))
    (psetf ox (ffloor vx)
           oy (ffloor vy)
           oz (ffloor vz)))
  out)

(u:fn-> floor (vec) vec)
(u:defun-inline floor (vec)
  (declare (optimize speed))
  (floor! (vec) vec))

(u:fn-> ceiling! (vec vec) vec)
(u:defun-inline ceiling! (out vec)
  (declare (optimize speed))
  (with-components ((o out) (v vec))
    (psetf ox (fceiling vx)
           oy (fceiling vy)
           oz (fceiling vz)))
  out)

(u:fn-> ceiling (vec) vec)
(u:defun-inline ceiling (vec)
  (declare (optimize speed))
  (ceiling! (vec) vec))

(u:fn-> mod! (vec vec u:f32) vec)
(u:defun-inline mod! (out vec divisor)
  (declare (optimize speed))
  (with-components ((o out) (v vec))
    (psetf ox (nth-value 1 (ffloor vx divisor))
           oy (nth-value 1 (ffloor vy divisor))
           oz (nth-value 1 (ffloor vz divisor))))
  out)

(u:fn-> mod (vec u:f32) vec)
(u:defun-inline mod (vec divisor)
  (declare (optimize speed))
  (mod! (vec) vec divisor))

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
  (sin! (vec) vec))

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
  (cos! (vec) vec))

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
  (tan! (vec) vec))

(u:fn-> asin! (vec vec) vec)
(u:defun-inline asin! (out vec)
  (declare (optimize speed))
  (with-components ((o out) (v vec))
    (setf ox (cl:asin (the (single-float -1f0 1f0) vx))
          oy (cl:asin (the (single-float -1f0 1f0) vy))
          oz (cl:asin (the (single-float -1f0 1f0) vz))))
  out)

(u:fn-> asin (vec) vec)
(u:defun-inline asin (vec)
  (declare (optimize speed))
  (asin! (vec) vec))

(u:fn-> acos! (vec vec) vec)
(u:defun-inline acos! (out vec)
  (declare (optimize speed))
  (with-components ((o out) (v vec))
    (setf ox (cl:acos (the (single-float -1f0 1f0) vx))
          oy (cl:acos (the (single-float -1f0 1f0) vy))
          oz (cl:acos (the (single-float -1f0 1f0) vz))))
  out)

(u:fn-> acos (vec) vec)
(u:defun-inline acos (vec)
  (declare (optimize speed))
  (acos! (vec) vec))

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
  (atan! (vec) vec))

(u:fn-> velocity! (vec vec u:f32) vec)
(u:defun-inline velocity! (vec axis rate)
  (declare (optimize speed))
  (copy! vec axis)
  (normalize! vec vec)
  (scale! vec vec rate))

(u:fn-> velocity (vec u:f32) vec)
(u:defun-inline velocity (axis rate)
  (velocity! (vec) axis rate))
