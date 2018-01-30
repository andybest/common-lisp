(in-package :box.math.vec2)

;;; Structure

(deftype vec () '(simple-array single-float (2)))

(defstruct (vec (:type (vector single-float))
                (:constructor %make (x y))
                (:conc-name nil)
                (:copier nil))
  (x 0.0f0 :type single-float)
  (y 0.0f0 :type single-float))

(defmacro with-components (((prefix vec) &rest rest) &body body)
  `(with-accessors ((,prefix identity)
                    (,(%make-accessor-symbol prefix 'x) x)
                    (,(%make-accessor-symbol prefix 'y) y))
       ,vec
     ,(if rest
          `(with-components ,rest ,@body)
          `(progn ,@body))))

;;; Constants

(alexandria:define-constant +zero+
    (make-array 2 :element-type 'single-float :initial-contents '(0.0f0 0.0f0))
  :test #'equalp)

;;; Operations

(declaim (inline make))
(declaim (ftype (function (real real) vec) make))
(defun make (x y)
  (%make (float x 1.0f0) (float y 1.0f0)))

(declaim (inline zero!))
(declaim (ftype (function (vec) vec) zero!))
(defun zero! (vec)
  (with-components ((v vec))
    (psetf vx 0.0f0 vy 0.0f0))
  vec)

(declaim (inline zero))
(declaim (ftype (function () vec) zero))
(defun zero ()
  (make 0 0))

(declaim (inline zerop))
(declaim (ftype (function (vec) boolean) zerop))
(defun zerop (vec)
  (with-components ((v vec))
    (and (cl:zerop vx)
         (cl:zerop vy))))

(declaim (inline copy!))
(declaim (ftype (function (vec vec) vec) copy!))
(defun copy! (out vec)
  (with-components ((o out) (v vec))
    (psetf ox vx oy vy))
  out)

(declaim (inline copy))
(declaim (ftype (function (vec) vec) copy))
(defun copy (vec)
  (copy! (zero) vec))

(declaim (inline clamp!))
(declaim (ftype (function (vec vec &key (:min single-float) (:max single-float)) vec) clamp!))
(defun clamp! (out vec &key (min most-negative-single-float) (max most-positive-single-float))
  (with-components ((o out) (v vec))
    (psetf ox (alexandria:clamp vx min max)
           oy (alexandria:clamp vy min max)))
  out)

(declaim (inline clamp))
(declaim (ftype (function (vec &key (:min single-float) (:max single-float)) vec) clamp))
(defun clamp (vec &key (min most-negative-single-float) (max most-positive-single-float))
  (clamp! (zero) vec :min min :max max))

(declaim (inline stabilize!))
(declaim (ftype (function (vec vec &key (:tolerance single-float)) vec) stabilize!))
(defun stabilize! (out vec &key (tolerance +epsilon+))
  (with-components ((o out) (v vec))
    (macrolet ((stabilize (place)
                 `(if (cl:< (cl:abs ,place) tolerance) 0.0f0 ,place)))
      (psetf ox (stabilize vx)
             oy (stabilize vy))))
  out)

(declaim (inline stabilize))
(declaim (ftype (function (vec &key (:tolerance single-float)) vec) stabilize))
(defun stabilize (vec &key (tolerance +epsilon+))
  (stabilize! (zero) vec :tolerance tolerance))

(declaim (inline to-list))
(declaim (ftype (function (vec) list) to-list))
(defun to-list (vec)
  (with-components ((v vec))
    (list vx vy)))

(declaim (inline from-list))
(declaim (ftype (function (list) vec) from-list))
(defun from-list (list)
  (apply #'make list))

(declaim (inline =))
(declaim (ftype (function (vec vec) boolean) =))
(defun = (vec1 vec2)
  (with-components ((v1 vec1) (v2 vec2))
    (and (cl:= v1x v2x)
         (cl:= v1y v2y))))

(declaim (inline ~))
(declaim (ftype (function (vec vec &key (:tolerance single-float)) boolean) ~))
(defun ~ (vec1 vec2 &key (tolerance +epsilon+))
  (with-components ((v1 vec1) (v2 vec2))
    (and (%~ v1x v2x tolerance)
         (%~ v1y v2y tolerance))))

(declaim (inline +!))
(declaim (ftype (function (vec vec vec) vec) +!))
(defun +! (out vec1 vec2)
  (with-components ((o out) (v1 vec1) (v2 vec2))
    (psetf ox (cl:+ v1x v2x)
           oy (cl:+ v1y v2y)))
  out)

(declaim (inline +))
(declaim (ftype (function (vec vec) vec) +))
(defun + (vec1 vec2)
  (+! (zero) vec1 vec2))

(declaim (inline -!))
(declaim (ftype (function (vec vec vec) vec) -!))
(defun -! (out vec1 vec2)
  (with-components ((o out) (v1 vec1) (v2 vec2))
    (psetf ox (cl:- v1x v2x)
           oy (cl:- v1y v2y)))
  out)

(declaim (inline -))
(declaim (ftype (function (vec vec) vec) -))
(defun - (vec1 vec2)
  (-! (zero) vec1 vec2))

(declaim (inline *!))
(declaim (ftype (function (vec vec vec) vec) *!))
(defun *! (out vec1 vec2)
  (with-components ((o out) (v1 vec1) (v2 vec2))
    (psetf ox (cl:* v1x v2x)
           oy (cl:* v1y v2y)))
  out)

(declaim (inline *))
(declaim (ftype (function (vec vec) vec) *))
(defun * (vec1 vec2)
  (*! (zero) vec1 vec2))

(declaim (inline /!))
(declaim (ftype (function (vec vec vec) vec) /!))
(defun /! (out vec1 vec2)
  (with-components ((o out) (v1 vec1) (v2 vec2))
    (psetf ox (if (cl:zerop v2x) 0.0f0 (cl:/ v1x v2x))
           oy (if (cl:zerop v2y) 0.0f0 (cl:/ v1y v2y))))
  out)

(declaim (inline /))
(declaim (ftype (function (vec vec) vec) /))
(defun / (vec1 vec2)
  (/! (zero) vec1 vec2))

(declaim (inline scale!))
(declaim (ftype (function (vec vec single-float) vec) scale!))
(defun scale! (out vec scalar)
  (with-components ((o out) (v vec))
    (psetf ox (cl:* vx scalar)
           oy (cl:* vy scalar)))
  out)

(declaim (inline scale))
(declaim (ftype (function (vec single-float) vec) scale))
(defun scale (vec scalar)
  (scale! (zero) vec scalar))

(declaim (inline dot))
(declaim (ftype (function (vec vec) single-float) dot))
(defun dot (vec1 vec2)
  (with-components ((v1 vec1) (v2 vec2))
    (cl:+ (cl:* v1x v2x) (cl:* v1y v2y))))

(declaim (inline magnitude-squared))
(declaim (ftype (function (vec) single-float) magnitude-squared))
(defun magnitude-squared (vec)
  (dot vec vec))

(declaim (inline magnitude))
(declaim (ftype (function (vec) single-float) magnitude))
(defun magnitude (vec)
  (sqrt (magnitude-squared vec)))

(declaim (inline normalize!))
(declaim (ftype (function (vec vec) vec) normalize!))
(defun normalize! (out vec)
  (let ((magnitude (magnitude vec)))
    (unless (cl:zerop magnitude)
      (scale! out vec (cl:/ magnitude))))
  out)

(declaim (inline normalize))
(declaim (ftype (function (vec) vec) normalize))
(defun normalize (vec)
  (normalize! (zero) vec))

(declaim (inline round!))
(declaim (ftype (function (vec vec) vec) round!))
(defun round! (out vec)
  (with-components ((o out) (v vec))
    (psetf ox (fround vx)
           oy (fround vy)))
  out)

(declaim (inline round))
(declaim (ftype (function (vec) vec) round))
(defun round (vec)
  (round! (zero) vec))

(declaim (inline abs!))
(declaim (ftype (function (vec vec) vec) abs!))
(defun abs! (out vec)
  (with-components ((o out) (v vec))
    (psetf ox (cl:abs vx)
           oy (cl:abs vy)))
  out)

(declaim (inline abs))
(declaim (ftype (function (vec) vec) abs))
(defun abs (vec)
  (abs! (zero) vec))

(declaim (inline negate!))
(declaim (ftype (function (vec vec) vec) negate!))
(defun negate! (out vec)
  (scale! out vec -1.0f0))

(declaim (inline negate))
(declaim (ftype (function (vec) vec) negate))
(defun negate (vec)
  (negate! (zero) vec))

(declaim (inline angle))
(declaim (ftype (function (vec vec) single-float) angle))
(defun angle (vec1 vec2)
  (let ((dot (dot vec1 vec2))
        (m*m (cl:* (magnitude vec1) (magnitude vec2))))
    (if (cl:zerop m*m) 0.0f0 (acos (cl:/ dot m*m)))))

(declaim (inline direction=))
(declaim (ftype (function (vec vec) boolean) direction=))
(defun direction= (vec1 vec2)
  (cl:>= (dot (normalize vec1) (normalize vec2)) (cl:- 1 +epsilon+)))

(declaim (inline lerp!))
(declaim (ftype (function (vec vec vec single-float) vec) lerp!))
(defun lerp! (out vec1 vec2 factor)
  (with-components ((o out) (v1 vec1) (v2 vec2))
    (psetf ox (alexandria:lerp factor v1x v2x)
           oy (alexandria:lerp factor v1y v2y)))
  out)

(declaim (inline lerp))
(declaim (ftype (function (vec vec single-float) vec) lerp))
(defun lerp (vec1 vec2 factor)
  (lerp! (zero) vec1 vec2 factor))

(declaim (inline <))
(declaim (ftype (function (vec vec) boolean) <))
(defun < (vec1 vec2)
  (with-components ((v1 vec1) (v2 vec2))
    (and (cl:< v1x v2x)
         (cl:< v1y v2y))))

(declaim (inline <=))
(declaim (ftype (function (vec vec) boolean) <=))
(defun <= (vec1 vec2)
  (with-components ((v1 vec1) (v2 vec2))
    (and (cl:<= v1x v2x)
         (cl:<= v1y v2y))))

(declaim (inline >))
(declaim (ftype (function (vec vec) boolean) >))
(defun > (vec1 vec2)
  (with-components ((v1 vec1) (v2 vec2))
    (and (cl:> v1x v2x)
         (cl:> v1y v2y))))

(declaim (inline >=))
(declaim (ftype (function (vec vec) boolean) >=))
(defun >= (vec1 vec2)
  (with-components ((v1 vec1) (v2 vec2))
    (and (cl:>= v1x v2x)
         (cl:>= v1y v2y))))

(declaim (inline min!))
(declaim (ftype (function (vec vec vec) vec) min!))
(defun min! (out vec1 vec2)
  (with-components ((o out) (v1 vec1) (v2 vec2))
    (psetf ox (cl:min v1x v2x)
           oy (cl:min v1y v2y)))
  out)

(declaim (inline min))
(declaim (ftype (function (vec vec) vec) min))
(defun min (vec1 vec2)
  (min! (zero) vec1 vec2))

(declaim (inline max!))
(declaim (ftype (function (vec vec vec) vec) max!))
(defun max! (out vec1 vec2)
  (with-components ((o out) (v1 vec1) (v2 vec2))
    (psetf ox (cl:max v1x v2x)
           oy (cl:max v1y v2y)))
  out)

(declaim (inline max))
(declaim (ftype (function (vec vec) vec) max))
(defun max (vec1 vec2)
  (max! (zero) vec1 vec2))
