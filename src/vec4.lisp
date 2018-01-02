(in-package :box.math.vec4)

;;; Structure

(deftype vec () '(simple-array single-float (4)))

(defstruct (vec (:type (vector single-float))
                (:constructor %vec (x y z w))
                (:conc-name nil)
                (:copier nil))
  (x 0.0f0 :type single-float)
  (y 0.0f0 :type single-float)
  (z 0.0f0 :type single-float)
  (w 0.0f0 :type single-float))

(defmacro with-components (((prefix vec) &rest rest) &body body)
  `(with-accessors ((,prefix identity)
                    (,(%make-accessor-symbol prefix 'x) x)
                    (,(%make-accessor-symbol prefix 'y) y)
                    (,(%make-accessor-symbol prefix 'z) z)
                    (,(%make-accessor-symbol prefix 'w) w))
       ,vec
     ,(if rest
          `(with-components ,rest ,@body)
          `(progn ,@body))))

(set-pprint-dispatch
 'vec
 (lambda (stream object)
   (print-unreadable-object (object stream)
     (with-components ((v object))
       (format stream "~f ~f ~f ~f" vx vy vz vw))))
 1)

;;; Constants

(alexandria:define-constant +zero+
    (make-array 4 :element-type 'single-float
                  :initial-contents '(0.0f0 0.0f0 0.0f0 0.0f0))
  :test #'equalp)

;;; Operations

(declaim (inline vec))
(defun* (vec -> vec) ((x real) (y real) (z real) (w real))
  (%vec (float x 1.0f0) (float y 1.0f0) (float z 1.0f0) (float w 1.0f0)))

(declaim (inline zero!))
(defun* (zero! -> vec) ((vec vec))
  (with-components ((v vec))
    (psetf vx 0.0f0 vy 0.0f0 vz 0.0f0 vw 0.0f0))
  vec)

(declaim (inline zero))
(defun* (zero -> vec) ()
  (vec 0 0 0 0))

(declaim (inline zerop))
(defun* (zerop -> boolean) ((vec vec))
  (with-components ((v vec))
    (and (cl:zerop vx)
         (cl:zerop vy)
         (cl:zerop vz)
         (cl:zerop vw))))

(declaim (inline copy!))
(defun* (copy! -> vec) ((out vec) (vec vec))
  (with-components ((o out) (v vec))
    (psetf ox vx oy vy oz vz ow vw))
  out)

(declaim (inline copy))
(defun* (copy -> vec) ((vec vec))
  (copy! (zero) vec))

(declaim (inline clamp!))
(defun* (clamp! -> vec) ((out vec) (vec vec)
                         &key
                         ((min single-float) most-negative-single-float)
                         ((max single-float) most-positive-single-float))
  (with-components ((o out) (v vec))
    (psetf ox (alexandria:clamp vx min max)
           oy (alexandria:clamp vy min max)
           oz (alexandria:clamp vz min max)
           ow (alexandria:clamp vw min max)))
  out)

(declaim (inline clamp))
(defun* (clamp -> vec) ((vec vec)
                        &key
                        ((min single-float) most-negative-single-float)
                        ((max single-float) most-positive-single-float))
  (clamp! (zero) vec :min min :max max))

(declaim (inline stabilize!))
(defun* (stabilize! -> vec) ((out vec) (vec vec)
                             &key
                             ((tolerance single-float) +epsilon+))
  (with-components ((o out) (v vec))
    (macrolet ((stabilize (place)
                 `(if (cl:< (cl:abs ,place) tolerance) 0.0f0 ,place)))
      (psetf ox (stabilize vx)
             oy (stabilize vy)
             oz (stabilize vz)
             ow (stabilize vw))))
  out)

(declaim (inline stabilize))
(defun* (stabilize -> vec) ((vec vec) &key ((tolerance single-float) +epsilon+))
  (stabilize! (zero) vec :tolerance tolerance))

(declaim (inline ->list))
(defun* (->list -> list) ((vec vec))
  (with-components ((v vec))
    (list vx vy vz vw)))

(declaim (inline =))
(defun* (= -> boolean) ((vec1 vec) (vec2 vec))
  (with-components ((v1 vec1) (v2 vec2))
    (and (cl:= v1x v2x)
         (cl:= v1y v2y)
         (cl:= v1z v2z)
         (cl:= v1w v2w))))

(declaim (inline ~))
(defun* (~ -> boolean) ((vec1 vec) (vec2 vec)
                        &key
                        ((tolerance single-float) +epsilon+))
  (with-components ((v1 vec1) (v2 vec2))
    (and (%~ v1x v2x tolerance)
         (%~ v1y v2y tolerance)
         (%~ v1z v2z tolerance)
         (%~ v1w v2w tolerance))))

(declaim (inline +!))
(defun* (+! -> vec) ((out vec) (vec1 vec) (vec2 vec))
  (with-components ((o out) (v1 vec1) (v2 vec2))
    (psetf ox (cl:+ v1x v2x)
           oy (cl:+ v1y v2y)
           oz (cl:+ v1z v2z)
           ow (cl:+ v1w v2w)))
  out)

(declaim (inline +))
(defun* (+ -> vec) ((vec1 vec) (vec2 vec))
  (+! (zero) vec1 vec2))

(declaim (inline -!))
(defun* (-! -> vec) ((out vec) (vec1 vec) (vec2 vec))
  (with-components ((o out) (v1 vec1) (v2 vec2))
    (psetf ox (cl:- v1x v2x)
           oy (cl:- v1y v2y)
           oz (cl:- v1z v2z)
           ow (cl:- v1w v2w)))
  out)

(declaim (inline -))
(defun* (- -> vec) ((vec1 vec) (vec2 vec))
  (-! (zero) vec1 vec2))

(declaim (inline hadamard*!))
(defun* (hadamard*! -> vec) ((out vec) (vec1 vec) (vec2 vec))
  (with-components ((o out) (v1 vec1) (v2 vec2))
    (psetf ox (cl:* v1x v2x)
           oy (cl:* v1y v2y)
           oz (cl:* v1z v2z)
           ow (cl:* v1w v2w)))
  out)

(declaim (inline hadamard*))
(defun* (hadamard* -> vec) ((vec1 vec) (vec2 vec))
  (hadamard*! (zero) vec1 vec2))

(declaim (inline hadamard/!))
(defun* (hadamard/! -> vec) ((out vec) (vec1 vec) (vec2 vec))
  (with-components ((o out) (v1 vec1) (v2 vec2))
    (psetf ox (if (cl:zerop v2x) 0.0f0 (/ v1x v2x))
           oy (if (cl:zerop v2y) 0.0f0 (/ v1y v2y))
           oz (if (cl:zerop v2z) 0.0f0 (/ v1z v2z))
           ow (if (cl:zerop v2w) 0.0f0 (/ v1w v2w))))
  out)

(declaim (inline hadamard/))
(defun* (hadamard/ -> vec) ((vec1 vec) (vec2 vec))
  (hadamard/! (zero) vec1 vec2))

(declaim (inline scale!))
(defun* (scale! -> vec) ((out vec) (vec vec) (scalar single-float))
  (with-components ((o out) (v vec))
    (psetf ox (cl:* vx scalar)
           oy (cl:* vy scalar)
           oz (cl:* vz scalar)
           ow (cl:* vw scalar)))
  out)

(declaim (inline scale))
(defun* (scale -> vec) ((vec vec) (scalar single-float))
  (scale! (zero) vec scalar))

(declaim (inline dot))
(defun* (dot -> single-float) ((vec1 vec) (vec2 vec))
  (with-components ((v1 vec1) (v2 vec2))
    (cl:+ (cl:* v1x v2x) (cl:* v1y v2y) (cl:* v1z v2z) (cl:* v1w v2w))))

(declaim (inline magnitude-squared))
(defun* (magnitude-squared -> single-float) ((vec vec))
  (dot vec vec))

(declaim (inline magnitude))
(defun* (magnitude -> single-float) ((vec vec))
  (sqrt (magnitude-squared vec)))

(declaim (inline normalize!))
(defun* (normalize! -> vec) ((out vec) (vec vec))
  (let ((magnitude (magnitude vec)))
    (unless (cl:zerop magnitude)
      (scale! out vec (/ magnitude))))
  out)

(declaim (inline normalize))
(defun* (normalize -> vec) ((vec vec))
  (normalize! (zero) vec))

(declaim (inline round!))
(defun* (round! -> vec) ((out vec) (vec vec))
  (with-components ((o out) (v vec))
    (psetf ox (fround vx)
           oy (fround vy)
           oz (fround vz)
           ow (fround vw)))
  out)

(declaim (inline round))
(defun* (round -> vec) ((vec vec))
  (round! (zero) vec))

(declaim (inline abs!))
(defun* (abs! -> vec) ((out vec) (vec vec))
  (with-components ((o out) (v vec))
    (psetf ox (cl:abs vx)
           oy (cl:abs vy)
           oz (cl:abs vz)
           ow (cl:abs vw)))
  out)

(declaim (inline abs))
(defun* (abs -> vec) ((vec vec))
  (abs! (zero) vec))

(declaim (inline negate!))
(defun* (negate! -> vec) ((out vec) (vec vec))
  (scale! out vec -1.0f0))

(declaim (inline negate))
(defun* (negate -> vec) ((vec vec))
  (negate! (zero) vec))

(declaim (inline angle))
(defun* (angle -> single-float) ((vec1 vec) (vec2 vec))
  (let ((dot (dot vec1 vec2))
        (m*m (cl:* (magnitude vec1) (magnitude vec2))))
    (if (cl:zerop m*m) 0.0f0 (acos (/ dot m*m)))))

(declaim (inline lerp!))
(defun* (lerp! -> vec) ((out vec) (vec1 vec) (vec2 vec) (factor single-float))
  (with-components ((o out) (v1 vec1) (v2 vec2))
    (psetf ox (alexandria:lerp factor v1x v2x)
           oy (alexandria:lerp factor v1y v2y)
           oz (alexandria:lerp factor v1z v2z)
           ow (alexandria:lerp factor v1w v2w)))
  out)

(declaim (inline lerp))
(defun* (lerp -> vec) ((vec1 vec) (vec2 vec) (factor single-float))
  (lerp! (zero) vec1 vec2 factor))

(declaim (inline <))
(defun* (< -> boolean) ((vec1 vec) (vec2 vec))
  (with-components ((v1 vec1) (v2 vec2))
    (and (cl:< v1x v2x)
         (cl:< v1y v2y)
         (cl:< v1z v2z)
         (cl:< v1w v2w))))

(declaim (inline <=))
(defun* (<= -> boolean) ((vec1 vec) (vec2 vec))
  (with-components ((v1 vec1) (v2 vec2))
    (and (cl:<= v1x v2x)
         (cl:<= v1y v2y)
         (cl:<= v1z v2z)
         (cl:<= v1w v2w))))

(declaim (inline >))
(defun* (> -> boolean) ((vec1 vec) (vec2 vec))
  (with-components ((v1 vec1) (v2 vec2))
    (and (cl:> v1x v2x)
         (cl:> v1y v2y)
         (cl:> v1z v2z)
         (cl:> v1w v2w))))

(declaim (inline >=))
(defun* (>= -> boolean) ((vec1 vec) (vec2 vec))
  (with-components ((v1 vec1) (v2 vec2))
    (and (cl:>= v1x v2x)
         (cl:>= v1y v2y)
         (cl:>= v1z v2z)
         (cl:>= v1w v2w))))

(declaim (inline min!))
(defun* (min! -> vec) ((out vec) (vec1 vec) (vec2 vec))
  (with-components ((o out) (v1 vec1) (v2 vec2))
    (psetf ox (cl:min v1x v2x)
           oy (cl:min v1y v2y)
           oz (cl:min v1z v2z)
           ow (cl:min v1w v2w)))
  out)

(declaim (inline min))
(defun* (min -> vec) ((vec1 vec) (vec2 vec))
  (min! (zero) vec1 vec2))

(declaim (inline max!))
(defun* (max! -> vec) ((out vec) (vec1 vec) (vec2 vec))
  (with-components ((o out) (v1 vec1) (v2 vec2))
    (psetf ox (cl:max v1x v2x)
           oy (cl:max v1y v2y)
           oz (cl:max v1z v2z)
           ow (cl:max v1w v2w)))
  out)

(declaim (inline max))
(defun* (max -> vec) ((vec1 vec) (vec2 vec))
  (max! (zero) vec1 vec2))
