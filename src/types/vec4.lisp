(in-package #:cl-user)

(defpackage #:origin.vec4
  (:local-nicknames
   (#:com #:origin.common)
   (#:const #:origin.constants)
   (#:ss #:specialization-store)
   (#:u #:golden-utils)
   (#:v2 #:origin.vec2)
   (#:v3 #:origin.vec3))
  (:use #:cl)
  (:shadow
   #:=
   #:+
   #:-
   #:*
   #:/
   #:random
   #:length
   #:round
   #:abs
   #:<
   #:<=
   #:>
   #:>=
   #:min
   #:max
   #:expt
   #:sqrt
   #:floor
   #:ceiling
   #:mod
   #:sin
   #:cos
   #:tan
   #:asin
   #:acos
   #:atan)
  (:export
   #:vec
   #:x
   #:y
   #:z
   #:w
   #:with-components
   #:+zero+
   #:+ones+
   #:zero
   #:zero!
   #:zero-p
   #:ones!
   #:ones
   #:uniform!
   #:uniform
   #:random!
   #:random
   #:copy!
   #:copy
   #:sign!
   #:sign
   #:fract!
   #:fract
   #:clamp!
   #:clamp
   #:clamp-range!
   #:clamp-range
   #:=
   #:+!
   #:+
   #:-!
   #:-
   #:*!
   #:*
   #:/!
   #:/
   #:scale!
   #:scale
   #:invert!
   #:invert
   #:dot
   #:length-squared
   #:length
   #:normalize!
   #:normalize
   #:round!
   #:round
   #:abs!
   #:abs
   #:negate!
   #:negate
   #:angle
   #:direction=
   #:parallel-p
   #:lerp!
   #:lerp
   #:<
   #:<=
   #:>
   #:>=
   #:min!
   #:min
   #:max!
   #:max
   #:radians!
   #:radians
   #:degrees!
   #:degrees
   #:expt!
   #:expt
   #:sqrt!
   #:sqrt
   #:floor!
   #:floor
   #:ceiling!
   #:ceiling
   #:mod!
   #:mod
   #:sin!
   #:sin
   #:cos!
   #:cos
   #:tan!
   #:tan
   #:asin!
   #:asin
   #:acos!
   #:acos
   #:atan!
   #:atan))

(in-package #:origin.vec4)

(deftype vec () '(u:f32a 4))

(defmacro with-components (((prefix vec) &rest rest) &body body)
  (u:once-only (vec)
    `(symbol-macrolet
         ((,prefix ,vec)
          (,(com:make-accessor-symbol prefix "X") (aref ,vec 0))
          (,(com:make-accessor-symbol prefix "Y") (aref ,vec 1))
          (,(com:make-accessor-symbol prefix "Z") (aref ,vec 2))
          (,(com:make-accessor-symbol prefix "W") (aref ,vec 3)))
       ,(if rest
            `(with-components ,rest ,@body)
            `(progn ,@body)))))

;;; Constructors

;; Low-level function for creating a vector. This is not exported, as it is requires passing a total
;; set of scalars, which is not as convenient as the specializations that follow this definition.
(u:fn-> %vec (u:f32 u:f32 u:f32 u:f32) vec)
(declaim (inline %vec))
(u:eval-always
  (defun %vec (x y z w)
    (declare (optimize speed))
    (let ((vec (u:make-f32-array 4)))
      (setf (aref vec 0) x
            (aref vec 1) y
            (aref vec 2) z
            (aref vec 3) w)
      vec)))

;;; Define a set of specializations for creating vectors from a variety of different inputs.

(ss:defstore vec (&rest args))

(ss:defspecialization (vec :inline t) () vec
  (%vec 0.0 0.0 0.0 0.0))

(ss:defspecialization (vec :inline t) ((x real)) vec
  (%vec (float x 1.0) (float x 1.0) (float x 1.0) (float x 1.0)))

(ss:defspecialization (vec :inline t) ((x real) (y real)) vec
  (%vec (float x 1.0) (float y 1.0) 0.0 0.0))

(ss:defspecialization (vec :inline t) ((x real) (y real) (z real)) vec
  (%vec (float x 1.0) (float y 1.0) (float z 1.0) 0.0))

(ss:defspecialization (vec :inline t) ((x real) (y real) (z real) (w real)) vec
  (%vec (float x 1.0) (float y 1.0) (float z 1.0) (float w 1.0)))

(ss:defspecialization (vec :inline t) ((vec v2:vec)) vec
  (%vec (aref vec 0) (aref vec 1) 0.0 0.0))

(ss:defspecialization (vec :inline t) ((vec v3:vec)) vec
  (%vec (aref vec 0) (aref vec 1) (aref vec 2) 0.0))

(ss:defspecialization (vec :inline t) ((vec vec)) vec
  (%vec (aref vec 0) (aref vec 1) (aref vec 2) (aref vec 3)))

(ss:defspecialization (vec :inline t) ((vec v2:vec) (z real)) vec
  (%vec (aref vec 0) (aref vec 1) (float z 1.0) 0.0))

(ss:defspecialization (vec :inline t) ((x real) (vec v2:vec)) vec
  (%vec (float x 1.0) (aref vec 0) (aref vec 1) 0.0))

(ss:defspecialization (vec :inline t) ((vec1 v2:vec) (vec2 v2:vec)) vec
  (%vec (aref vec1 0) (aref vec1 1) (aref vec2 0) (aref vec2 1)))

(ss:defspecialization (vec :inline t) ((x real) (vec v3:vec)) vec
  (%vec (float x 1.0) (aref vec 0) (aref vec 1) (aref vec 2)))

(ss:defspecialization (vec :inline t) ((vec v3:vec) (w real)) vec
  (%vec (aref vec 0) (aref vec 1) (aref vec 2) (float w 1.0)))

(ss:defspecialization (vec :inline t) ((vec v2:vec) (z real) (w real)) vec
  (%vec (aref vec 0) (aref vec 1) (float z 1.0) (float w 1.0)))

(ss:defspecialization (vec :inline t) ((x real) (y real) (vec v2:vec)) vec
  (%vec (float x 1.0) (float y 1.0) (aref vec 0) (aref vec 1)))

(ss:defspecialization (vec :inline t) ((x real) (vec v2:vec) (w real)) vec
  (%vec (float x 1.0) (aref vec 0) (aref vec 1) (float w 1.0)))

(ss:defspecialization (vec :inline t) ((vec (u:f64a 4))) vec
  (%vec (float (aref vec 0) 1.0)
        (float (aref vec 1) 1.0)
        (float (aref vec 2) 1.0)
        (float (aref vec 3) 1.0)))

;;; Accessors

(u:fn-> x (vec) u:f32)
(declaim (inline x))
(defun x (vec)
  "Read the 'X' component of vector VEC."
  (declare (optimize speed))
  (aref vec 0))

(u:fn-> (setf x) (u:f32 vec) u:f32)
(declaim (inline (setf x)))
(defun (setf x) (value vec)
  "Write VALUE to the 'X' component of vector VEC."
  (declare (optimize speed))
  (setf (aref vec 0) value))

(u:fn-> y (vec) u:f32)
(declaim (inline y))
(defun y (vec)
  "Read the 'Y' component of vector VEC."
  (declare (optimize speed))
  (aref vec 1))

(u:fn-> (setf y) (u:f32 vec) u:f32)
(declaim (inline (setf y)))
(defun (setf y) (value vec)
  "Write VALUE to the 'Y' component of vector VEC."
  (declare (optimize speed))
  (setf (aref vec 1) value))

(u:fn-> z (vec) u:f32)
(declaim (inline z))
(defun z (vec)
  "Read the 'Z' component of vector VEC."
  (declare (optimize speed))
  (aref vec 2))

(u:fn-> (setf z) (u:f32 vec) u:f32)
(declaim (inline (setf z)))
(defun (setf z) (value vec)
  "Write VALUE to the 'Z' component of vector VEC."
  (declare (optimize speed))
  (setf (aref vec 2) value))

(u:fn-> w (vec) u:f32)
(declaim (inline w))
(defun w (vec)
  "Read the 'W' component of vector VEC."
  (declare (optimize speed))
  (aref vec 3))

(u:fn-> (setf w) (u:f32 vec) u:f32)
(declaim (inline (setf w)))
(defun (setf w) (value vec)
  "Write VALUE to the 'W' component of vector VEC."
  (declare (optimize speed))
  (setf (aref vec 3) value))

;;; Constants

(u:define-constant +zero+ (%vec 0.0 0.0 0.0 0.0)
  :test #'equalp
  :documentation "Constant representing a 4D zero vector.")

(u:define-constant +ones+ (%vec 1.0 1.0 1.0 1.0)
  :test #'equalp
  :documentation "Constant representing a 4D vector with each component being 1.")
