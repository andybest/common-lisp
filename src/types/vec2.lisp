(in-package #:cl-user)

(defpackage #:net.mfiano.lisp.origin.vec2
  (:local-nicknames
   (#:com #:net.mfiano.lisp.origin.common)
   (#:const #:net.mfiano.lisp.origin.constants)
   (#:ss #:specialization-store)
   (#:u #:net.mfiano.lisp.golden-utils))
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
   #:with-components
   #:+zero+
   #:+up+
   #:+down+
   #:+left+
   #:+right+
   #:zero
   #:zero!
   #:zero-p
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
   #:atan
   #:velocity!
   #:velocity))

(in-package #:net.mfiano.lisp.origin.vec2)

(deftype vec () '(simple-array u:f32 (2)))

(defmacro with-components (((prefix vec) &rest rest) &body body)
  (u:once-only (vec)
    `(symbol-macrolet
         ((,prefix ,vec)
          (,(com:make-accessor-symbol prefix "X") (aref ,vec 0))
          (,(com:make-accessor-symbol prefix "Y") (aref ,vec 1)))
       ,(if rest
            `(with-components ,rest ,@body)
            `(progn ,@body)))))

;;; constructors

;; Low-level function for creating a vector. This is not exported, as it is
;; requires passing a total set of scalars, which is not as convenient as the
;; specializations that follow this definition.
(u:fn-> %vec (u:f32 u:f32) vec)
(declaim (inline %vec))
(u:eval-always
  (defun %vec (x y)
    (declare (optimize speed))
    (let ((vec (make-array 2 :element-type 'u:f32)))
      (setf (aref vec 0) x
            (aref vec 1) y)
      vec)))

;;; Define a set of specializations for creating vectors from a variety of
;;; different inputs.

(ss:defstore vec (&rest args))

(ss:defspecialization (vec :inline t) () vec
  (%vec 0.0 0.0))

(ss:defspecialization (vec :inline t) ((x real)) vec
  (%vec (float x 1.0) (float x 1.0)))

(ss:defspecialization (vec :inline t) ((x real) (y real)) vec
  (%vec (float x 1.0) (float y 1.0)))

(ss:defspecialization (vec :inline t)
    ((vec (or (simple-array u:f32 (2))
              (simple-array u:f32 (3))
              (simple-array u:f32 (4)))))
    vec
  (%vec (aref vec 0) (aref vec 1)))

(ss:defspecialization (vec :inline t)
    ((vec (or (simple-array u:f64 (2))
              (simple-array u:f64 (3))
              (simple-array u:f64 (4)))))
    vec
  (%vec (float (aref vec 0) 1.0) (float (aref vec 1) 1.0)))

;;; accessors

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

;;; constants

(u:define-constant +zero+ (%vec 0.0 0.0)
  :test #'equalp
  :documentation "Constant representing a 2D zero vector.")

(u:define-constant +up+ (%vec 0.0 1.0)
  :test #'equalp
  :documentation "Constant representing a 2D unit vector facing up.")

(u:define-constant +down+ (%vec 0.0 -1.0)
  :test #'equalp
  :documentation "Constant representing a 2D unit vector facing down.")

(u:define-constant +left+ (%vec -1.0 0.0)
  :test #'equalp
  :documentation "Constant representing a 2D unit vector facing left.")

(u:define-constant +right+ (%vec 1.0 0.0)
  :test #'equalp
  :documentation "Constant representing a 2D unit vector facing right.")
