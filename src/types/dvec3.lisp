(in-package #:cl-user)

(defpackage #:net.mfiano.lisp.origin.dvec3
  (:local-nicknames
   (#:dv2 #:net.mfiano.lisp.origin.dvec2)
   (#:com #:net.mfiano.lisp.origin.common)
   (#:ss #:specialization-store)
   (#:u #:net.mfiano.lisp.golden-utils)
   (#:v3 #:net.mfiano.lisp.origin.vec3))
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
   #:with-components
   #:+zero+
   #:+up+
   #:+down+
   #:+left+
   #:+right+
   #:+forward+
   #:+back+
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
   #:distance-squared
   #:distance
   #:normalize!
   #:normalize
   #:round!
   #:round
   #:abs!
   #:abs
   #:negate!
   #:negate
   #:cross!
   #:cross
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

(in-package #:net.mfiano.lisp.origin.dvec3)

(deftype vec () '(simple-array u:f64 (3)))

(defmacro with-components (((prefix vec) &rest rest) &body body)
  (u:once-only (vec)
    `(symbol-macrolet
         ((,prefix ,vec)
          (,(com:make-accessor-symbol prefix "X") (aref ,vec 0))
          (,(com:make-accessor-symbol prefix "Y") (aref ,vec 1))
          (,(com:make-accessor-symbol prefix "Z") (aref ,vec 2)))
       ,(if rest
            `(with-components ,rest ,@body)
            `(progn ,@body)))))

;;; constructors

(u:fn-> %vec (u:f64 u:f64 u:f64) vec)
(declaim (inline %vec))
(u:eval-always
  (defun %vec (x y z)
    (declare (optimize speed))
    (let ((vec (make-array 3 :element-type 'u:f64)))
      (setf (aref vec 0) x
            (aref vec 1) y
            (aref vec 2) z)
      vec)))

(ss:defstore vec (&rest args))

(ss:defspecialization (vec :inline t) () vec
  (%vec 0d0 0d0 0d0))

(ss:defspecialization (vec :inline t) ((x real)) vec
  (%vec (float x 1d0) (float x 1d0) (float x 1d0)))

(ss:defspecialization (vec :inline t) ((x real) (y real)) vec
  (%vec (float x 1d0) (float y 1d0) 0d0))

(ss:defspecialization (vec :inline t) ((x real) (y real) (z real)) vec
  (%vec (float x 1d0) (float y 1d0) (float z 1d0)))

(ss:defspecialization (vec :inline t) ((vec dv2:vec)) vec
  (%vec (aref vec 0) (aref vec 1) 0d0))

(ss:defspecialization (vec :inline t)
    ((vec (or (simple-array u:f64 (3))
              (simple-array u:f64 (4)))))
    vec
  (%vec (aref vec 0) (aref vec 1) (aref vec 2)))

(ss:defspecialization (vec :inline t) ((vec dv2:vec) (z real)) vec
  (%vec (aref vec 0) (aref vec 1) (float z 1d0)))

(ss:defspecialization (vec :inline t) ((x real) (vec dv2:vec)) vec
  (%vec (float x 1d0) (aref vec 0) (aref vec 1)))

(ss:defspecialization (vec :inline t)
    ((vec (or (simple-array u:f32 (3))
              (simple-array u:f32 (4)))))
    vec
  (%vec (float (aref vec 0) 1d0)
        (float (aref vec 1) 1d0)
        (float (aref vec 2) 1d0)))

;;; accessors

(u:fn-> x (vec) u:f64)
(declaim (inline x))
(defun x (vec)
  (aref vec 0))

(u:fn-> (setf x) (u:f64 vec) u:f64)
(declaim (inline (setf x)))
(defun (setf x) (value vec)
  (setf (aref vec 0) value))

(u:fn-> y (vec) u:f64)
(declaim (inline y))
(defun y (vec)
  (aref vec 1))

(u:fn-> (setf y) (u:f64 vec) u:f64)
(declaim (inline (setf y)))
(defun (setf y) (value vec)
  (setf (aref vec 1) value))

(u:fn-> z (vec) u:f64)
(declaim (inline z))
(defun z (vec)
  (aref vec 2))

(u:fn-> (setf z) (u:f64 vec) u:f64)
(declaim (inline (setf z)))
(defun (setf z) (value vec)
  (setf (aref vec 2) value))

;; ;;; constants

(u:define-constant +zero+ (%vec 0d0 0d0 0d0) :test #'equalp)

(u:define-constant +up+ (%vec 0d0 1d0 0d0) :test #'equalp)

(u:define-constant +down+ (%vec 0d0 -1d0 0d0) :test #'equalp)

(u:define-constant +left+ (%vec -1d0 0d0 0d0) :test #'equalp)

(u:define-constant +right+ (%vec 1d0 0d0 0d0) :test #'equalp)

(u:define-constant +forward+ (%vec 0d0 0d0 1d0) :test #'equalp)

(u:define-constant +back+ (%vec 0d0 0d0 -1d0) :test #'equalp)
