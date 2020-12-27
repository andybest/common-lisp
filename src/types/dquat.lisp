(in-package #:cl-user)

(defpackage #:origin.dquat
  (:local-nicknames
   (#:com #:origin.common)
   (#:dm3 #:origin.dmat3)
   (#:dm4 #:origin.dmat4)
   (#:dv3 #:origin.dvec3)
   (#:dv4 #:origin.dvec4)
   (#:q #:origin.quat)
   (#:ss #:specialization-store)
   (#:u #:golden-utils))
  (:use #:cl)
  (:shadow
   #:=
   #:+
   #:-
   #:*
   #:conjugate
   #:length
   #:random)
  (:export
   #:quat
   #:with-components
   #:w
   #:x
   #:y
   #:z
   #:+id+
   #:id
   #:id!
   #:id-p
   #:=
   #:random!
   #:random
   #:copy!
   #:copy
   #:+!
   #:+
   #:-!
   #:-
   #:*!
   #:*
   #:scale!
   #:scale
   #:conjugate!
   #:conjugate
   #:cross!
   #:cross
   #:length-squared
   #:length
   #:normalize!
   #:normalize
   #:negate!
   #:negate
   #:dot
   #:inverse!
   #:inverse
   #:rotate-euler!
   #:rotate-euler
   #:rotate!
   #:rotate
   #:to-euler!
   #:to-euler
   #:to-mat3!
   #:to-mat3
   #:to-mat4!
   #:to-mat4
   #:from-mat3!
   #:from-mat3
   #:from-mat4!
   #:from-mat4
   #:slerp!
   #:slerp
   #:from-axis-angle!
   #:from-axis-angle
   #:orient!
   #:orient
   #:from-velocity!
   #:from-velocity))

(in-package #:origin.dquat)

(deftype quat () '(simple-array u:f64 (4)))

(defmacro with-components (((prefix quat) &rest rest) &body body)
  (u:once-only (quat)
    `(symbol-macrolet
         ((,prefix ,quat)
          (,(com:make-accessor-symbol prefix "W") (aref ,quat 0))
          (,(com:make-accessor-symbol prefix "X") (aref ,quat 1))
          (,(com:make-accessor-symbol prefix "Y") (aref ,quat 2))
          (,(com:make-accessor-symbol prefix "Z") (aref ,quat 3)))
       ,(if rest
            `(with-components ,rest ,@body)
            `(progn ,@body)))))

;;; constructors

(u:fn-> %quat (u:f64 u:f64 u:f64 u:f64) quat)
(declaim (inline %quat))
(u:eval-always
  (defun %quat (w x y z)
    (declare (optimize speed))
    (let ((quat (make-array 4 :element-type 'u:f64)))
      (setf (aref quat 0) w
            (aref quat 1) x
            (aref quat 2) y
            (aref quat 3) z)
      quat)))

(ss:defstore quat (&rest args))

(ss:defspecialization (quat :inline t) () quat
  (%quat 1d0 0d0 0d0 0d0))

(ss:defspecialization (quat :inline t) ((w real)) quat
  (%quat (float w 1d0) 0d0 0d0 0d0))

(ss:defspecialization (quat :inline t) ((w real) (x real) (y real) (z real))
    quat
  (%quat (float w 1d0) (float x 1d0) (float y 1d0) (float z 1d0)))

(ss:defspecialization (quat :inline t) ((quat q:quat)) quat
  (%quat (float (aref quat 0) 1d0)
         (float (aref quat 1) 1d0)
         (float (aref quat 2) 1d0)
         (float (aref quat 3) 1d0)))

;;; accessors

(u:fn-> w (quat) u:f64)
(declaim (inline w))
(defun w (quat)
  (aref quat 0))

(u:fn-> (setf w) (u:f64 quat) u:f64)
(declaim (inline (setf w)))
(defun (setf w) (value quat)
  (setf (aref quat 0) value))

(u:fn-> x (quat) u:f64)
(declaim (inline x))
(defun x (quat)
  (aref quat 1))

(u:fn-> (setf x) (u:f64 quat) u:f64)
(declaim (inline (setf x)))
(defun (setf x) (value quat)
  (setf (aref quat 1) value))

(u:fn-> y (quat) u:f64)
(declaim (inline y))
(defun y (quat)
  (aref quat 2))

(u:fn-> (setf y) (u:f64 quat) u:f64)
(declaim (inline (setf y)))
(defun (setf y) (value quat)
  (setf (aref quat 2) value))

(u:fn-> z (quat) u:f64)
(declaim (inline z))
(defun z (quat)
  (aref quat 3))

(u:fn-> (setf z) (u:f64 quat) u:f64)
(declaim (inline (setf z)))
(defun (setf z) (value quat)
  (setf (aref quat 3) value))

;;; constants

(u:define-constant +id+ (%quat 1d0 0d0 0d0 0d0) :test #'equalp)
