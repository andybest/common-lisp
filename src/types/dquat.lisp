(in-package #:cl-user)

(defpackage #:net.mfiano.lisp.origin.dquat
  (:local-nicknames
   (#:com #:net.mfiano.lisp.origin.common)
   (#:dm3 #:net.mfiano.lisp.origin.dmat3)
   (#:dm4 #:net.mfiano.lisp.origin.dmat4)
   (#:dv3 #:net.mfiano.lisp.origin.dvec3)
   (#:dv4 #:net.mfiano.lisp.origin.dvec4)
   (#:q #:net.mfiano.lisp.origin.quat)
   (#:ss #:specialization-store)
   (#:u #:net.mfiano.lisp.golden-utils))
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
   #:with-elements
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

(in-package #:net.mfiano.lisp.origin.dquat)

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

(defmacro with-elements (((prefix w x y z) &rest rest) &body body)
  (let ((%w (com:make-accessor-symbol prefix "W"))
        (%x (com:make-accessor-symbol prefix "X"))
        (%y (com:make-accessor-symbol prefix "Y"))
        (%z (com:make-accessor-symbol prefix "Z")))
    `(let ((,%w ,w) (,%x ,x) (,%y ,y) (,%z ,z))
       (declare (ignorable ,%w ,%x ,%y ,%z))
       ,(if rest
            `(with-elements ,rest ,@body)
            `(progn ,@body)))))

;;; constructors

(u:fn-> %quat (&rest u:f64) quat)
(declaim (inline %quat))
(u:eval-always
  (defun %quat (&rest args)
    (declare (optimize speed))
    (make-array 4 :element-type 'u:f64 :initial-contents args)))

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
