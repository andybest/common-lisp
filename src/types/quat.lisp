(in-package #:cl-user)

(defpackage #:net.mfiano.lisp.origin.quat
  (:local-nicknames
   (#:com #:net.mfiano.lisp.origin.common)
   (#:m3 #:net.mfiano.lisp.origin.mat3)
   (#:m4 #:net.mfiano.lisp.origin.mat4)
   (#:ss #:specialization-store)
   (#:u #:net.mfiano.lisp.golden-utils)
   (#:v3 #:net.mfiano.lisp.origin.vec3)
   (#:v4 #:net.mfiano.lisp.origin.vec4))
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

(in-package #:net.mfiano.lisp.origin.quat)

(deftype quat () '(simple-array u:f32 (4)))

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

(u:fn-> %quat (&rest u:f32) quat)
(declaim (inline %quat))
(u:eval-always
  (defun %quat (&rest args)
    (declare (optimize speed))
    (make-array 4 :element-type 'u:f32 :initial-contents args)))

(ss:defstore quat (&rest args))

(ss:defspecialization (quat :inline t) () quat
  (%quat 1f0 0f0 0f0 0f0))

(ss:defspecialization (quat :inline t) ((w real)) quat
  (%quat (float w 1f0) 0f0 0f0 0f0))

(ss:defspecialization (quat :inline t) ((w real) (x real) (y real) (z real))
    quat
  (%quat (float w 1f0) (float x 1f0) (float y 1f0) (float z 1f0)))

(ss:defspecialization (quat :inline t) ((quat (simple-array u:f64 (4)))) quat
  (%quat (float (aref quat 0) 1f0)
         (float (aref quat 1) 1f0)
         (float (aref quat 2) 1f0)
         (float (aref quat 3) 1f0)))

;;; accessors

(u:fn-> w (quat) u:f32)
(declaim (inline w))
(defun w (quat)
  (declare (optimize speed))
  (aref quat 0))

(u:fn-> (setf w) (u:f32 quat) u:f32)
(declaim (inline (setf w)))
(defun (setf w) (value quat)
  (declare (optimize speed))
  (setf (aref quat 0) value))

(u:fn-> x (quat) u:f32)
(declaim (inline x))
(defun x (quat)
  (declare (optimize speed))
  (aref quat 1))

(u:fn-> (setf x) (u:f32 quat) u:f32)
(declaim (inline (setf x)))
(defun (setf x) (value quat)
  (declare (optimize speed))
  (setf (aref quat 1) value))

(u:fn-> y (quat) u:f32)
(declaim (inline y))
(defun y (quat)
  (declare (optimize speed))
  (aref quat 2))

(u:fn-> (setf y) (u:f32 quat) u:f32)
(declaim (inline (setf y)))
(defun (setf y) (value quat)
  (declare (optimize speed))
  (setf (aref quat 2) value))

(u:fn-> z (quat) u:f32)
(declaim (inline z))
(defun z (quat)
  (declare (optimize speed))
  (aref quat 3))

(u:fn-> (setf z) (u:f32 quat) u:f32)
(declaim (inline (setf z)))
(defun (setf z) (value quat)
  (declare (optimize speed))
  (setf (aref quat 3) value))

;;; constants

(u:define-constant +id+ (%quat 1f0 0f0 0f0 0f0) :test #'equalp)
