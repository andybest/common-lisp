(in-package #:cl-user)

(defpackage #:mfiano.math.origin.geometry.plane
  (:local-nicknames
   (#:point3d #:mfiano.math.origin.geometry.point3d)
   (#:u #:mfiano.misc.utils)
   (#:v3 #:mfiano.math.origin.vec3))
  (:use #:cl)
  (:export
   #:distance
   #:normal
   #:plane
   #:plane-p))

(in-package #:mfiano.math.origin.geometry.plane)

(declaim (inline %plane))
(defstruct (plane
            (:copier nil)
            (:constructor %plane)
            (:conc-name nil))
  (normal (v3:vec 0 1 0) :type v3:vec)
  (distance 0.0 :type u:f32))

(u:fn-> plane (&key (:normal v3:vec) (:distance u:f32)) plane)
(declaim (inline plane))
(defun plane (&key (normal (v3:vec 0 1 0)) (distance 0.0))
  (declare (optimize speed))
  (%plane :normal (v3:normalize normal) :distance distance))
