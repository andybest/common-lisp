(in-package #:cl-user)

;;;; A 3D sphere primitive.

(defpackage #:mfiano.math.origin.geometry.sphere
  (:local-nicknames
   (#:point3d #:mfiano.math.origin.geometry.point3d)
   (#:u #:mfiano.misc.utils))
  (:use #:cl)
  (:export
   #:origin
   #:radius
   #:sphere))

(in-package #:mfiano.math.origin.geometry.sphere)

(declaim (inline %sphere))
(defstruct (sphere
            (:copier nil)
            (:constructor %sphere)
            (:conc-name nil))
  (origin (point3d:point) :type point3d:point)
  (radius 1.0 :type u:f32))

(u:fn-> sphere (&key (:origin point3d:point) (:radius u:f32)) sphere)
(declaim (inline sphere))
(defun sphere (&key (origin (point3d:point)) (radius 1.0))
  (declare (optimize speed))
  (%sphere :origin origin :radius radius))
