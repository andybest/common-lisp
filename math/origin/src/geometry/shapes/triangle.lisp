(in-package #:cl-user)

(defpackage #:mfiano.math.origin.geometry.triangle
  (:local-nicknames
   (#:point3d #:mfiano.math.origin.geometry.point3d)
   (#:u #:mfiano.misc.utils)
   (#:v3 #:mfiano.math.origin.vec3))
  (:use #:cl)
  (:export
   #:triangle
   #:triangle-p))

(in-package #:mfiano.math.origin.geometry.triangle)

(declaim (inline %triangle))
(defstruct (triangle
            (:copier nil)
            (:constructor %triangle (a b c))
            (:conc-name nil))
  (a (point3d:point) :type point3d:point)
  (b (point3d:point) :type point3d:point)
  (c (point3d:point) :type point3d:point))

(u:fn-> triangle (&optional point3d:point point3d:point point3d:point) triangle)
(declaim (inline triangle))
(defun triangle (&optional (a (point3d:point)) (b (point3d:point)) (c (point3d:point)))
  (declare (optimize speed))
  (%triangle a b c))
