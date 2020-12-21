(in-package #:cl-user)

;;;; A 3D oriented box primitive.

(defpackage #:net.mfiano.lisp.origin.geometry.obb
  (:local-nicknames
   (#:m3 #:net.mfiano.lisp.origin.mat3)
   (#:point3d #:net.mfiano.lisp.origin.geometry.point3d)
   (#:u #:net.mfiano.lisp.golden-utils)
   (#:v3 #:net.mfiano.lisp.origin.vec3))
  (:use #:cl)
  (:export
   #:obb
   #:obb-p
   #:origin
   #:size
   #:rotation))

(in-package #:net.mfiano.lisp.origin.geometry.obb)

(declaim (inline %obb))
(defstruct (obb
            (:copier nil)
            (:constructor %obb)
            (:conc-name nil))
  (origin (point3d:point) :type point3d:point)
  (size (v3:vec 1) :type v3:vec)
  (rotation (m3:mat 1) :type m3:mat))

(u:fn-> obb (&key (:origin point3d:point) (:size v3:vec) (:rotation m3:mat)) obb)
(declaim (inline obb))
(defun obb (&key
              (origin (point3d:point))
              (size (v3:vec 1))
              (rotation (m3:mat 1)))
  (declare (optimize speed))
  (%obb :origin origin :size size :rotation rotation))
