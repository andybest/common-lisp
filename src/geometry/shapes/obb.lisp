(in-package #:cl-user)

;;;; A 3D oriented box primitive.

(defpackage #:origin.geometry.obb
  (:local-nicknames
   (#:m3 #:origin.mat3)
   (#:point3d #:origin.geometry.point3d)
   (#:u #:golden-utils)
   (#:v2 #:origin.vec2)
   (#:v3 #:origin.vec3))
  (:use #:cl)
  (:export
   #:obb
   #:obb-p
   #:origin
   #:size
   #:rotation))

(in-package #:origin.geometry.obb)

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
(defun obb (&key (origin (point3d:point)) (size (v3:vec 1)) (rotation (m3:mat 1)))
  (declare (optimize speed))
  (%obb :origin origin :size size :rotation rotation))

(u:fn-> vertices (obb) (simple-vector 8))
(defun vertices (obb)
  "Get a vector of an OBB's vertices."
  (declare (optimize speed))
  (let* ((origin (origin obb))
         (size (size obb))
         (rotation (rotation obb))
         (x (v3:scale (m3:get-column rotation 0) (v3:x size)))
         (y (v3:scale (m3:get-column rotation 1) (v3:y size)))
         (z (v3:scale (m3:get-column rotation 2) (v3:z size)))
         (v1 (v3:+ origin x))
         (v2 (v3:- origin x))
         (v3 (v3:copy v1))
         (v4 (v3:copy v1))
         (v5 (v3:copy v1))
         (v6 (v3:copy v2))
         (v7 (v3:copy v2))
         (v8 (v3:copy v2)))
    (declare (dynamic-extent x y z))
    (v3:+! v1 v1 y)
    (v3:+! v1 v1 z)
    (v3:+! v2 v2 y)
    (v3:+! v2 v2 z)
    (v3:-! v3 v3 y)
    (v3:+! v3 v3 z)
    (v3:+! v4 v4 y)
    (v3:-! v4 v4 z)
    (v3:-! v5 v5 y)
    (v3:-! v5 v5 z)
    (v3:-! v6 v6 y)
    (v3:-! v6 v6 z)
    (v3:+! v7 v7 y)
    (v3:-! v7 v7 z)
    (v3:-! v8 v8 y)
    (v3:+! v8 v8 z)
    (vector v1 v2 v3 v4 v5 v6 v7 v8)))

(u:fn-> interval (obb v3:vec) v2:vec)
(defun interval (obb axis)
  "Get the interval of the OBB along the given AXIS. This is used to test if two intervals overlap
in a separating axis theorem test. AXIS is expected to be normalized."
  (declare (optimize speed))
  (let ((vertices (vertices obb)))
    (v2:with-components ((r (v2:vec (v3:dot axis (aref vertices 0)))))
      (dotimes (i 8)
        (let ((projection (v3:dot axis (aref vertices i))))
          (setf rx (u:clamp rx rx projection)
                ry (u:clamp ry projection ry))))
      r)))
