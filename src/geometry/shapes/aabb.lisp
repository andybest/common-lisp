(in-package #:cl-user)

;;;; A 3D axis-aligned box primitive.

(defpackage #:net.mfiano.lisp.origin.geometry.aabb
  (:local-nicknames
   (#:point3d #:net.mfiano.lisp.origin.geometry.point3d)
   (#:u #:net.mfiano.lisp.golden-utils)
   (#:v3 #:net.mfiano.lisp.origin.vec3))
  (:use #:cl)
  (:shadow
   #:max
   #:min)
  (:export
   #:aabb
   #:aabb-from-min/max
   #:aabb-p
   #:max
   #:min
   #:origin
   #:size))

(in-package #:net.mfiano.lisp.origin.geometry.aabb)

(declaim (inline %aabb))
(defstruct (aabb
            (:copier nil)
            (:constructor %aabb)
            (:conc-name nil))
  (origin (point3d:point) :type point3d:point)
  (size (v3:vec 1) :type v3:vec))

(u:fn-> aabb (&key (:origin point3d:point) (:size v3:vec)) aabb)
(declaim (inline aabb))
(defun aabb (&key (origin (point3d:point)) (size (v3:vec 1)))
  (declare (optimize speed))
  (%aabb :origin origin :size size))

(u:fn-> aabb-from-min/max (&key (:min point3d:point) (:max point3d:point)) aabb)
(declaim (inline aabb-from-min/max))
(defun aabb-from-min/max (&key
                            (min (point3d:point -0.5))
                            (max (point3d:point 0.5)))
  "Construct an AABB from a MINIMUM and MAXIMUM points. "
  (declare (optimize speed))
  (let ((origin (v3:scale (v3:+ min max) 0.5))
        (size (v3:scale (v3:- max min) 0.5)))
    (%aabb :origin origin :size size)))

(u:fn-> min (aabb) point3d:point)
(declaim (inline min))
(defun min (aabb)
  "Return the minimum point of an AABB."
  (declare (optimize speed))
  (let ((origin (origin aabb))
        (size (size aabb)))
    (v3:min (v3:+ origin size)
            (v3:- origin size))))

(u:fn-> max (aabb) point3d:point)
(declaim (inline max))
(defun max (aabb)
  "Return the maximum point of an AABB."
  (declare (optimize speed))
  (let ((origin (origin aabb))
        (size (size aabb)))
    (v3:max (v3:+ origin size)
            (v3:- origin size))))
