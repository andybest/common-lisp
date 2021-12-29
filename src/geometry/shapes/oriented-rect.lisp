(in-package #:cl-user)

;;;; A 2D oriented rectangle primitive. The oriented rectangle is represented as a center origin
;;;; point, and half-extents.

(defpackage #:mfiano.math.origin.geometry.oriented-rect
  (:local-nicknames
   (#:m2 #:mfiano.math.origin.mat2)
   (#:point2d #:mfiano.math.origin.geometry.point2d)
   (#:rect #:mfiano.math.origin.geometry.rect)
   (#:u #:mfiano.misc.utils)
   (#:v2 #:mfiano.math.origin.vec2))
  (:use #:cl)
  (:export
   #:angle
   #:half-extents
   #:origin
   #:rect
   #:rect-p))

(in-package #:mfiano.math.origin.geometry.oriented-rect)

(declaim (inline %rect))
(defstruct (rect
            (:copier nil)
            (:constructor %rect)
            (:conc-name nil))
  (origin (point2d:point) :type point2d:point)
  (half-extents (v2:vec 1) :type v2:vec)
  (angle 0.0 :type u:f32))

(u:fn-> rect (&key (:origin point2d:point) (:half-extents v2:vec) (:angle u:f32)) rect)
(declaim (inline rect))
(defun rect (&key (origin (point2d:point)) (half-extents (v2:vec 1)) (angle 0.0))
  "Construct a rect whose center point is origined at ORIGIN, which extends along both axes by half
of HALF-EXTENTS, with an ANGLE of rotation in radians."
  (declare (optimize speed))
  (%rect :origin origin :half-extents half-extents :angle angle))

(u:fn-> interval (rect v2:vec) v2:vec)
(defun interval (rect axis)
  "Get the interval of the rect along the given AXIS. This is used to test if two intervals overlap
in a separating axis theorem test. AXIS is expected to be normalized."
  (declare (optimize speed))
  (let* ((origin (origin rect))
         (half-extents (half-extents rect))
         (aligned-rect (rect:rect :origin (v2:- (origin rect) half-extents)
                                  :size (v2:scale half-extents 2.0)))
         (rotation (m2:rotation-from-angle (angle rect)))
         (vertices (rect::vertices aligned-rect)))
    (map nil
         (lambda (x)
           (v2:-! x x origin)
           (m2:*v2! x rotation x)
           (v2:+! x x origin))
         vertices)
    (v2:with-components ((r (v2:vec (v2:dot axis (aref vertices 0)))))
      (dotimes (i 4)
        (let ((projection (v2:dot axis (aref vertices i))))
          (setf rx (u:clamp rx rx projection)
                ry (u:clamp ry projection ry))))
      r)))
