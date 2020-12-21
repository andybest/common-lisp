(in-package #:cl-user)

;;;; A 2D oriented rectangle primitive. The oriented rectangle is represented as
;;;; a center origin point, and half-extents.

(defpackage #:net.mfiano.lisp.origin.oriented-rect
  (:local-nicknames
   (#:m2 #:net.mfiano.lisp.origin.mat2)
   (#:point2d #:net.mfiano.lisp.origin.point2d)
   (#:rect #:net.mfiano.lisp.origin.rect)
   (#:u #:net.mfiano.lisp.golden-utils)
   (#:v2 #:net.mfiano.lisp.origin.vec2))
  (:use #:cl)
  (:shadow
   #:position)
  ;; type and accessors
  (:export
   #:rect
   #:position
   #:half-extents
   #:angle))

(in-package #:net.mfiano.lisp.origin.oriented-rect)

(declaim (inline %rect))
(defstruct (rect
            (:predicate nil)
            (:copier nil)
            (:constructor %rect)
            (:conc-name nil))
  (position (point2d:point) :type point2d:point)
  (half-extents (v2:vec 1 1) :type v2:vec)
  (angle 0f0 :type u:f32))

(u:fn-> rect (&key (:position point2d:point)
                   (:half-extents v2:vec)
                   (:angle u:f32))
        rect)
(declaim (inline rect))
(defun rect (&key
               (position (point2d:point))
               (half-extents (v2:vec 1f0 1f0))
               (angle 0f0))
  "Construct a rect whose center point is origined at POSITION, which extends
along both axes by half of HALF-EXTENTS, with an ANGLE of rotation in radians."
  (declare (optimize speed))
  (%rect :position position :half-extents half-extents :angle angle))

(u:fn-> interval (rect v2:vec) v2:vec)
(defun interval (rect axis)
  "Get the interval of the rect along the given AXIS. This is used to test if
two intervals overlap in a separating axis theorem test. AXIS is expected to be
normalized."
  (declare (optimize speed))
  (let* ((position (position rect))
         (half-extents (half-extents rect))
         (aligned-rect (rect:rect :position (v2:- (position rect) half-extents)
                                  :size (v2:scale half-extents 2.0)))
         (rotation (m2:rotation-from-angle (angle rect)))
         (vertices (rect::vertices aligned-rect)))
    (map nil
         (lambda (x)
           (v2:-! x x position)
           (m2:*v2! x rotation x)
           (v2:+! x x position))
         vertices)
    (v2:with-components ((r (v2:vec (v2:dot axis (aref vertices 0)))))
      (map nil
           (lambda (x)
             (let ((projection (v2:dot axis x)))
               (setf rx (u:clamp rx rx projection)
                     ry (u:clamp ry projection ry))))
           vertices)
      r)))
