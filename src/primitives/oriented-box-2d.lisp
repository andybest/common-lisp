(in-package #:cl-user)

;;;; A 2D oriented rectangle primitive. The oriented rectangle is represented as
;;;; a center origin point, and half-extents.

(defpackage #:net.mfiano.lisp.origin.oriented-box-2d
  (:local-nicknames
   (#:point2d #:net.mfiano.lisp.origin.point2d)
   (#:u #:net.mfiano.lisp.golden-utils)
   (#:v2 #:net.mfiano.lisp.origin.vec2))
  (:use #:cl)
  (:shadow
   #:position)
  ;; type and accessors
  (:export
   #:box
   #:position
   #:half-extents
   #:angle)
  ;; operations
  (:export))

(in-package #:net.mfiano.lisp.origin.oriented-box-2d)

(declaim (inline %box))
(defstruct (box
            (:predicate nil)
            (:copier nil)
            (:constructor %box)
            (:conc-name nil))
  (position (point2d:point) :type point2d:point)
  (half-extents (v2:vec 1 1) :type v2:vec)
  (angle 0f0 :type u:f32))

(u:fn-> box (&key (:position point2d:point)
                  (:half-extents v2:vec)
                  (:angle u:f32))
        box)
(declaim (inline box))
(defun box (&key
              (position (point2d:point))
              (half-extents (v2:vec 1f0 1f0))
              (angle 0f0))
  "Construct a rectangle whose center point is origined at POSITION, which
extends along both axes by half of HALF-EXTENTS, with an ANGLE of rotation in
radians."
  (declare (optimize speed))
  (%box :position position :half-extents half-extents :angle angle))
