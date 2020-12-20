(in-package #:cl-user)

;;; 2D points are just type aliased to be vec2, with a convenience constructor
;;; function.

(defpackage #:net.mfiano.lisp.origin.point2d
  (:local-nicknames
   (#:u #:net.mfiano.lisp.golden-utils)
   (#:v2 #:net.mfiano.lisp.origin.vec2))
  (:use #:cl)
  (:import-from
   #:net.mfiano.lisp.origin.vec2
   #:x
   #:y)
  ;; type and accessors
  (:export
   #:point
   #:x
   #:y)
  ;; operations
  (:export
   #:distance
   #:distance-squared
   #:translate))

(in-package #:net.mfiano.lisp.origin.point2d)

(deftype point () 'v2:vec)

(u:fn-> point (&optional u:f32 u:f32) point)
(declaim (inline point))
(defun point (&optional (x 0f0) (y 0f0))
  (declare (optimize speed))
  (v2::%vec x y))

(u:fn-> translate (point v2:vec u:f32) point)
(declaim (inline translate))
(defun translate (point direction distance)
  (declare (optimize speed))
  (v2:+ point (v2:scale direction distance)))

(u:fn-> distance-squared (point point) u:f32)
(declaim (inline distance-squared))
(defun distance-squared (point1 point2)
  (declare (optimize speed))
  (v2:length-squared (v2:- point2 point1)))

(u:fn-> distance (point point) u:f32)
(declaim (inline distance))
(defun distance (point1 point2)
  (declare (optimize speed))
  (sqrt (distance-squared point1 point2)))
