(in-package #:cl-user)

;;;; A 2D circle primitive.

(defpackage #:origin.geometry.circle
  (:local-nicknames
   (#:point2d #:origin.geometry.point2d)
   (#:u #:mfiano-utils))
  (:use #:cl)
  (:export
   #:circle
   #:circle-p
   #:origin
   #:radius))

(in-package #:origin.geometry.circle)

(declaim (inline %circle))
(defstruct (circle
            (:copier nil)
            (:constructor %circle)
            (:conc-name nil))
  (origin (point2d:point) :type point2d:point)
  (radius 1.0 :type u:f32))

(u:fn-> circle (&key (:origin point2d:point) (:radius u:f32)) circle)
(declaim (inline circle))
(defun circle (&key (origin (point2d:point)) (radius 1.0))
  (declare (optimize speed))
  (%circle :origin origin :radius radius))
