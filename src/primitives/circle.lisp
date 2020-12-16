(in-package #:cl-user)

;;;; A 2D circle primitive.

(defpackage #:net.mfiano.lisp.origin.circle
  (:local-nicknames
   (#:point2d #:net.mfiano.lisp.origin.point2d)
   (#:u #:net.mfiano.lisp.golden-utils))
  (:use #:cl)
  (:shadow
   #:position)
  ;; type and accessors
  (:export
   #:circle
   #:position
   #:radius)
  ;; operations
  (:export))

(in-package #:net.mfiano.lisp.origin.circle)

(declaim (inline %circle))
(defstruct (circle
            (:predicate nil)
            (:copier nil)
            (:constructor %circle)
            (:conc-name nil))
  (position (point2d:point 0f0 0f0) :type point2d:point)
  (radius 1f0 :type u:f32))

(u:fn-> circle (&key (:position point2d:point) (:radius u:f32)) circle)
(u:defun-inline circle (&key (position (point2d:point 0f0 0f0)) (radius 1f0))
  (declare (optimize speed))
  (%circle :position position :radius radius))
