(in-package #:cl-user)

;;; NOTE: Line2d represents a 2D line segment, not a infinite line in the
;;; mathematical sense. Since line segments are so common in physics, we have
;;; chosen to use this convention (as many other game physics libraries do).

(defpackage #:net.mfiano.lisp.origin.line2d
  (:local-nicknames
   (#:point2d #:net.mfiano.lisp.origin.point2d)
   (#:u #:net.mfiano.lisp.golden-utils)
   (#:v2 #:net.mfiano.lisp.origin.vec2))
  (:use #:cl)
  (:shadow
   #:length)
  ;; type and accessors
  (:export
   #:line
   #:start
   #:end)
  ;; operations
  (:export
   #:direction
   #:length
   #:length-squared
   #:midpoint))

(in-package #:net.mfiano.lisp.origin.line2d)

(declaim (inline line))
(defstruct (line
            (:constructor %line (start end))
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (start (point2d:point 0f0 0f0) :type point2d:point)
  (end (point2d:point 0f0 0f0) :type point2d:point))

(u:fn-> line (point2d:point point2d:point) line)
(u:defun-inline line (start end)
  (declare (optimize speed))
  (%line start end))

(u:fn-> length (line) u:f32)
(u:defun-inline length (line)
  (declare (optimize speed))
  (v2:length (v2:- (end line) (start line))))

(u:fn-> length-squared (line) u:f32)
(u:defun-inline length-squared (line)
  (declare (optimize speed))
  (v2:length-squared (v2:- (end line) (start line))))

(u:fn-> midpoint (line) point2d:point)
(u:defun-inline midpoint (line)
  (declare (optimize speed))
  (v2:lerp (start line) (end line) 0.5))

(u:fn-> direction (line) v2:vec)
(u:defun-inline direction (line)
  (declare (optimize speed))
  (v2:normalize (v2:- (end line) (start line))))
