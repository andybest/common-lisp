(in-package #:cl-user)

;;; NOTE: Line2d represents a 2D line segment, not a infinite line in the
;;; mathematical sense. Since line segments are so common in physics, we have
;;; chosen to use this convention (as many other game physics libraries do).

(defpackage #:net.mfiano.lisp.origin.line3d
  (:local-nicknames
   (#:point3d #:net.mfiano.lisp.origin.point3d)
   (#:u #:net.mfiano.lisp.golden-utils)
   (#:v3 #:net.mfiano.lisp.origin.vec3))
  (:use #:cl)
  (:shadow
   #:length)
  ;; type and accessors
  (:export
   #:line
   #:start
   #:end)
  (:export
   #:direction
   #:length
   #:length-squared
   #:midpoint))

(in-package #:net.mfiano.lisp.origin.line3d)

(declaim (inline line))
(defstruct (line
            (:constructor %line (start end))
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (start (point3d:point 0f0 0f0 0f0) :type point3d:point)
  (end (point3d:point 0f0 0f0 0f0) :type point3d:point))

(u:fn-> line (point3d:point point3d:point) line)
(u:defun-inline line (start end)
  (declare (optimize speed))
  (%line start end))

(u:fn-> length (line) u:f32)
(u:defun-inline length (line)
  (declare (optimize speed))
  (v3:length (v3:- (end line) (start line))))

(u:fn-> length-squared (line) u:f32)
(u:defun-inline length-squared (line)
  (declare (optimize speed))
  (v3:length-squared (v3:- (end line) (start line))))

(u:fn-> midpoint (line) point3d:point)
(u:defun-inline midpoint (line)
  (declare (optimize speed))
  (v3:lerp (start line) (end line) 0.5))

(u:fn-> direction (line) v3:vec)
(u:defun-inline direction (line)
  (declare (optimize speed))
  (v3:normalize (v3:- (end line) (start line))))
