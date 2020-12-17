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
   #:translate))

(in-package #:net.mfiano.lisp.origin.point2d)

(deftype point () 'v2:vec)

(u:fn-> point (&optional u:f32 u:f32) point)
(u:defun-inline point (&optional (x 0f0) (y 0f0))
  (declare (optimize speed))
  (v2::%vec x y))

(u:fn-> translate (point v2:vec u:f32) point)
(u:defun-inline translate (point direction distance)
  (declare (optimize speed))
  (v2:+ point (v2:scale direction distance)))
