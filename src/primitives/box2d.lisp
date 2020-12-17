(in-package #:cl-user)

;;;; A 2D axis-aligned rectangle primitive. A rectangle can be represented a
;;;; number of different ways. This package allows constructing a rectangle from
;;;; two different representations: an origin and size, with BOX, and a minimum
;;;; and maximum point with BOX-FROM-MIN/MAX.

(defpackage #:net.mfiano.lisp.origin.box2d
  (:local-nicknames
   (#:point2d #:net.mfiano.lisp.origin.point2d)
   (#:u #:net.mfiano.lisp.golden-utils)
   (#:v2 #:net.mfiano.lisp.origin.vec2))
  (:use #:cl)
  (:shadow
   #:max
   #:min
   #:position)
  ;; type and accessors
  (:export
   #:box
   #:position
   #:size)
  ;; operations
  (:export
   #:max
   #:min))

(in-package #:net.mfiano.lisp.origin.box2d)

(declaim (inline %box))
(defstruct (box
            (:predicate nil)
            (:copier nil)
            (:constructor %box)
            (:conc-name nil))
  (position (point2d:point) :type point2d:point)
  (size (v2:vec 1f0 1f0) :type v2:vec))

(u:fn-> box (&key (:position point2d:point) (:size v2:vec)) box)
(declaim (inline box))
(defun box (&key (position (point2d:point)) (size (v2:vec 1f0 1f0)))
  "Construct a rectangle whose bottom-left corner is origined at POSITION,
extending to SIZE units."
  (declare (optimize speed))
  (%box :position position :size size))

(u:fn-> box-from-min/max (&key (:min point2d:point) (:max point2d:point)) box)
(declaim (inline box-from-min/max))
(defun box-from-min/max (&key
                           (min (point2d:point -0.5 -0.5))
                           (max (point2d:point 0.5 0.5)))
  "Construct a rectangle from a MINIMUM and MAXIMUM points. These correspond to
the bottom-left corner and upper-right corner of the resulting rectangle,
respectively."
  (declare (optimize speed))
  (%box :position min :size (v2:- max min)))

(u:fn-> min (box) point2d:point)
(declaim (inline min))
(defun min (box)
  "Return the minimum point of a rectangle. This is usually its bottom-left
corner."
  (declare (optimize speed))
  (let ((position (position box)))
    (v2:vec (v2:min position (v2:+ position (size box))))))

(u:fn-> max (box) point2d:point)
(declaim (inline max))
(defun max (box)
  "Return the maximum point of a rectangle. This is usually its top-left
corner."
  (declare (optimize speed))
  (let ((position (position box)))
    (v2:vec (v2:max position (v2:+ position (size box))))))
