(in-package #:cl-user)

;;;; This file defines a package for constructing 2D and 3D minimum bounding
;;;; shapes and shape collections.

;;;; A 2D minimum bounding shape is a primitve circle or rect that encompasses
;;;; all of a set of 2D points. 3D minimum bounding shapes are symmetrical
;;;; except using spheres and AABBs.

;;;; Minimum bounding shapes are useful in collision detection to simplify a
;;;; collision region. It is much less expensive to test a circle, rect, or
;;;; their 3D variants, than it is to test a more complex shape, such as a point
;;;; cloud or concave polygon. They are an important part of a broad-phase
;;;; collision detection system to quickly check to see if more expensive checks
;;;; are needed, before actually performing these expensive checks first.

;;;; A 2D shape collection is a collection of multiple circles and rects that
;;;; can be tested against other primitives or shape collections for a
;;;; collision. 3D shape collections are symmetrical except using spheres and
;;;; AABBs as their constituents.

;;;; Shape collections are useful because sometimes a minimum bounding shape is
;;;; not accurate enough for the object which needs a collision region, or they
;;;; can be used in a later phase of collision detection after the broad phase
;;;; using a minimum bounding shape. With a shape collection, there is a single
;;;; object with N circles and M rects (or N spheres and M AABBs for 3D) to
;;;; approximate the shape of a complex piece of geometry. This single shape
;;;; collection can be tested against other shape collections, points, lines,
;;;; and other primitives with a single function call.

(defpackage #:net.mfiano.lisp.origin.geometry.containers
  (:local-nicknames
   (#:circle #:net.mfiano.lisp.origin.circle)
   (#:point2d #:net.mfiano.lisp.origin.point2d)
   (#:rect #:net.mfiano.lisp.origin.rect)
   (#:u #:net.mfiano.lisp.golden-utils)
   (#:v2 #:net.mfiano.lisp.origin.vec2))
  (:use #:cl)
  (:export
   #:rect
   #:circle))

(in-package #:net.mfiano.lisp.origin.geometry.containers)

(defstruct (collection2d
            (:predicate nil)
            (:copier nil))
  (circles (vector) :type simple-vector)
  (rects (vector) :type simple-vector))

(u:fn-> circle (simple-vector) circle:circle)
(defun circle (points)
  "Construct a minimum bounding circle that encompasses all points in the
supplied vector of 2D points."
  (declare (optimize speed))
  (let* ((point-count (length points))
         (center (point2d:point)))
    (dotimes (i point-count)
      (v2:+! center center (svref points i)))
    (v2:scale! center center (/ 1.0 point-count))
    (let ((radius (point2d:distance-squared (svref points 0) center)))
      (dotimes (i point-count)
        (let ((distance (point2d:distance-squared (svref points i) center)))
          (when (> distance radius)
            (setf radius distance))))
      (circle:circle :position center
                     :radius (sqrt (the (single-float 0.0) radius))))))

(u:fn-> rect (simple-vector) rect:rect)
(defun rect (points)
  "Construct a minimum bounding rect that encompasses all points in the supplied
vector of 2D points."
  (declare (optimize speed))
  (let ((min (v2:copy (svref points 0)))
        (max (v2:copy (svref points 0))))
    (dotimes (i (length points))
      (let ((point (svref points i)))
        (v2:min! min point min)
        (v2:max! max point max)))
    (rect:rect-from-min/max :min min :max max)))
