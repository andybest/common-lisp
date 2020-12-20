(in-package #:cl-user)

(defpackage #:net.mfiano.lisp.origin.bounds
  (:local-nicknames
   (#:box2d #:net.mfiano.lisp.origin.box2d)
   (#:circle #:net.mfiano.lisp.origin.circle)
   (#:point2d #:net.mfiano.lisp.origin.point2d)
   (#:u #:net.mfiano.lisp.golden-utils)
   (#:v2 #:net.mfiano.lisp.origin.vec2))
  (:use #:cl)
  (:export
   #:box2d
   #:circle))

(in-package #:net.mfiano.lisp.origin.bounds)

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

(u:fn-> box2d (simple-vector) box2d:box)
(defun box2d (points)
  "Construct a minimum bounding 2D box that encompasses all points in the
supplied vector of 2D points."
  (declare (optimize speed))
  (let ((min (v2:copy (svref points 0)))
        (max (v2:copy (svref points 0))))
    (dotimes (i (length points))
      (let ((point (svref points i)))
        (v2:min! min point min)
        (v2:max! max point max)))
    (box2d:box-from-min/max :min min :max max)))
