(in-package #:net.mfiano.lisp.origin.geometry)

(u:fn-> %closest-point-sphere (sphere:sphere point3d:point) point3d:point)
(defun closest-point-sphere (sphere point)
  (declare (optimize speed))
  (let* ((origin (sphere:origin sphere))
         (vector (v3:- point origin)))
    (declare (dynamic-extent vector))
    (v3:normalize! vector vector)
    (v3:scale! vector vector (sphere:radius sphere))
    (v3:+ vector origin)))

(u:fn-> closest-point-aabb (aabb:aabb point3d:point) point3d:point)
(declaim (inline closest-point-aabb))
(defun closest-point-aabb (aabb point)
  (declare (optimize speed))
  (v3:clamp point (aabb:min aabb) (aabb:max aabb)))

(u:fn-> closest-point-obb (obb:obb point3d:point) point3d:point)
(defun closest-point-obb (obb point)
  (declare (optimize speed))
  (let* ((origin (obb:origin obb))
         (size (obb:size obb))
         (-size (v3:negate size))
         (direction (v3:- point origin))
         (rotation (obb:rotation obb))
         (distances (v3:vec (v3:dot direction (m3:get-column rotation 0))
                            (v3:dot direction (m3:get-column rotation 1))
                            (v3:dot direction (m3:get-column rotation 2)))))
    (declare (dynamic-extent direction -size distances))
    (v3:clamp! distances distances -size size)
    (m3:*v3! distances rotation distances)
    (v3:+ origin distances)))

(u:fn-> closest-point-plane (plane:plane point3d:point) point3d:point)
(defun closest-point-plane (plane point)
  (declare (optimize speed))
  (let* ((normal (plane:normal plane))
         (distance (- (v3:dot normal point) (plane:distance plane))))
    (v3:- point (v3:scale normal distance))))

(u:fn-> closest-point-line3d (line3d:line point3d:point) point3d:point)
(defun closest-point-line3d (line point)
  (declare (optimize speed))
  (let* ((start (line3d:start line))
         (vector (v3:- (line3d:end line) start))
         (x (u:clamp (/ (v3:dot (v3:- point start) vector)
                        (v3:length-squared vector))
                     0.0
                     1.0)))
    (declare (dynamic-extent vector))
    (v3:scale! vector vector x)
    (v3:+ start vector)))

(u:fn-> closest-point-ray (ray:ray point3d:point) point3d:point)
(defun closest-point-ray (ray point)
  (declare (optimize speed))
  ;; NOTE: This assumes that the ray direction is normalized, which is the case
  ;; upon the construction of a ray. It is therefor important not to erroneously
  ;; alter a ray's direction vector slot, at least not without re-normalizing
  ;; it.
  (let* ((origin (ray:origin ray))
         (direction (ray:direction ray))
         (vector (v3:- point origin)))
    (declare (dynamic-extent vector))
    (v3:scale! vector direction (max (v3:dot vector direction) 0.0))
    (v3:+ origin vector)))
