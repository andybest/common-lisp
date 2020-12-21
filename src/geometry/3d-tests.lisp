(in-package #:net.mfiano.lisp.origin.geometry)

(u:fn-> point3d/point3d (point3d:point point3d:point) boolean)
(declaim (inline point3d/point3d))
(defun point3d/point3d (point1 point2)
  "Test if two 3D points intersect."
  (declare (optimize speed))
  (v3:= point1 point2))

(u:fn-> %point3d/sphere (point3d:point sphere:sphere) boolean)
(declaim (inline %point3d/sphere))
(defun %point3d/sphere (point sphere)
  "Helper function that does the work for POINT3D/SPHERE and SPHERE/POINT3D."
  (declare (optimize speed))
  (let ((line (line3d:line :start point :end (sphere:origin sphere))))
    (< (line3d:length-squared line) (expt (sphere:radius sphere) 2))))

(u:fn-> point3d/sphere (point3d:point sphere:sphere) boolean)
(defun point3d/sphere (point sphere)
  "Test if a 3D point is contained within a sphere."
  (declare (optimize speed))
  (%point3d/sphere point sphere))

(u:fn-> sphere/point3d (sphere:sphere point3d:point) boolean)
(defun sphere/point3d (sphere point)
  "Test if a sphere contains a 3D point."
  (declare (optimize speed))
  (%point3d/sphere point sphere))

(u:fn-> %point3d/aabb (point3d:point aabb:aabb) boolean)
(declaim (inline %point3d/aabb))
(defun %point3d/aabb (point aabb)
  "Helper function that does the work for POINT3D/AABB and AABB/POINT3D."
  (declare (optimize speed))
  (and (v3:<= (aabb:min aabb) point)
       (v3:<= point (aabb:max aabb))))

(u:fn-> point3d/aabb (point3d:point aabb:aabb) boolean)
(defun point3d/aabb (point aabb)
  "Test if a 3D point is contained within an AABB."
  (declare (optimize speed))
  (%point3d/aabb point aabb))

(u:fn-> aabb/point3d (aabb:aabb point3d:point) boolean)
(defun aabb/point3d (aabb point)
  "Test if an AABB contains a 3D point."
  (declare (optimize speed))
  (%point3d/aabb point aabb))

(u:fn-> %point3d/obb (point3d:point obb:obb) boolean)
(declaim (inline %point3d/obb))
(defun %point3d/obb (point obb)
  "Helper function that does the work for POINT3D/OBB and OBB/POINT3D."
  (declare (optimize speed))
  (let* ((direction (v3:- point (obb:origin obb)))
         (size (obb:size obb))
         (-size (v3:negate size))
         (rotation (obb:rotation obb))
         (distances (v3:vec (v3:dot direction (m3:get-column rotation 0))
                            (v3:dot direction (m3:get-column rotation 1))
                            (v3:dot direction (m3:get-column rotation 2)))))
    (declare (dynamic-extent direction -size distances))
    (and (v3:<= distances size)
         (v3:>= distances -size))))

(u:fn-> point3d/obb (point3d:point obb:obb) boolean)
(defun point3d/obb (point obb)
  "Test if a 3D point is contained within an OBB."
  (declare (optimize speed))
  (%point3d/obb point obb))

(u:fn-> obb/point3d (obb:obb point3d:point) boolean)
(defun obb/point3d (obb point)
  "Test if an OBB contains a 3D point."
  (declare (optimize speed))
  (%point3d/obb point obb))

(u:fn-> %point3d/plane (point3d:point plane:plane) boolean)
(declaim (inline %point3d/plane))
(defun %point3d/plane (point plane)
  "Helper function that does the work for POINT3D/PLANE and PLANE/POINT3D."
  (declare (optimize speed))
  (let ((dot (v3:dot point (plane:normal plane))))
    (com:= (- dot (plane:distance plane)) 0.0 1e-7 1e-7)))

(u:fn-> point3d/plane (point3d:point plane:plane) boolean)
(defun point3d/plane (point plane)
  "Test if a 3D point is on a plane."
  (declare (optimize speed))
  (%point3d/plane point plane))

(u:fn-> plane/point3d (plane:plane point3d:point) boolean)
(defun plane/point3d (plane point)
  "Test is a plane contains a 3D point."
  (declare (optimize speed))
  (%point3d/plane point plane))
