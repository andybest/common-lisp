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

(u:fn-> %point3d/line3d (point3d:point line3d:line) boolean)
(declaim (inline %point3d/line3d))
(defun %point3d/line3d (point line)
  "Helper function that does the work for POINT3D/LINE3D and LINE3D/POINT3D."
  (declare (optimize speed))
  (let* ((closest-point (closest-point-line3d line point))
         (vector (v3:- closest-point point)))
    (declare (dynamic-extent vector))
    (com:= (v3:length-squared vector) 0.0 1e-7 1e-7)))

(u:fn-> point3d/line3d (point3d:point line3d:line) boolean)
(defun point3d/line3d (point line)
  "Test if a 3D point is on a 3D line."
  (declare (optimize speed))
  (%point3d/line3d point line))

(u:fn-> line3d/point3d (line3d:line point3d:point) boolean)
(defun line3d/point3d (line point)
  "Test if a 3D line contains a 3D point."
  (declare (optimize speed))
  (%point3d/line3d point line))

(u:fn-> %point3d/ray (point3d:point ray:ray) boolean)
(declaim (inline %point3d/ray))
(defun %point3d/ray (point ray)
  "Helper function that does the work for POINT3D/RAY and RAY/POINT3D."
  (declare (optimize speed))
  (let ((origin (ray:origin ray)))
    (when (v3:= point origin)
      (return-from %point3d/ray t))
    (let ((normal (v3:- point origin)))
      (declare (dynamic-extent normal))
      (v3:normalize! normal normal)
      (com:= (v3:dot normal (ray:direction ray)) 1.0 1e-7 1e-7))))

(u:fn-> point3d/ray (point3d:point ray:ray) boolean)
(defun point3d/ray (point ray)
  "Test if a 3D point is on a ray."
  (declare (optimize speed))
  (%point3d/ray point ray))

(u:fn-> ray/point3d (ray:ray point3d:point) boolean)
(defun ray/point3d (ray point)
  "Test if a ray contains a 3D point."
  (declare (optimize speed))
  (%point3d/ray point ray))

(u:fn-> sphere/sphere (sphere:sphere sphere:sphere) boolean)
(defun sphere/sphere (sphere1 sphere2)
  "Test if two spheres intersect."
  (declare (optimize speed))
  (<= (point3d:distance-squared (sphere:origin sphere1) (sphere:origin sphere2))
      (expt (+ (sphere:radius sphere1) (sphere:radius sphere2)) 2)))

(u:fn-> %sphere/aabb (sphere:sphere aabb:aabb) boolean)
(declaim (inline %sphere/aabb))
(defun %sphere/aabb (sphere aabb)
  "Helper function that does the work for SPHERE/AABB and AABB/SPHERE."
  (declare (optimize speed))
  (let* ((sphere-origin (sphere:origin sphere))
         (closest-point (closest-point-aabb aabb sphere-origin)))
    (declare (dynamic-extent closest-point))
    (< (point3d:distance-squared closest-point sphere-origin)
       (expt (sphere:radius sphere) 2))))

(u:fn-> sphere/aabb (sphere:sphere aabb:aabb) boolean)
(defun sphere/aabb (sphere aabb)
  "Test if a sphere intersects an AABB."
  (declare (optimize speed))
  (%sphere/aabb sphere aabb))

(u:fn-> aabb/sphere (aabb:aabb sphere:sphere) boolean)
(defun aabb/sphere (aabb sphere)
  "Test if an AABB intersects a sphere."
  (declare (optimize speed))
  (%sphere/aabb sphere aabb))

(u:fn-> %sphere/obb (sphere:sphere obb:obb) boolean)
(declaim (inline %sphere/obb))
(defun %sphere/obb (sphere obb)
  "Helper function that does the work for SPHERE/OBB and OBB/SPHERE."
  (declare (optimize speed))
  (let* ((sphere-origin (sphere:origin sphere))
         (closest-point (closest-point-obb obb sphere-origin)))
    (< (point3d:distance-squared closest-point sphere-origin)
       (expt (sphere:radius sphere) 2))))

(u:fn-> sphere/obb (sphere:sphere obb:obb) boolean)
(defun sphere/obb (sphere obb)
  "Test if a sphere intersects an OBB."
  (declare (optimize speed))
  (%sphere/obb sphere obb))

(u:fn-> obb/sphere (obb:obb sphere:sphere) boolean)
(defun obb/sphere (obb sphere)
  "Test if an OBB intersects a sphere."
  (declare (optimize speed))
  (%sphere/obb sphere obb))

(u:fn-> %sphere/plane (sphere:sphere plane:plane) boolean)
(declaim (inline %sphere/plane))
(defun %sphere/plane (sphere plane)
  "Helper function that does the work for SPHERE/PLANE and PLANE/SPHERE."
  (declare (optimize speed))
  (let* ((sphere-origin (sphere:origin sphere))
         (closest-point (closest-point-plane plane sphere-origin)))
    (< (point3d:distance-squared closest-point sphere-origin)
       (expt (sphere:radius sphere) 2))))

(u:fn-> sphere/plane (sphere:sphere plane:plane) boolean)
(defun sphere/plane (sphere plane)
  "Test if a sphere intersects a plane."
  (declare (optimize speed))
  (%sphere/plane sphere plane))

(u:fn-> plane/sphere (plane:plane sphere:sphere) boolean)
(defun plane/sphere (plane sphere)
  "Test if a plane intersects a sphere."
  (declare (optimize speed))
  (%sphere/plane sphere plane))

(u:fn-> aabb/aabb (aabb:aabb aabb:aabb) boolean)
(defun aabb/aabb (aabb1 aabb2)
  "Test if two AABBs intersect."
  (declare (optimize speed))
  (and (v3:<= (aabb:min aabb1) (aabb:max aabb2))
       (v3:<= (aabb:min aabb2) (aabb:max aabb1))))

(u:fn-> %aabb/obb (aabb:aabb obb:obb) boolean)
(declaim (inline %aabb/obb))
(defun %aabb/obb (aabb obb)
  "Helper function that does the work for AABB/OBB and OBB/AABB."
  (declare (optimize speed))
  (let* ((rotation (obb:rotation obb))
         (obb-x (m3:get-column rotation 0))
         (obb-y (m3:get-column rotation 1))
         (obb-z (m3:get-column rotation 2))
         (axes (vector v3:+right+
                       v3:+up+
                       v3:+forward+
                       obb-x
                       obb-y
                       obb-z
                       (v3:cross obb-x v3:+right+)
                       (v3:cross obb-x v3:+up+)
                       (v3:cross obb-x v3:+forward+)
                       (v3:cross obb-y v3:+right+)
                       (v3:cross obb-y v3:+up+)
                       (v3:cross obb-y v3:+forward+)
                       (v3:cross obb-z v3:+right+)
                       (v3:cross obb-z v3:+up+)
                       (v3:cross obb-z v3:+forward+))))
    (declare (dynamic-extent obb-x obb-y obb-z axes))
    (dotimes (i 15)
      (let ((x (aref axes i)))
        (v2:with-components ((i1 (aabb::interval aabb x))
                             (i2 (obb::interval obb x)))
          (unless (and (<= i2x i1y) (<= i1x i2y))
            (return-from %aabb/obb nil)))))
    t))

(u:fn-> aabb/obb (aabb:aabb obb:obb) boolean)
(defun aabb/obb (aabb obb)
  "Test if an AABB intersects an OBB."
  (declare (optimize speed))
  (%aabb/obb aabb obb))

(u:fn-> obb/aabb (obb:obb aabb:aabb) boolean)
(defun obb/aabb (obb aabb)
  "Test if an OBB intersects an AABB."
  (declare (optimize speed))
  (%aabb/obb aabb obb))

(u:fn-> %aabb/plane (aabb:aabb plane:plane) boolean)
(declaim (inline %aabb/plane))
(defun %aabb/plane (aabb plane)
  "Helper function that does the work for AABB/PLANE and PLANE/AABB."
  (declare (optimize speed))
  (let ((normal (plane:normal plane)))
    (<= (abs (- (v3:dot normal (aabb:origin aabb))
                (plane:distance plane)))
        (v3:dot (aabb:size aabb) (v3:abs normal)))))

(u:fn-> aabb/plane (aabb:aabb plane:plane) boolean)
(defun aabb/plane (aabb plane)
  "Test if an AABB intersects a plane."
  (declare (optimize speed))
  (%aabb/plane aabb plane))

(u:fn-> plane/aabb (plane:plane aabb:aabb) boolean)
(defun plane/aabb (plane aabb)
  "Test if a plane intersects an AABB."
  (declare (optimize speed))
  (%aabb/plane aabb plane))

(u:fn-> obb/obb (obb:obb obb:obb) boolean)
(defun obb/obb (obb1 obb2)
  "Test if two OBB's intersect."
  (declare (optimize speed))
  (let* ((rotation1 (obb:rotation obb1))
         (rotation2 (obb:rotation obb2))
         (obb1-x (m3:get-column rotation1 0))
         (obb1-y (m3:get-column rotation1 1))
         (obb1-z (m3:get-column rotation1 2))
         (obb2-x (m3:get-column rotation2 0))
         (obb2-y (m3:get-column rotation2 1))
         (obb2-z (m3:get-column rotation2 2))
         (axes (vector obb1-x
                       obb1-y
                       obb1-z
                       obb2-x
                       obb2-y
                       obb2-z
                       (v3:cross obb2-x obb1-x)
                       (v3:cross obb2-x obb1-y)
                       (v3:cross obb2-x obb1-z)
                       (v3:cross obb2-y obb1-x)
                       (v3:cross obb2-y obb1-y)
                       (v3:cross obb2-y obb1-z)
                       (v3:cross obb2-z obb1-x)
                       (v3:cross obb2-z obb1-y)
                       (v3:cross obb2-z obb1-z))))
    (declare (dynamic-extent obb1-x obb1-y obb1-z obb2-x obb2-y obb2-z axes))
    (dotimes (i 15)
      (let ((x (aref axes i)))
        (v2:with-components ((i1 (obb::interval obb1 x))
                             (i2 (obb::interval obb2 x)))
          (unless (and (<= i2x i1y) (<= i1x i2y))
            (return-from obb/obb nil)))))
    t))

(u:fn-> %obb/plane (obb:obb plane:plane) boolean)
(declaim (inline %obb/plane))
(defun %obb/plane (obb plane)
  "Helper function that does the work for OBB/PLANE and PLANE/OBB."
  (declare (optimize speed))
  (let* ((normal (plane:normal plane))
         (rotation (obb:rotation obb))
         (x (m3:get-column rotation 0))
         (y (m3:get-column rotation 1))
         (z (m3:get-column rotation 2))
         (vector (v3:abs
                  (v3:vec (v3:dot normal x)
                          (v3:dot normal y)
                          (v3:dot normal z)))))
    (declare (dynamic-extent x y z vector))
    (<= (abs (- (v3:dot normal (obb:origin obb))
                (plane:distance plane)))
        (v3:dot (obb:size obb) vector))))

(u:fn-> obb/plane (obb:obb plane:plane) boolean)
(defun obb/plane (obb plane)
  "Test if an OBB intersects a plane."
  (declare (optimize speed))
  (%obb/plane obb plane))

(u:fn-> plane/obb (plane:plane obb:obb) boolean)
(defun plane/obb (plane obb)
  "Test if a plane intersects an OBB."
  (declare (optimize speed))
  (%obb/plane obb plane))

(u:fn-> plane/plane (plane:plane plane:plane) boolean)
(defun plane/plane (plane1 plane2)
  "Test if two planes intersect."
  (declare (optimize speed))
  (let ((direction (v3:cross (plane:normal plane1) (plane:normal plane2))))
    (declare (dynamic-extent direction))
    (not (com:= (v3:length-squared direction) 0.0 1e-7 1e-7))))
