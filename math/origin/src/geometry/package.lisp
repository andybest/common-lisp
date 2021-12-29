(in-package #:cl-user)

(defpackage #:mfiano.math.origin.geometry
  (:local-nicknames
   (#:aabb #:mfiano.math.origin.geometry.aabb)
   (#:com #:mfiano.math.origin.common)
   (#:circle #:mfiano.math.origin.geometry.circle)
   (#:line2d #:mfiano.math.origin.geometry.line2d)
   (#:line3d #:mfiano.math.origin.geometry.line3d)
   (#:m2 #:mfiano.math.origin.mat2)
   (#:m3 #:mfiano.math.origin.mat3)
   (#:obb #:mfiano.math.origin.geometry.obb)
   (#:orect #:mfiano.math.origin.geometry.oriented-rect)
   (#:plane #:mfiano.math.origin.geometry.plane)
   (#:point2d #:mfiano.math.origin.geometry.point2d)
   (#:point3d #:mfiano.math.origin.geometry.point3d)
   (#:ray #:mfiano.math.origin.geometry.ray)
   (#:rect #:mfiano.math.origin.geometry.rect)
   (#:shape-set-2d #:mfiano.math.origin.geometry.shape-set-2d)
   (#:sphere #:mfiano.math.origin.geometry.sphere)
   (#:u #:mfiano.misc.utils)
   (#:v2 #:mfiano.math.origin.vec2)
   (#:v3 #:mfiano.math.origin.vec3))
  (:use #:cl)
  ;; bounding shapes
  (:export
   #:bounding-circle
   #:bounding-rect)
  ;; 2D intersection tests
  (:export
   #:circle/circle
   #:circle/line2d
   #:circle/oriented-rect
   #:circle/point2d
   #:circle/rect
   #:circle/shape-set-2d
   #:line2d/circle
   #:line2d/oriented-rect
   #:line2d/point2d
   #:line2d/rect
   #:line2d/shape-set-2d
   #:oriented-rect/circle
   #:oriented-rect/line2d
   #:oriented-rect/oriented-rect
   #:oriented-rect/point2d
   #:oriented-rect/rect
   #:oriented-rect/shape-set-2d
   #:point2d/circle
   #:point2d/line2d
   #:point2d/oriented-rect
   #:point2d/point2d
   #:point2d/rect
   #:point2d/shape-set-2d
   #:rect/circle
   #:rect/line2d
   #:rect/oriented-rect
   #:rect/point2d
   #:rect/rect
   #:rect/shape-set-2d
   #:shape-set-2d/circle
   #:shape-set-2d/line2d
   #:shape-set-2d/oriented-rect
   #:shape-set-2d/point2d
   #:shape-set-2d/rect
   #:shape-set-2d/shape-set-2d)
  ;; 3D intersection tests
  (:export
   #:aabb/aabb
   #:aabb/obb
   #:aabb/plane
   #:aabb/point3d
   #:aabb/sphere
   #:line3d/point3d
   #:obb/aabb
   #:obb/obb
   #:obb/plane
   #:obb/point3d
   #:obb/sphere
   #:plane/aabb
   #:plane/obb
   #:plane/plane
   #:plane/point3d
   #:plane/sphere
   #:point3d/aabb
   #:point3d/line3d
   #:point3d/obb
   #:point3d/plane
   #:point3d/point3d
   #:point3d/ray
   #:point3d/sphere
   #:ray/point3d
   #:sphere/aabb
   #:sphere/obb
   #:sphere/plane
   #:sphere/point3d
   #:sphere/sphere)
  ;; 3D closest point tests
  (:export
   #:closest-point-aabb
   #:closest-point-line3d
   #:closest-point-obb
   #:closest-point-plane
   #:closest-point-ray
   #:closest-point-sphere)
  ;; 3D raycast tests
  (:export
   #:raycast-aabb
   #:raycast-sphere))
