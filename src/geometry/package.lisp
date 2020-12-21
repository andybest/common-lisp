(in-package #:cl-user)

(defpackage #:net.mfiano.lisp.origin.geometry
  (:local-nicknames
   (#:aabb #:net.mfiano.lisp.origin.geometry.aabb)
   (#:com #:net.mfiano.lisp.origin.common)
   (#:circle #:net.mfiano.lisp.origin.geometry.circle)
   (#:line2d #:net.mfiano.lisp.origin.geometry.line2d)
   (#:line3d #:net.mfiano.lisp.origin.geometry.line3d)
   (#:m2 #:net.mfiano.lisp.origin.mat2)
   (#:m3 #:net.mfiano.lisp.origin.mat3)
   (#:obb #:net.mfiano.lisp.origin.geometry.obb)
   (#:orect #:net.mfiano.lisp.origin.geometry.oriented-rect)
   (#:plane #:net.mfiano.lisp.origin.geometry.plane)
   (#:point2d #:net.mfiano.lisp.origin.geometry.point2d)
   (#:point3d #:net.mfiano.lisp.origin.geometry.point3d)
   (#:rect #:net.mfiano.lisp.origin.geometry.rect)
   (#:shape-set-2d #:net.mfiano.lisp.origin.geometry.shape-set-2d)
   (#:sphere #:net.mfiano.lisp.origin.geometry.sphere)
   (#:u #:net.mfiano.lisp.golden-utils)
   (#:v2 #:net.mfiano.lisp.origin.vec2)
   (#:v3 #:net.mfiano.lisp.origin.vec3))
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
   #:aabb/point3d
   #:obb/point3d
   #:plane/point3d
   #:point3d/aabb
   #:point3d/obb
   #:point3d/plane
   #:point3d/point3d
   #:point3d/sphere
   #:sphere/point3d)
  ;; 3D closest point tests
  (:export))
