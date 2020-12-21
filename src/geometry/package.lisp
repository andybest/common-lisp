(in-package #:cl-user)

(defpackage #:net.mfiano.lisp.origin.geometry
  (:local-nicknames
   (#:com #:net.mfiano.lisp.origin.common)
   (#:circle #:net.mfiano.lisp.origin.geometry.circle)
   (#:line2d #:net.mfiano.lisp.origin.geometry.line2d)
   (#:m2 #:net.mfiano.lisp.origin.mat2)
   (#:orect #:net.mfiano.lisp.origin.geometry.oriented-rect)
   (#:point2d #:net.mfiano.lisp.origin.geometry.point2d)
   (#:rect #:net.mfiano.lisp.origin.geometry.rect)
   (#:shape-set-2d #:net.mfiano.lisp.origin.geometry.shape-set-2d)
   (#:u #:net.mfiano.lisp.golden-utils)
   (#:v2 #:net.mfiano.lisp.origin.vec2))
  (:use #:cl)
  ;; bounding shapes
  (:export
   #:bounding-circle
   #:bounding-rect)
  ;; collision tests
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
   #:shape-set-2d/shape-set-2d))
