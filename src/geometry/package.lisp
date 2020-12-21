(in-package #:cl-user)

(defpackage #:net.mfiano.lisp.origin.geometry
  (:local-nicknames
   (#:com #:net.mfiano.lisp.origin.common)
   (#:circle #:net.mfiano.lisp.origin.geometry.circle)
   (#:line #:net.mfiano.lisp.origin.geometry.line2d)
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
   #:circle/oriented-rect
   #:circle/rect
   #:circle/line
   #:line/circle
   #:line/oriented-rect
   #:line/rect
   #:oriented-rect/circle
   #:oriented-rect/line
   #:oriented-rect/oriented-rect
   #:oriented-rect/rect
   #:point-in-rect-p
   #:point-in-circle-p
   #:point-in-oriented-rect-p
   #:point-on-line-p
   #:rect/rect
   #:rect/circle
   #:rect/line
   #:rect/oriented-rect))
