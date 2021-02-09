(in-package #:cl-user)

(defpackage #:cubic-bezier
  (:local-nicknames
   (#:dv3 #:origin.dvec3)
   (#:dv4 #:origin.dvec4)
   (#:dm4 #:origin.dmat4)
   (#:m4 #:origin.mat4)
   (#:point3d #:origin.geometry.point3d)
   (#:u #:golden-utils)
   (#:v3 #:origin.vec3)
   (#:v4 #:origin.vec4))
  (:use #:cl)
  (:export
   #:add-points
   #:collect-points
   #:collect-segments
   #:curve
   #:edit-point
   #:evaluate
   #:make-curve
   #:point-count-valid-p
   #:point-index-present-p))
