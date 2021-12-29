(in-package #:cl-user)

(defpackage #:mfiano.math.cubic-bezier
  (:local-nicknames
   (#:dv3 #:mfiano.math.origin.dvec3)
   (#:dv4 #:mfiano.math.origin.dvec4)
   (#:dm4 #:mfiano.math.origin.dmat4)
   (#:m4 #:mfiano.math.origin.mat4)
   (#:point3d #:mfiano.math.origin.geometry.point3d)
   (#:u #:mfiano.misc.utils)
   (#:v3 #:mfiano.math.origin.vec3)
   (#:v4 #:mfiano.math.origin.vec4))
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
