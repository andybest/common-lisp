(in-package #:cl-user)

(defpackage #:mfiano.math.origin.test
  (:local-nicknames
   (#:const #:mfiano.math.origin.constants)
   (#:m2 #:mfiano.math.origin.mat2)
   (#:m3 #:mfiano.math.origin.mat3)
   (#:m4 #:mfiano.math.origin.mat4)
   (#:q #:mfiano.math.origin.quat)
   (#:v2 #:mfiano.math.origin.vec2)
   (#:v3 #:mfiano.math.origin.vec3)
   (#:v4 #:mfiano.math.origin.vec4))
  (:use #:cl
        #:parachute))
