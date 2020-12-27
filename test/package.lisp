(in-package #:cl-user)

(defpackage #:origin.test
  (:local-nicknames
   (#:const #:origin.constants)
   (#:m2 #:origin.mat2)
   (#:m3 #:origin.mat3)
   (#:m4 #:origin.mat4)
   (#:q #:origin.quat)
   (#:v2 #:origin.vec2)
   (#:v3 #:origin.vec3)
   (#:v4 #:origin.vec4))
  (:use #:cl
        #:parachute))
