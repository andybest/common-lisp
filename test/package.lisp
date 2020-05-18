(in-package #:cl-user)

(defpackage #:net.mfiano.lisp.origin.test
  (:local-nicknames
   (#:const #:net.mfiano.lisp.origin.constants)
   (#:m2 #:net.mfiano.lisp.origin.mat2)
   (#:m3 #:net.mfiano.lisp.origin.mat3)
   (#:m4 #:net.mfiano.lisp.origin.mat4)
   (#:q #:net.mfiano.lisp.origin.quat)
   (#:v2 #:net.mfiano.lisp.origin.vec2)
   (#:v3 #:net.mfiano.lisp.origin.vec3)
   (#:v4 #:net.mfiano.lisp.origin.vec4))
  (:use #:cl
        #:parachute))
