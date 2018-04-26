(in-package :defpackage+-user-1)

(defpackage+ #:box.math.test
  (:local-nicknames (#:v2i #:box.math.vec2i)
                    (#:v2 #:box.math.vec2f)
                    (#:v3i #:box.math.vec3i)
                    (#:v3 #:box.math.vec3f)
                    (#:v4i #:box.math.vec4i)
                    (#:v4 #:box.math.vec4f)
                    (#:m2 #:box.math.mat2)
                    (#:m3 #:box.math.mat3)
                    (#:m4 #:box.math.mat4)
                    (#:q #:box.math.quat)
                    (#:dq #:box.math.dquat))
  (:use #:cl
        #:prove))
