(defpackage #:box.math.test
  (:use #:cl
        #:prove)
  (:local-nicknames (#:v2 #:box.math.vec2)
                    (#:v3 #:box.math.vec3)
                    (#:v4 #:box.math.vec4)
                    (#:m2 #:box.math.mat2)
                    (#:m3 #:box.math.mat3)
                    (#:m4 #:box.math.mat4)
                    (#:q #:box.math.quat)
                    (#:dq #:box.math.dquat)))
