(in-package :defpackage+-user-1)

(defpackage+ #:box.math.quat
  (:local-nicknames (#:v3 #:box.math.vec3)
                    (#:v4 #:box.math.vec4)
                    (#:m4 #:box.math.mat4))
  (:use #:box.math.base)
  (:shadow #:=
           #:+
           #:-
           #:*
           #:conjugate)
  (:export #:quat
           #:with-components
           #:w
           #:x
           #:y
           #:z
           #:+zero+
           #:+id+
           #:id!
           #:id
           #:make
           #:zero!
           #:zero
           #:=
           #:~
           #:copy!
           #:copy
           #:+!
           #:+
           #:-!
           #:-
           #:*!
           #:*
           #:scale!
           #:scale
           #:conjugate!
           #:conjugate
           #:cross!
           #:cross
           #:magnitude-squared
           #:magnitude
           #:normalize!
           #:normalize
           #:negate!
           #:negate
           #:dot
           #:inverse!
           #:inverse
           #:rotate!
           #:rotate
           #:to-vec3!
           #:to-vec3
           #:to-vec4!
           #:to-vec4
           #:from-vec3!
           #:from-vec3
           #:from-vec4!
           #:from-vec4
           #:to-mat4!
           #:to-mat4
           #:from-mat4!
           #:from-mat4
           #:slerp!
           #:slerp))

(defpackage+ #:box.math.dquat
  (:local-nicknames (#:v3 #:box.math.vec3)
                    (#:v4 #:box.math.vec4)
                    (#:m4 #:box.math.mat4)
                    (#:q #:box.math.quat))
  (:use #:box.math.base)
  (:shadow #:=
           #:+
           #:-
           #:*
           #:conjugate
           #:apply)
  (:export #:dquat
           #:with-components
           #:make
           #:id!
           #:id
           #:zero!
           #:zero
           #:=
           #:~
           #:copy!
           #:copy
           #:+!
           #:+
           #:-!
           #:-
           #:*!
           #:*
           #:scale!
           #:scale
           #:conjugate!
           #:conjugate
           #:conjugate-full!
           #:conjugate-full
           #:magnitude-squared
           #:magnitude
           #:normalize!
           #:normalize
           #:negate!
           #:negate
           #:apply!
           #:apply
           #:dot
           #:inverse!
           #:inverse
           #:translation-to-vec3!
           #:translation-to-vec3
           #:translation-from-vec3!
           #:translation-from-vec3
           #:translate!
           #:translate
           #:rotation-to-quat!
           #:rotation-to-quat
           #:rotation-from-quat!
           #:rotation-from-quat
           #:rotate!
           #:rotate
           #:to-mat4!
           #:to-mat4
           #:from-mat4!
           #:from-mat4
           #:to-screw
           #:from-screw!
           #:from-screw
           #:sclerp!
           #:sclerp
           #:nlerp!
           #:nlerp))
