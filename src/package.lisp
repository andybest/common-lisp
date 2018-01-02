(uiop:define-package #:box.math.base
    (:use-reexport #:cl
                   #:defstar)
  (:export #:+epsilon+
           #:%make-accessor-symbol
           #:%~))

(defpackage #:box.math.vec2
  (:use #:box.math.base)
  (:shadow #:=
           #:+
           #:-
           #:round
           #:abs
           #:zerop
           #:<
           #:<=
           #:>
           #:>=
           #:min
           #:max)
  (:export #:vec
           #:x
           #:y
           #:with-components
           #:+zero+
           #:zero!
           #:zero
           #:zerop
           #:copy!
           #:copy
           #:clamp!
           #:clamp
           #:stabilize!
           #:stabilize
           #:->list
           #:=
           #:~
           #:+!
           #:+
           #:-!
           #:-
           #:hadamard*!
           #:hadamard*
           #:hadamard/!
           #:hadamard/
           #:scale!
           #:scale
           #:dot
           #:magnitude-squared
           #:magnitude
           #:normalize!
           #:normalize
           #:round!
           #:round
           #:abs!
           #:abs
           #:negate!
           #:negate
           #:angle
           #:direction=
           #:lerp!
           #:lerp
           #:<
           #:<=
           #:>
           #:>=
           #:min!
           #:min
           #:max!
           #:max))

(defpackage #:box.math.vec3
  (:use #:box.math.base)
  (:shadow #:=
           #:+
           #:-
           #:round
           #:abs
           #:zerop
           #:<
           #:<=
           #:>
           #:>=
           #:min
           #:max)
  (:export #:vec
           #:x
           #:y
           #:z
           #:with-components
           #:+zero+
           #:zero!
           #:zero
           #:zerop
           #:copy!
           #:copy
           #:clamp!
           #:clamp
           #:stabilize!
           #:stabilize
           #:->list
           #:=
           #:~
           #:+!
           #:+
           #:-!
           #:-
           #:hadamard*!
           #:hadamard*
           #:hadamard/!
           #:hadamard/
           #:scale!
           #:scale
           #:dot
           #:magnitude-squared
           #:magnitude
           #:normalize!
           #:normalize
           #:round!
           #:round
           #:abs!
           #:abs
           #:negate!
           #:negate
           #:cross!
           #:cross
           #:box
           #:angle
           #:direction=
           #:parallelp
           #:lerp!
           #:lerp
           #:<
           #:<=
           #:>
           #:>=
           #:min!
           #:min
           #:max!
           #:max))

(defpackage #:box.math.vec4
  (:use #:box.math.base)
  (:shadow #:=
           #:+
           #:-
           #:round
           #:abs
           #:zerop
           #:<
           #:<=
           #:>
           #:>=
           #:min
           #:max)
  (:export #:vec
           #:x
           #:y
           #:z
           #:w
           #:with-components
           #:+zero+
           #:zero!
           #:zero
           #:zerop
           #:copy!
           #:copy
           #:clamp!
           #:clamp
           #:stabilize!
           #:stabilize
           #:->list
           #:=
           #:~
           #:+!
           #:+
           #:-!
           #:-
           #:hadamard*!
           #:hadamard*
           #:hadamard/!
           #:hadamard/
           #:scale!
           #:scale
           #:dot
           #:magnitude-squared
           #:magnitude
           #:normalize!
           #:normalize
           #:round!
           #:round
           #:abs!
           #:abs
           #:negate!
           #:negate
           #:angle
           #:lerp!
           #:lerp
           #:<
           #:<=
           #:>
           #:>=
           #:min!
           #:min
           #:max!
           #:max))

(defpackage #:box.math.mat4
  (:use #:box.math.base)
  (:local-nicknames (#:v3 #:box.math.vec3)
                    (#:v4 #:box.math.vec4))
  (:shadow #:=
           #:*
           #:trace)
  (:export #:matrix
           #:with-components
           #:mref
           #:+zero+
           #:+id+
           #:zero!
           #:zero
           #:id!
           #:id
           #:=
           #:~
           #:copy!
           #:copy
           #:clamp!
           #:clamp
           #:*!
           #:*
           #:translation->v3!
           #:translation->v3
           #:v3->translation!
           #:v3->translation
           #:translate!
           #:translate
           #:copy-rotation!
           #:copy-rotation
           #:rotation->v3!
           #:rotation->v3
           #:v3->rotation!
           #:v3->rotation
           #:rotate!
           #:rotate
           #:scale->v3!
           #:scale->v3
           #:v3->scale!
           #:v3->scale
           #:scale!
           #:scale
           #:*v3!
           #:*v3
           #:*v4!
           #:*v4
           #:transpose!
           #:transpose
           #:orthogonalp
           #:orthonormalize!
           #:orthonormalize
           #:trace
           #:diagonalp
           #:main-diagonal!
           #:main-diagonal
           #:anti-diagonal!
           #:anti-diagonal
           #:determinant
           #:invert-orthogonal!
           #:invert-orthogonal
           #:invert!
           #:invert
           #:view!
           #:view
           #:orthographic-projection!
           #:orthographic-projection
           #:perspective-projection!
           #:perspective-projection))

(defpackage #:box.math.quat
  (:use #:box.math.base)
  (:local-nicknames (#:v3 #:box.math.vec3)
                    (#:v4 #:box.math.vec4)
                    (#:m4 #:box.math.mat4))
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
           #:->v3!
           #:->v3
           #:->v4!
           #:->v4
           #:v3->q!
           #:v3->q
           #:v4->q!
           #:v4->q
           #:->m4!
           #:->m4
           #:m4->q!
           #:m4->q
           #:slerp!
           #:slerp))

(defpackage #:box.math.dquat
  (:use #:box.math.base)
  (:local-nicknames (#:v3 #:box.math.vec3)
                    (#:v4 #:box.math.vec4)
                    (#:m4 #:box.math.mat4)
                    (#:q #:box.math.quat))
  (:shadow #:=
           #:+
           #:-
           #:*
           #:conjugate
           #:apply)
  (:export #:dquat
           #:with-components
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
           #:translation->v3!
           #:translation->v3
           #:v3->translation!
           #:v3->translation
           #:translate!
           #:translate
           #:rotation->q!
           #:rotation->q
           #:q->rotation!
           #:q->rotation
           #:rotate!
           #:rotate
           #:->m4!
           #:->m4
           #:m4->dq!
           #:m4->dq
           #:->screw
           #:screw->dq!
           #:screw->dq
           #:sclerp!
           #:sclerp
           #:nlerp!
           #:nlerp))
