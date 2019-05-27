(in-package #:cl-user)

(defpackage #:box.math.common
  (:use #:cl)
  (:export #:int32
           #:+epsilon+))

(defpackage #:box.math.vec2
  (:use #:cl
        #:box.math.common)
  (:shadow #:=
           #:+
           #:-
           #:*
           #:/
           #:round
           #:abs
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
           #:make
           #:+zero+
           #:zero!
           #:zero
           #:one!
           #:one
           #:zero-p
           #:copy!
           #:copy
           #:clamp!
           #:clamp
           #:stabilize!
           #:stabilize
           #:to-list
           #:from-list
           #:=
           #:~
           #:+!
           #:+
           #:-!
           #:-
           #:*!
           #:*
           #:/!
           #:/
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
  (:use #:cl
        #:box.math.common)
  (:shadow #:=
           #:+
           #:-
           #:*
           #:/
           #:round
           #:abs
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
           #:make
           #:+zero+
           #:zero!
           #:zero
           #:one!
           #:one
           #:zero-p
           #:copy!
           #:copy
           #:clamp!
           #:clamp
           #:stabilize!
           #:stabilize
           #:to-list
           #:from-list
           #:=
           #:~
           #:+!
           #:+
           #:-!
           #:-
           #:*!
           #:*
           #:/!
           #:/
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
           #:parallel-p
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
  (:use #:cl
        #:box.math.common)
  (:shadow #:=
           #:+
           #:-
           #:*
           #:/
           #:round
           #:abs
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
           #:make
           #:+zero+
           #:zero!
           #:zero
           #:one!
           #:one
           #:zero-p
           #:copy!
           #:copy
           #:clamp!
           #:clamp
           #:stabilize!
           #:stabilize
           #:to-list
           #:from-list
           #:=
           #:~
           #:+!
           #:+
           #:-!
           #:-
           #:*!
           #:*
           #:/!
           #:/
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

(defpackage #:box.math.mat2
  (:local-nicknames (#:v2 #:box.math.vec2))
  (:use #:cl
        #:box.math.common)
  (:shadow #:=
           #:+
           #:-
           #:*
           #:trace)
  (:export #:matrix
           #:with-components
           #:mref
           #:+zero+
           #:+id+
           #:make
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
           #:+!
           #:+
           #:-!
           #:-
           #:*!
           #:*
           #:rotation-axis-to-vec2!
           #:rotation-axis-to-vec2
           #:rotation-axis-from-vec2!
           #:rotation-axis-from-vec2
           #:rotate!
           #:rotate
           #:scale-to-vec2!
           #:scale-to-vec2
           #:scale-from-vec2!
           #:scale-from-vec2
           #:scale!
           #:scale
           #:*v2!
           #:*v2
           #:transpose!
           #:transpose
           #:orthogonal-p
           #:trace
           #:diagonalp
           #:main-diagonal!
           #:main-diagonal
           #:anti-diagonal!
           #:anti-diagonal))

(defpackage #:box.math.mat3
  (:local-nicknames (#:v2 #:box.math.vec2)
                    (#:v3 #:box.math.vec3))
  (:use #:cl
        #:box.math.common)
  (:shadow #:=
           #:+
           #:-
           #:*
           #:trace)
  (:export #:matrix
           #:with-components
           #:mref
           #:+zero+
           #:+id+
           #:make
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
           #:+!
           #:+
           #:-!
           #:-
           #:*!
           #:*
           #:translation-to-vec2!
           #:translation-to-vec2
           #:translation-from-vec2!
           #:translation-from-vec2
           #:translate!
           #:translate
           #:copy-rotation!
           #:copy-rotation
           #:rotation-axis-to-vec2!
           #:rotation-axis-to-vec2
           #:rotation-axis-from-vec2!
           #:rotation-axis-from-vec2
           #:rotate!
           #:rotate
           #:scale-to-vec2!
           #:scale-to-vec2
           #:scale-from-vec2!
           #:scale-from-vec2
           #:scale!
           #:scale
           #:*v3!
           #:*v3
           #:transpose!
           #:transpose
           #:orthogonal-p
           #:trace
           #:diagonalp
           #:main-diagonal!
           #:main-diagonal
           #:anti-diagonal!
           #:anti-diagonal))

(defpackage #:box.math.mat4
  (:local-nicknames (#:v3 #:box.math.vec3)
                    (#:v4 #:box.math.vec4))
  (:use #:cl
        #:box.math.common)
  (:shadow #:=
           #:+
           #:-
           #:*
           #:trace)
  (:export #:matrix
           #:with-components
           #:mref
           #:+zero+
           #:+id+
           #:make
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
           #:+!
           #:+
           #:-!
           #:-
           #:*!
           #:*
           #:translation-to-vec3!
           #:translation-to-vec3
           #:translation-from-vec3!
           #:translation-from-vec3
           #:translate!
           #:translate
           #:copy-rotation!
           #:copy-rotation
           #:rotation-axis-to-vec3!
           #:rotation-axis-to-vec3
           #:rotation-axis-from-vec3!
           #:rotation-axis-from-vec3
           #:rotate!
           #:rotate
           #:scale-to-vec3!
           #:scale-to-vec3
           #:scale-from-vec3!
           #:scale-from-vec3
           #:scale!
           #:scale
           #:*v4!
           #:*v4
           #:transpose!
           #:transpose
           #:orthogonal-p
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
  (:local-nicknames (#:v3 #:box.math.vec3)
                    (#:v4 #:box.math.vec4)
                    (#:m3 #:box.math.mat3)
                    (#:m4 #:box.math.mat4))
  (:use #:cl
        #:box.math.common)
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
           #:to-mat3!
           #:to-mat3
           #:to-mat4!
           #:to-mat4
           #:from-mat3!
           #:from-mat3
           #:from-mat4!
           #:from-mat4
           #:slerp!
           #:slerp))
