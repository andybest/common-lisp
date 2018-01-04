(in-package :defpackage+-1)

(defpackage+ #:box.math.mat2
  (:local-nicknames (#:v2 #:box.math.vec2))
  (:use #:box.math.base)
  (:shadow #:=
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
           #:orthogonalp
           #:trace
           #:diagonalp
           #:main-diagonal!
           #:main-diagonal
           #:anti-diagonal!
           #:anti-diagonal))

(defpackage+ #:box.math.mat3
  (:local-nicknames (#:v2 #:box.math.vec2)
                    (#:v3 #:box.math.vec3))
  (:use #:box.math.base)
  (:shadow #:=
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
           #:orthogonalp
           #:trace
           #:diagonalp
           #:main-diagonal!
           #:main-diagonal
           #:anti-diagonal!
           #:anti-diagonal))

(defpackage+ #:box.math.mat4
  (:local-nicknames (#:v3 #:box.math.vec3)
                    (#:v4 #:box.math.vec4))
  (:use #:box.math.base)
  (:shadow #:=
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
