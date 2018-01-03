(in-package :defpackage+-1)

(eval-when (:compile-toplevel :load-toplevel)
  (defun ensure-global-nickname (package nickname)
    (let ((package (find-package package)))
      (rename-package package
                      (package-name package)
                      (adjoin nickname
                              (package-nicknames package)
                              :key #'string
                              :test #'equal))))

  (defun add-local-nickname (package nickname local-to)
    (declare (ignorable package nickname local-to))
    #+sbcl
    (sb-ext:add-package-local-nickname nickname package local-to)
    #+(or abcl ecl)
    (ext:add-package-local-nickname nickname package local-to))

  (defmethod defpackage+-dispatch ((option (eql :local-nicknames))
                                   parameters package)
    (loop :for (nickname package-name) :in parameters
          :do (progn
                (ensure-package package-name)
                #+package-local-nicknames
                (add-local-nickname pack nickname package)
                #-package-local-nicknames
                (ensure-global-nickname package-name nickname)))))

(defpackage+ #:box.math.base
  (:inherit #:cl
            #:defstar)
  (:export #:+epsilon+
           #:%make-accessor-symbol
           #:%~))

(defpackage+ #:box.math.vec2
  (:use #:box.math.base)
  (:shadow #:=
           #:+
           #:-
           #:*
           #:/
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

(defpackage+ #:box.math.vec3
  (:use #:box.math.base)
  (:shadow #:=
           #:+
           #:-
           #:*
           #:/
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

(defpackage+ #:box.math.vec4
  (:use #:box.math.base)
  (:shadow #:=
           #:+
           #:-
           #:*
           #:/
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

(defpackage+ #:box.math.mat2
  (:use #:box.math.base)
  (:local-nicknames (#:v2 #:box.math.vec2))
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
  (:use #:box.math.base)
  (:local-nicknames (#:v2 #:box.math.vec2)
                    (#:v3 #:box.math.vec3))
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

(defpackage+ #:box.math.quat
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
