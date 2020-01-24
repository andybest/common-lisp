(in-package #:cl-user)

(defpackage #:origin.internal
  (:local-nicknames (#:a #:alexandria)
                    (#:u #:golden-utils))
  (:use #:cl)
  (:import-from
   #+sbcl #:sb-ext
   #+ccl #:ccl
   #+(or ecl abcl clasp) #:ext
   #+lispworks #:hcl
   #+allegro #:excl
   #:add-package-local-nickname)
  (:import-from #:specialization-store #:defstore #:defspecialization)
  (:export
   #:add-package-local-nickname
   #:make-accessor-symbol
   #:define-op
   #:defstore
   #:defspecialization))

(defpackage #:origin.vec2
  (:use #:cl #:origin.internal)
  (:shadow
   #:=
   #:+
   #:-
   #:*
   #:/
   #:random
   #:length
   #:round
   #:abs
   #:<
   #:<=
   #:>
   #:>=
   #:min
   #:max
   #:expt
   #:sqrt
   #:floor
   #:ceiling
   #:mod
   #:sin
   #:cos
   #:tan
   #:asin
   #:acos
   #:atan)
  (:export
   #:vec
   #:x
   #:y
   #:with-components
   #:with-elements
   #:+zero+
   #:zero!
   #:zero-p
   #:random!
   #:random
   #:copy!
   #:copy
   #:sign!
   #:sign
   #:fract!
   #:fract
   #:clamp!
   #:clamp
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
   #:length-squared
   #:length
   #:distance-squared
   #:distance
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
   #:max
   #:radians!
   #:radians
   #:degrees!
   #:degrees
   #:expt!
   #:expt
   #:sqrt!
   #:sqrt
   #:floor!
   #:floor
   #:ceiling!
   #:ceiling
   #:mod!
   #:mod
   #:sin!
   #:sin
   #:cos!
   #:cos
   #:tan!
   #:tan
   #:asin!
   #:asin
   #:acos!
   #:acos
   #:atan!
   #:atan))

(defpackage #:origin.vec3
  (:use #:cl #:origin.internal)
  (:shadow
   #:=
   #:+
   #:-
   #:*
   #:/
   #:random
   #:length
   #:round
   #:abs
   #:<
   #:<=
   #:>
   #:>=
   #:min
   #:max
   #:expt
   #:sqrt
   #:floor
   #:ceiling
   #:mod
   #:sin
   #:cos
   #:tan
   #:asin
   #:acos
   #:atan)
  (:export
   #:vec
   #:x
   #:y
   #:z
   #:with-components
   #:with-elements
   #:+zero+
   #:+up+
   #:+down+
   #:+left+
   #:+right+
   #:+forward+
   #:+back+
   #:zero!
   #:zero-p
   #:random!
   #:random
   #:copy!
   #:copy
   #:sign!
   #:sign
   #:fract!
   #:fract
   #:clamp!
   #:clamp
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
   #:length-squared
   #:length
   #:distance-squared
   #:distance
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
   #:max
   #:radians!
   #:radians
   #:degrees!
   #:degrees
   #:expt!
   #:expt
   #:sqrt!
   #:sqrt
   #:floor!
   #:floor
   #:ceiling!
   #:ceiling
   #:mod!
   #:mod
   #:sin!
   #:sin
   #:cos!
   #:cos
   #:tan!
   #:tan
   #:asin!
   #:asin
   #:acos!
   #:acos
   #:atan!
   #:atan))

(defpackage #:origin.vec4
  (:use #:cl #:origin.internal)
  (:shadow
   #:=
   #:+
   #:-
   #:*
   #:/
   #:random
   #:length
   #:round
   #:abs
   #:<
   #:<=
   #:>
   #:>=
   #:min
   #:max
   #:expt
   #:sqrt
   #:floor
   #:ceiling
   #:mod
   #:sin
   #:cos
   #:tan
   #:asin
   #:acos
   #:atan)
  (:export
   #:vec
   #:x
   #:y
   #:z
   #:w
   #:with-components
   #:with-elements
   #:+zero+
   #:zero!
   #:zero-p
   #:random!
   #:random
   #:copy!
   #:copy
   #:sign!
   #:sign
   #:fract!
   #:fract
   #:clamp!
   #:clamp
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
   #:length-squared
   #:length
   #:distance-squared
   #:distance
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
   #:max
   #:radians!
   #:radians
   #:degrees!
   #:degrees
   #:expt!
   #:expt
   #:sqrt!
   #:sqrt
   #:floor!
   #:floor
   #:ceiling!
   #:ceiling
   #:mod!
   #:mod
   #:sin!
   #:sin
   #:cos!
   #:cos
   #:tan!
   #:tan
   #:asin!
   #:asin
   #:acos!
   #:acos
   #:atan!
   #:atan))

(defpackage #:origin.mat2
  (:use #:cl #:origin.internal)
  (:shadow
   #:=
   #:+
   #:-
   #:*
   #:random
   #:trace)
  (:export
   #:mat
   #:with-components
   #:with-elements
   #:+zero+
   #:+id+
   #:zero!
   #:zero-p
   #:random
   #:id!
   #:id-p
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
   #:get-column!
   #:get-column
   #:set-column!
   #:set-column
   #:rotation-axis-to-vec2!
   #:rotation-axis-to-vec2
   #:rotation-axis-from-vec2!
   #:rotation-axis-from-vec2
   #:rotate!
   #:rotate
   #:get-scale!
   #:get-scale
   #:set-scale!
   #:set-scale
   #:scale!
   #:scale
   #:*v2!
   #:*v2
   #:transpose!
   #:transpose
   #:orthogonal-p
   #:trace
   #:diagonal-p
   #:main-diagonal!
   #:main-diagonal
   #:anti-diagonal!
   #:anti-diagonal))

(defpackage #:origin.mat3
  (:use #:cl #:origin.internal)
  (:shadow
   #:=
   #:+
   #:-
   #:*
   #:random
   #:trace)
  (:export
   #:mat
   #:with-components
   #:with-elements
   #:+zero+
   #:+id+
   #:zero!
   #:zero-p
   #:id!
   #:id-p
   #:=
   #:~
   #:random!
   #:random
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
   #:get-column!
   #:get-column
   #:set-column!
   #:set-column
   #:get-translation!
   #:get-translation
   #:set-translation!
   #:set-translation
   #:translate!
   #:translate
   #:copy-rotation!
   #:copy-rotation
   #:rotation-to-mat2!
   #:rotation-to-mat2
   #:normalize-rotation!
   #:normalize-rotation
   #:rotation-axis-to-vec2!
   #:rotation-axis-to-vec2
   #:rotation-axis-from-vec2!
   #:rotation-axis-from-vec2
   #:rotate!
   #:rotate
   #:get-scale!
   #:get-scale
   #:set-scale!
   #:set-scale
   #:scale!
   #:scale
   #:*v3!
   #:*v3
   #:transpose!
   #:transpose
   #:orthogonal-p
   #:trace
   #:diagonal-p
   #:main-diagonal!
   #:main-diagonal
   #:anti-diagonal!
   #:anti-diagonal))

(defpackage #:origin.mat4
  (:use #:cl #:origin.internal)
  (:shadow
   #:=
   #:+
   #:-
   #:*
   #:random
   #:trace)
  (:export
   #:mat
   #:with-components
   #:with-elements
   #:+zero+
   #:+id+
   #:zero!
   #:zero-p
   #:id!
   #:id-p
   #:=
   #:~
   #:random!
   #:random
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
   #:get-column!
   #:get-column
   #:set-column!
   #:set-column
   #:get-translation!
   #:get-translation
   #:set-translation!
   #:set-translation
   #:translate!
   #:translate
   #:copy-rotation!
   #:copy-rotation
   #:rotation-to-mat3!
   #:rotation-to-mat3
   #:normalize-rotation!
   #:normalize-rotation
   #:rotation-axis-to-vec3!
   #:rotation-axis-to-vec3
   #:rotation-axis-from-vec3!
   #:rotation-axis-from-vec3
   #:rotate!
   #:rotate
   #:get-scale!
   #:get-scale
   #:set-scale!
   #:set-scale
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
   #:diagonal-p
   #:main-diagonal!
   #:main-diagonal
   #:anti-diagonal!
   #:anti-diagonal
   #:determinant
   #:invert-orthogonal!
   #:invert-orthogonal
   #:invert!
   #:invert
   #:set-view!
   #:set-view
   #:set-projection/orthographic!
   #:set-projection/orthographic
   #:set-projection/perspective!
   #:set-projection/perspective))

(defpackage #:origin.quat
  (:use #:cl #:origin.internal)
  (:shadow
   #:=
   #:+
   #:-
   #:*
   #:conjugate
   #:length
   #:random)
  (:export
   #:quat
   #:with-components
   #:with-elements
   #:w
   #:x
   #:y
   #:z
   #:+zero+
   #:+id+
   #:id!
   #:id-p
   #:zero!
   #:=
   #:~
   #:random!
   #:random
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
   #:length-squared
   #:length
   #:normalize!
   #:normalize
   #:negate!
   #:negate
   #:dot
   #:inverse!
   #:inverse
   #:rotate-euler!
   #:rotate-euler
   #:rotate!
   #:rotate
   #:to-euler!
   #:to-euler
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
   #:slerp
   #:from-axis-angle!
   #:from-axis-angle
   #:orient!
   #:orient))

(defpackage #:origin
  (:use #:cl #:origin.internal)
  ;; math constants
  (:shadow
   #:pi)
  (:export
   #:pi/12
   #:pi/6
   #:2pi/12
   #:pi/4
   #:3pi/12
   #:pi/3
   #:4pi/12
   #:5pi/12
   #:pi/2
   #:6pi/12
   #:7pi/12
   #:2pi/3
   #:8pi/12
   #:3pi/4
   #:9pi/12
   #:5pi/6
   #:10pi/12
   #:11pi/12
   #:pi
   #:12pi/12
   #:13pi/12
   #:7pi/6
   #:14pi/12
   #:5pi/4
   #:15pi/12
   #:4pi/3
   #:16pi/12
   #:17pi/12
   #:3pi/2
   #:18pi/12
   #:19pi/12
   #:5pi/3
   #:20pi/12
   #:7pi/4
   #:21pi/12
   #:11pi/6
   #:22pi/12
   #:23pi/12
   #:2pi
   #:24pi/12
   #:+rad+
   #:+deg+)
  ;; general
  (:export
   #:line-direction
   #:line-plane-intersect
   #:line-point-distance
   #:line-segment-midpoint
   #:translate-point
   #:unproject!
   #:unproject)
  ;; shaping
  (:export
   #:linear
   #:sine-out
   #:sine-in
   #:sine-in-out
   #:quadratic-out
   #:quadratic-in
   #:quadratic-in-out
   #:cubic-out
   #:cubic-in
   #:cubic-in-out
   #:quartic-out
   #:quartic-in
   #:quartic-in-out
   #:quintic-out
   #:quintic-in
   #:quintic-in-out
   #:exponential-out
   #:exponential-in
   #:exponential-in-out
   #:circular-out
   #:circular-in
   #:circular-in-out
   #:back-out
   #:back-in
   #:back-in-out
   #:elastic-out
   #:elastic-in
   #:elastic-in-out
   #:bounce-out
   #:bounce-in
   #:bounce-in-out
   #:hermite-curve
   #:quintic-curve)
  ;; physics
  (:export
   #:make-velocity!
   #:make-velocity
   #:velocity->rotation!
   #:velocity->rotation))

(origin.internal:add-package-local-nickname :a :alexandria :origin.vec2)
(origin.internal:add-package-local-nickname :v3 :origin.vec3 :origin.vec2)
(origin.internal:add-package-local-nickname :v4 :origin.vec4 :origin.vec2)

(origin.internal:add-package-local-nickname :a :alexandria :origin.vec3)
(origin.internal:add-package-local-nickname :v2 :origin.vec2 :origin.vec3)
(origin.internal:add-package-local-nickname :v4 :origin.vec4 :origin.vec3)
(origin.internal:add-package-local-nickname :q :origin.quat :origin.vec3)

(origin.internal:add-package-local-nickname :a :alexandria :origin.vec4)
(origin.internal:add-package-local-nickname :v2 :origin.vec2 :origin.vec4)
(origin.internal:add-package-local-nickname :v3 :origin.vec3 :origin.vec4)
(origin.internal:add-package-local-nickname :q :origin.quat :origin.vec4)

(origin.internal:add-package-local-nickname :a :alexandria :origin.mat2)
(origin.internal:add-package-local-nickname :v2 :origin.vec2 :origin.mat2)
(origin.internal:add-package-local-nickname :m3 :origin.mat3 :origin.mat2)
(origin.internal:add-package-local-nickname :m4 :origin.mat4 :origin.mat2)

(origin.internal:add-package-local-nickname :a :alexandria :origin.mat3)
(origin.internal:add-package-local-nickname :v2 :origin.vec2 :origin.mat3)
(origin.internal:add-package-local-nickname :v3 :origin.vec3 :origin.mat3)
(origin.internal:add-package-local-nickname :m2 :origin.mat2 :origin.mat3)
(origin.internal:add-package-local-nickname :m4 :origin.mat4 :origin.mat3)

(origin.internal:add-package-local-nickname :a :alexandria :origin.mat4)
(origin.internal:add-package-local-nickname :v3 :origin.vec3 :origin.mat4)
(origin.internal:add-package-local-nickname :v4 :origin.vec4 :origin.mat4)
(origin.internal:add-package-local-nickname :m2 :origin.mat2 :origin.mat4)
(origin.internal:add-package-local-nickname :m3 :origin.mat3 :origin.mat4)

(origin.internal:add-package-local-nickname :a :alexandria :origin.quat)
(origin.internal:add-package-local-nickname :v3 :origin.vec3 :origin.quat)
(origin.internal:add-package-local-nickname :v4 :origin.vec4 :origin.quat)
(origin.internal:add-package-local-nickname :m3 :origin.mat3 :origin.quat)
(origin.internal:add-package-local-nickname :m4 :origin.mat4 :origin.quat)

(origin.internal:add-package-local-nickname :a :alexandria :origin)
(origin.internal:add-package-local-nickname :u :golden-utils :origin)
(origin.internal:add-package-local-nickname :v2 :origin.vec2 :origin)
(origin.internal:add-package-local-nickname :v3 :origin.vec3 :origin)
(origin.internal:add-package-local-nickname :v4 :origin.vec4 :origin)
(origin.internal:add-package-local-nickname :m2 :origin.mat2 :origin)
(origin.internal:add-package-local-nickname :m3 :origin.mat3 :origin)
(origin.internal:add-package-local-nickname :m4 :origin.mat4 :origin)
(origin.internal:add-package-local-nickname :q :origin.quat :origin)
