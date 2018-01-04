(in-package :defpackage+-user-1)

(defpackage+ #:box.math.vectors
  (:local-nicknames (#:v2 #:box.math.vec2)
                    (#:v3 #:box.math.vec3)
                    (#:v4 #:box.math.vec4))
  (:use #:cl))

(defpackage+ #:box.math.vec2
  (:use #:box.math.base)
  (:inherit #:box.math.vectors)
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
           #:make
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
  (:inherit #:box.math.vectors)
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
           #:make
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
  (:inherit #:box.math.vectors)
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
           #:make
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
