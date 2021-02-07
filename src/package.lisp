(in-package #:cl-user)

(defpackage #:coherent-noise/internal
  (:local-nicknames
   (#:lp #:lparallel)
   (#:rng #:seedable-rng)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:make-sampler
   #:sample
   #:write-image)
  ;; Samplers
  (:export
   #:cellular-2d
   #:cellular-3d
   #:checkered
   #:constant
   #:cylinders
   #:open-simplex-2d
   #:open-simplex-3d
   #:open-simplex-4d
   #:open-simplex2-fast-2d
   #:perlin-1d
   #:perlin-2d
   #:perlin-3d
   #:perlin-4d
   #:simplex-1d
   #:simplex-2d
   #:simplex-3d
   #:simplex-4d
   #:spheres
   #:value-2d
   #:value-3d)
  ;; Conditions
  (:export
   #:coherent-noise-error
   #:invalid-cellular-distance-method
   #:invalid-cellular-jitter
   #:invalid-cellular-output-type
   #:invalid-seed))

(defpackage #:coherent-noise/modifiers
  (:local-nicknames
   (#:cn #:coherent-noise/internal)
   (#:u #:golden-utils))
  (:use #:cl)
  (:shadow #:+ #:- #:* #:abs #:expt #:max #:min)
  (:export
   #:+
   #:-
   #:*
   #:abs
   #:billow
   #:blend
   #:clamp
   #:displace
   #:expt
   #:fractal
   #:invert
   #:max
   #:min
   #:power
   #:ridges
   #:ridges2
   #:rotate
   #:scale
   #:select
   #:strengthen
   #:translate
   #:turbulence
   #:uniform-scale))

(uiop:define-package #:coherent-noise
  (:mix #:coherent-noise/internal #:coherent-noise/modifiers #:cl)
  (:reexport #:coherent-noise/internal #:coherent-noise/modifiers))
