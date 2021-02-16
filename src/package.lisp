(in-package #:cl-user)

(defpackage #:coherent-noise.internal
  (:local-nicknames
   (#:lp #:lparallel)
   (#:rng #:seedable-rng)
   (#:u #:golden-utils))
  (:use #:cl)
  ;; API
  (:export
   #:sample
   #:sampler
   #:write-image)
  ;; Conditions
  (:export
   #:coherent-noise-error
   #:invalid-cellular-distance-method
   #:invalid-cellular-jitter
   #:invalid-cellular-output-type
   #:invalid-modifier-input
   #:invalid-open-simplex2-orientation
   #:invalid-seed))

(defpackage #:coherent-noise.generators
  (:export
   #:perlin-1d
   #:perlin-2d
   #:perlin-3d
   #:perlin-4d
   #:simplex-1d
   #:simplex-2d
   #:simplex-3d
   #:simplex-4d
   #:open-simplex-2d
   #:open-simplex-3d
   #:open-simplex-4d
   #:open-simplex2f-2d
   #:open-simplex2f-3d
   #:open-simplex2f-4d
   #:open-simplex2s-2d
   #:open-simplex2s-3d
   #:open-simplex2s-4d
   #:value-2d
   #:value-3d
   #:cellular-2d
   #:cellular-3d
   #:cylinders-3d
   #:spheres-3d
   #:checker-2d
   #:constant))

(defpackage #:coherent-noise.modifiers
  (:shadow #:+ #:- #:* #:/ #:abs #:expt #:max #:min)
  (:export
   #:+
   #:-
   #:*
   #:/
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
   #:ridged
   #:ridged-multifractal
   #:rotate
   #:scale
   #:select
   #:strengthen
   #:translate
   #:turbulence
   #:uniform-scale))

(uiop:define-package #:coherent-noise
  (:mix #:coherent-noise.modifiers #:cl)
  (:reexport #:coherent-noise.modifiers)
  (:mix-reexport #:coherent-noise.generators
                 #:coherent-noise.internal))
