(in-package #:cl-user)

(defpackage #:%coherent-noise.internal
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
   #:invalid-cellular-output-type
   #:invalid-fractal-octave-count
   #:invalid-open-simplex2-orientation
   #:invalid-real-argument
   #:invalid-sampler-argument
   #:invalid-seed))

(defpackage #:%coherent-noise.generators
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
   #:constant
   #:fbm-2d
   #:fbm-3d
   #:fbm-4d
   #:billow-2d
   #:billow-3d
   #:billow-4d
   #:multifractal-2d
   #:multifractal-3d
   #:multifractal-4d
   #:hybrid-multifractal-2d
   #:hybrid-multifractal-3d
   #:hybrid-multifractal-4d
   #:ridged-multifractal-2d
   #:ridged-multifractal-3d
   #:ridged-multifractal-4d))

(defpackage #:%coherent-noise.modifiers
  (:shadow #:+ #:- #:* #:/ #:abs #:expt #:max #:min)
  (:export
   #:+
   #:-
   #:*
   #:/
   #:abs
   #:blend
   #:cache
   #:clamp
   #:curve
   #:displace
   #:expt
   #:fractalize
   #:invert
   #:max
   #:min
   #:power
   #:rotate
   #:scale
   #:select
   #:strengthen
   #:terrace
   #:translate
   #:turbulence
   #:uniform-scale))

(defpackage #:%coherent-noise.map
  (:local-nicknames
   (#:int #:%coherent-noise.internal)
   (#:lp #:lparallel)
   (#:u #:golden-utils))
  (:use #:cl)
  (:shadow #:map)
  (:export
   #:define-gradient
   #:get-image-pixel
   #:image
   #:image-height
   #:image-width
   #:image-data
   #:make-map
   #:map
   #:map-data
   #:map-height
   #:map-value
   #:map-width
   #:render-map
   #:write-image))

(uiop:define-package #:coherent-noise
  (:import-from #:arrow-macros #:->)
  (:mix #:%coherent-noise.modifiers #:%coherent-noise.map #:cl)
  (:reexport #:%coherent-noise.modifiers #:%coherent-noise.map)
  (:mix-reexport #:%coherent-noise.generators #:%coherent-noise.internal)
  (:export #:->))
