(in-package #:cl-user)

(defpackage #:coherent-noise
  (:use #:cl)
  ;; Generators
  (:import-from #:coherent-noise.generators.perlin-1d #:perlin-1d)
  (:import-from #:coherent-noise.generators.perlin-2d #:perlin-2d)
  (:import-from #:coherent-noise.generators.perlin-3d #:perlin-3d)
  (:import-from #:coherent-noise.generators.perlin-4d #:perlin-4d)
  (:import-from #:coherent-noise.generators.simplex-1d #:simplex-1d)
  (:import-from #:coherent-noise.generators.simplex-2d #:simplex-2d)
  (:import-from #:coherent-noise.generators.simplex-3d #:simplex-3d)
  (:import-from #:coherent-noise.generators.simplex-4d #:simplex-4d)
  (:import-from #:coherent-noise.generators.open-simplex-2d #:open-simplex-2d)
  (:import-from #:coherent-noise.generators.open-simplex-3d #:open-simplex-3d)
  (:import-from #:coherent-noise.generators.open-simplex-4d #:open-simplex-4d)
  (:import-from #:coherent-noise.generators.open-simplex2f-2d #:open-simplex2f-2d)
  (:import-from #:coherent-noise.generators.open-simplex2f-3d #:open-simplex2f-3d)
  (:import-from #:coherent-noise.generators.open-simplex2f-4d #:open-simplex2f-4d)
  (:import-from #:coherent-noise.generators.open-simplex2s-2d #:open-simplex2s-2d)
  (:import-from #:coherent-noise.generators.open-simplex2s-3d #:open-simplex2s-3d)
  (:import-from #:coherent-noise.generators.open-simplex2s-4d #:open-simplex2s-4d)
  (:import-from #:coherent-noise.generators.value-2d #:value-2d)
  (:import-from #:coherent-noise.generators.value-3d #:value-3d)
  (:import-from #:coherent-noise.generators.cellular-2d #:cellular-2d)
  (:import-from #:coherent-noise.generators.cellular-3d #:cellular-3d)
  (:import-from #:coherent-noise.generators.cylinders-3d #:cylinders-3d)
  (:import-from #:coherent-noise.generators.spheres-3d #:spheres-3d)
  (:import-from #:coherent-noise.generators.checker-2d #:checker-2d)
  (:import-from #:coherent-noise.generators.constant #:constant)
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
   #:constant)
  ;; Modifiers
  (:shadowing-import-from #:coherent-noise.modifiers.add #:+)
  (:shadowing-import-from #:coherent-noise.modifiers.subtract #:-)
  (:shadowing-import-from #:coherent-noise.modifiers.multiply #:*)
  (:shadowing-import-from #:coherent-noise.modifiers.divide #:/)
  (:shadowing-import-from #:coherent-noise.modifiers.abs #:abs)
  (:shadowing-import-from #:coherent-noise.modifiers.expt #:expt)
  (:shadowing-import-from #:coherent-noise.modifiers.max #:max)
  (:shadowing-import-from #:coherent-noise.modifiers.min #:min)
  (:import-from #:coherent-noise.modifiers.billow #:billow)
  (:import-from #:coherent-noise.modifiers.blend #:blend)
  (:import-from #:coherent-noise.modifiers.clamp #:clamp)
  (:import-from #:coherent-noise.modifiers.displace #:displace)
  (:import-from #:coherent-noise.modifiers.fractal #:fractal)
  (:import-from #:coherent-noise.modifiers.invert #:invert)
  (:import-from #:coherent-noise.modifiers.power #:power)
  (:import-from #:coherent-noise.modifiers.ridged #:ridged)
  (:import-from #:coherent-noise.modifiers.ridged-multifractal #:ridged-multifractal)
  (:import-from #:coherent-noise.modifiers.rotate #:rotate)
  (:import-from #:coherent-noise.modifiers.scale #:scale #:uniform-scale)
  (:import-from #:coherent-noise.modifiers.select #:select)
  (:import-from #:coherent-noise.modifiers.strengthen #:strengthen)
  (:import-from #:coherent-noise.modifiers.translate #:translate)
  (:import-from #:coherent-noise.modifiers.turbulence #:turbulence)
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
   #:uniform-scale)
  ;; API
  (:import-from #:coherent-noise.internal #:sample #:write-image)
  (:export
   #:sample
   #:write-image))
