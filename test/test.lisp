(in-package #:coherent-noise.test)

(plan 3)

(subtest "generators"
  (plan 25)
  (is-values (compare 'generate-perlin-1d
                      (cn:uniform-scale (cn:perlin-1d :seed "default") 10))
             '(t t)
             "perlin-1d")
  (is-values (compare 'generate-perlin-2d
                      (cn:uniform-scale (cn:perlin-2d :seed "default") 10))
             '(t t)
             "perlin-2d")
  (is-values (compare 'generate-perlin-3d
                      (cn:uniform-scale (cn:perlin-3d :seed "default") 10))
             '(t t)
             "perlin-2d")
  (is-values (compare 'generate-perlin-4d
                      (cn:uniform-scale (cn:perlin-4d :seed "default") 10))
             '(t t)
             "perlin-2d")
  (is-values (compare 'generate-simplex-1d
                      (cn:uniform-scale (cn:simplex-1d :seed "default") 10))
             '(t t)
             "simplex-1d")
  (is-values (compare 'generate-simplex-2d
                      (cn:uniform-scale (cn:simplex-2d :seed "default") 10))
             '(t t)
             "simplex-2d")
  (is-values (compare 'generate-simplex-3d
                      (cn:uniform-scale (cn:simplex-3d :seed "default") 10))
             '(t t)
             "simplex-3d")
  (is-values (compare 'generate-simplex-4d
                      (cn:uniform-scale (cn:simplex-4d :seed "default") 10))
             '(t t)
             "simplex-4d")
  (is-values (compare 'generate-open-simplex-2d
                      (cn:uniform-scale (cn:open-simplex-2d :seed "default") 10))
             '(t t)
             "open-simplex-2d")
  (is-values (compare 'generate-open-simplex-3d
                      (cn:uniform-scale (cn:open-simplex-3d :seed "default") 10))
             '(t t)
             "open-simplex-3d")
  (is-values (compare 'generate-open-simplex-4d
                      (cn:uniform-scale (cn:open-simplex-4d :seed "default") 10))
             '(t t)
             "open-simplex-4d")
  (is-values (compare 'generate-open-simplex2f-2d
                      (cn:uniform-scale (cn:open-simplex2f-2d :seed "default") 10))
             '(t t)
             "open-simplex2f-2d")
  (is-values (compare 'generate-open-simplex2f-3d
                      (cn:uniform-scale (cn:open-simplex2f-3d :seed "default") 10))
             '(t t)
             "open-simplex2f-3d")
  (is-values (compare 'generate-open-simplex2f-4d
                      (cn:uniform-scale (cn:open-simplex2f-4d :seed "default") 10))
             '(t t)
             "open-simplex2f-4d")
  (is-values (compare 'generate-open-simplex2s-2d
                      (cn:uniform-scale (cn:open-simplex2s-2d :seed "default") 10))
             '(t t)
             "open-simplex2s-2d")
  (is-values (compare 'generate-open-simplex2s-3d
                      (cn:uniform-scale (cn:open-simplex2s-3d :seed "default") 10))
             '(t t)
             "open-simplex2s-3d")
  (is-values (compare 'generate-open-simplex2s-4d
                      (cn:uniform-scale (cn:open-simplex2s-4d  :seed "default") 10))
             '(t t)
             "open-simplex2s-4d")
  (is-values (compare 'generate-value-2d
                      (cn:uniform-scale (cn:value-2d :seed "default") 10))
             '(t t)
             "value-2d")
  (is-values (compare 'generate-value-3d
                      (cn:uniform-scale (cn:value-3d :seed "default") 10))
             '(t t)
             "value-3d")
  (is-values (compare 'generate-cellular-2d
                      (cn:uniform-scale (cn:cellular-2d :seed "default") 10))
             '(t t)
             "cellular-2d")
  (is-values (compare 'generate-cellular-3d
                      (cn:uniform-scale (cn:cellular-3d :seed "default") 10))
             '(t t)
             "cellular-3d")
  (is-values (compare 'generate-cylinders-3d
                      (cn:uniform-scale (cn:cylinders-3d :seed "default") 10))
             '(t t)
             "cylinders-3d")
  (is-values (compare 'generate-spheres-3d
                      (cn:uniform-scale (cn:spheres-3d :seed "default") 10))
             '(t t)
             "spheres-3d")
  (is-values (compare 'generate-checker-2d
                      (cn:uniform-scale (cn:checker-2d :seed "default") 10))
             '(t t)
             "checker-2d")
  (is-values (compare 'generate-constant
                      (cn:uniform-scale (cn:constant 0.5 :seed "default") 10))
             '(t t)
             "constant")
  (finalize))

(subtest "seed"
  (plan 21)
  (is-values (compare 'generate-perlin-1d
                      (cn:uniform-scale (cn:perlin-1d :seed "new-seed") 10))
             '(nil t)
             "perlin-1d")
  (is-values (compare 'generate-perlin-2d
                      (cn:uniform-scale (cn:perlin-2d :seed "new-seed") 10))
             '(nil t)
             "perlin-2d")
  (is-values (compare 'generate-perlin-3d
                      (cn:uniform-scale (cn:perlin-3d :seed "new-seed") 10))
             '(nil t)
             "perlin-3d")
  (is-values (compare 'generate-perlin-4d
                      (cn:uniform-scale (cn:perlin-4d :seed "new-seed") 10))
             '(nil t)
             "perlin-4d")
  (is-values (compare 'generate-simplex-1d
                      (cn:uniform-scale (cn:simplex-1d :seed "new-seed") 10))
             '(nil t)
             "simplex-1d")
  (is-values (compare 'generate-simplex-2d
                      (cn:uniform-scale (cn:simplex-2d :seed "new-seed") 10))
             '(nil t)
             "simplex-2d")
  (is-values (compare 'generate-simplex-3d
                      (cn:uniform-scale (cn:simplex-3d :seed "new-seed") 10))
             '(nil t)
             "simplex-3d")
  (is-values (compare 'generate-simplex-4d
                      (cn:uniform-scale (cn:simplex-4d :seed "new-seed") 10))
             '(nil t)
             "simplex-4d")
  (is-values (compare 'generate-open-simplex-2d
                      (cn:uniform-scale (cn:open-simplex-2d :seed "new-seed") 10))
             '(nil t)
             "open-simplex-2d")
  (is-values (compare 'generate-open-simplex-3d
                      (cn:uniform-scale (cn:open-simplex-3d :seed "new-seed") 10))
             '(nil t)
             "open-simplex-3d")
  (is-values (compare 'generate-open-simplex-4d
                      (cn:uniform-scale (cn:open-simplex-4d :seed "new-seed") 10))
             '(nil t)
             "open-simplex-4d")
  (is-values (compare 'generate-open-simplex2f-2d
                      (cn:uniform-scale (cn:open-simplex2f-2d :seed "new-seed") 10))
             '(nil t)
             "open-simplex2f-2d")
  (is-values (compare 'generate-open-simplex2f-3d
                      (cn:uniform-scale (cn:open-simplex2f-3d :seed "new-seed") 10))
             '(nil t)
             "open-simplex2f-3d")
  (is-values (compare 'generate-open-simplex2f-4d
                      (cn:uniform-scale (cn:open-simplex2f-4d :seed "new-seed") 10))
             '(nil t)
             "open-simplex2f-4d")
  (is-values (compare 'generate-open-simplex2s-2d
                      (cn:uniform-scale (cn:open-simplex2s-2d :seed "new-seed") 10))
             '(nil t)
             "open-simplex2s-2d")
  (is-values (compare 'generate-open-simplex2s-3d
                      (cn:uniform-scale (cn:open-simplex2s-3d :seed "new-seed") 10))
             '(nil t)
             "open-simplex2s-3d")
  (is-values (compare 'generate-open-simplex2s-4d
                      (cn:uniform-scale (cn:open-simplex2s-4d :seed "new-seed") 10))
             '(nil t)
             "open-simplex2s-4d")
  (is-values (compare 'generate-value-2d
                      (cn:uniform-scale (cn:value-2d :seed "new-seed") 10))
             '(nil t)
             "value-2d")
  (is-values (compare 'generate-value-3d
                      (cn:uniform-scale (cn:value-3d :seed "new-seed") 10))
             '(nil t)
             "value-3d")
  (is-values (compare 'generate-cellular-2d
                      (cn:uniform-scale (cn:cellular-2d :seed "new-seed") 10))
             '(nil t)
             "cellular-2d")
  (is-values (compare 'generate-cellular-3d
                      (cn:uniform-scale (cn:cellular-3d :seed "new-seed") 10))
             '(nil t)
             "cellular-3d")
  (finalize))

(subtest "modifiers"
  (plan 20)
  (is-values (compare 'modify-abs
                      (cn:abs (cn:uniform-scale (cn:perlin-3d :seed "default") 10)))
             '(t t)
             "abs")
  (is-values (compare 'modify-billow
                      (cn:billow (cn:uniform-scale (cn:perlin-3d :seed "default") 10)))
             '(t t)
             "billow")
  (is-values (compare 'modify-blend
                      (cn:blend (cn:uniform-scale (cn:perlin-3d :seed "default") 10)
                                (cn:uniform-scale (cn:simplex-2d :seed "default") 8)
                                (cn:uniform-scale (cn:open-simplex-3d :seed "default") 10)))
             '(t t)
             "blend")
  (is-values (compare 'modify-clamp
                      (cn:clamp (cn:uniform-scale (cn:perlin-3d :seed "default") 10) -0.2 0.4))
             '(t t)
             "clamp")
  (is-values (compare 'modify-displace
                      (cn:displace (cn:uniform-scale (cn:perlin-3d :seed "default") 10)
                                   :x (cn:uniform-scale (cn:simplex-3d :seed "default") 8)
                                   :y (cn:uniform-scale (cn:open-simplex-2d :seed "default") 9)
                                   :z (cn:uniform-scale (cn:perlin-2d :seed "default") 7)))
             '(t t)
             "displace")
  (is-values (compare 'modify-expt
                      (cn:expt (cn:uniform-scale (cn:perlin-3d :seed "default") 10) 2))
             '(t t)
             "expt")
  (is-values (compare 'modify-fractal
                      (cn:fractal (cn:uniform-scale (cn:perlin-3d :seed "default") 10)))
             '(t t)
             "fract")
  (is-values (compare 'modify-invert
                      (cn:invert (cn:uniform-scale (cn:perlin-3d :seed "default") 10)))
             '(t t)
             "invert")
  (is-values (compare 'modify-max
                      (cn:max (cn:uniform-scale (cn:perlin-3d :seed "default") 10)
                              (cn:uniform-scale (cn:simplex-3d :seed "default") 10)))
             '(t t)
             "max")
  (is-values (compare 'modify-min
                      (cn:min (cn:uniform-scale (cn:perlin-3d :seed "default") 10)
                              (cn:uniform-scale (cn:simplex-3d :seed "default") 10)))
             '(t t)
             "min")
  (is-values (compare 'modify-power
                      (cn:power (cn:uniform-scale (cn:perlin-3d :seed "default") 10)
                                (cn:uniform-scale (cn:simplex-3d :seed "default") 10)))
             '(t t)
             "power")
  (is-values (compare 'modify-ridged
                      (cn:ridged (cn:uniform-scale (cn:perlin-3d :seed "default") 10)))
             '(t t)
             "ridged")
  (is-values (compare 'modify-ridged-multifractal
                      (cn:ridged-multifractal (cn:uniform-scale (cn:perlin-3d :seed "default") 10)))
             '(t t)
             "ridged-multifractal")
  (is-values (compare 'modify-rotate
                      (cn:rotate (cn:uniform-scale (cn:simplex-3d :seed "default") 10) :z (/ pi 4)))
             '(t t)
             "rotate")
  (is-values (compare 'modify-scale
                      (cn:scale (cn:simplex-3d :seed "default") :x 5 :y 10 :z 15))
             '(t t)
             "scale")
  (is-values (compare 'modify-select
                      (cn:select (cn:uniform-scale (cn:perlin-3d :seed "default") 10)
                                 (cn:uniform-scale (cn:simplex-2d :seed "default") 9)
                                 (cn:uniform-scale (cn:open-simplex-2d :seed "default") 8)
                                 :min -0.5
                                 :max 0.2
                                 :falloff 0.5))
             '(t t)
             "select")
  (is-values (compare 'modify-strengthen
                      (cn:strengthen
                       (cn:uniform-scale (cn:perlin-3d :seed "default") 10)
                       1.5
                       :bias 0.1))
             '(t t)
             "strengthen")
  (is-values (compare 'modify-translate
                      (cn:translate (cn:uniform-scale (cn:perlin-3d :seed "default") 10)
                                    :x 5
                                    :y 10
                                    :z 15))
             '(t t)
             "translate")
  (is-values (compare 'modify-turbulence
                      (cn:turbulence (cn:uniform-scale (cn:perlin-3d :seed "default") 10)
                                     (cn:perlin-3d :seed "default")))
             '(t t)
             "turbulence")
  (is-values (compare 'modify-uniform-scale
                      (cn:uniform-scale (cn:perlin-3d :seed "default") 5))
             '(t t)
             "uniform-scale")
  (finalize))

(finalize)
