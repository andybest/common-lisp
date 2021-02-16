(asdf:defsystem #:coherent-noise
  :description ""
  :author ("Michael Fiano <mail@mfiano.net>")
  :license "MIT"
  :homepage "https://mfiano.net/projects/coherent-noise"
  :source-control (:git "https://git.mfiano.net/mfiano/coherent-noise.git")
  :encoding :utf-8
  :depends-on (#:cl-cpus
               #:lparallel
               #:golden-utils
               #:seedable-rng
               #:uiop
               #:zpng)
  :in-order-to ((asdf:test-op (asdf:test-op #:coherent-noise.test)))
  :pathname "src"
  :serial t
  :components
  ((:file "package-internal")
   (:file "conditions")
   (:file "common")
   (:module "generators"
    :components
    ((:file "package")
     (:file "perlin-1d")
     (:file "perlin-2d")
     (:file "perlin-3d")
     (:file "perlin-4d")
     (:file "simplex-1d")
     (:file "simplex-2d")
     (:file "simplex-3d")
     (:file "simplex-4d")
     (:file "open-simplex-2d")
     (:file "open-simplex-3d")
     (:file "open-simplex-4d")
     (:file "open-simplex2f-2d")
     (:file "open-simplex2f-3d")
     (:file "open-simplex2f-4d")
     (:file "open-simplex2s-2d")
     (:file "open-simplex2s-3d")
     (:file "open-simplex2s-4d")
     (:file "value-2d")
     (:file "value-3d")
     (:file "cellular-2d")
     (:file "cellular-3d")
     (:file "cylinders-3d")
     (:file "spheres-3d")
     (:file "checker-2d")
     (:file "constant")))
   (:module "modifiers"
    :components
    ((:file "package")
     (:file "abs")
     (:file "add")
     (:file "billow")
     (:file "blend")
     (:file "clamp")
     (:file "displace")
     (:file "divide")
     (:file "expt")
     (:file "fractal")
     (:file "invert")
     (:file "max")
     (:file "min")
     (:file "multiply")
     (:file "power")
     (:file "ridged")
     (:file "ridged-multifractal")
     (:file "rotate")
     (:file "scale")
     (:file "select")
     (:file "strengthen")
     (:file "subtract")
     (:file "translate")
     (:file "turbulence")))
   (:file "package-api")))
