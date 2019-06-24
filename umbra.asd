(asdf:defsystem #:umbra
  :description "A library of reusable GPU shader functions."
  :author ("Michael Fiano <mail@michaelfiano.com>")
  :maintainer "Michael Fiano <mail@michaelfiano.com>"
  :license "MIT"
  :homepage "https://www.michaelfiano.com/projects/umbra"
  :source-control (:git "https://github.com/mfiano/umbra.git")
  :bug-tracker "https://github.com/mfiano/umbra/issues"
  :version "0.1.0"
  :encoding :utf-8
  :depends-on (#:golden-utils
               #:alexandria
               #:defpackage-plus
               #:shadow)
  :pathname "src"
  :serial t
  :components
  ((:module "common"
    :components
    ((:file "package")
     (:file "common")
     (:file "swizzle")
     (:file "vari")
     (:file "math")
     (:file "structs")))
   (:module "color"
    :components
    ((:file "package")
     (:file "grading")
     (:file "space")))
   (:module "graph"
    :components
    ((:file "package")
     (:file "graph")))
   (:module "shaping"
    :components
    ((:file "package")
     (:file "iq")
     (:file "levin")
     (:file "misc")
     (:file "penner")))
   (:module "hashing"
    :components
    ((:file "package")
     (:file "bbs")
     (:file "fast32")
     (:file "fast32-2")
     (:file "sgpp")))
   (:module "noise"
    :components
    ((:file "package")
     (:file "cellular")
     (:file "hermite")
     (:file "misc")
     (:file "perlin")
     (:file "polkadot")
     (:file "simplex")
     (:file "value")))
   (:module "sdf"
    :components
    ((:file "package")
     (:file "2d")))
   (:module "sprite"
    :components
    ((:file "package")
     (:file "sprite")
     (:file "tile-map")))))
