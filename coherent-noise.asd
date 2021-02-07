(asdf:defsystem #:coherent-noise
  :description ""
  :author ("Michael Fiano <mail@mfiano.net>")
  :license "MIT"
  :homepage "https://mfiano.net/projects/coherent-noise"
  :source-control (:git "https://github.com/mfiano/coherent-noise.git")
  :encoding :utf-8
  :depends-on (#:cl-cpus
               #:lparallel
               #:golden-utils
               #:seedable-rng
               #:zpng
               #:uiop)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "conditions")
   (:file "common")
   (:file "modifiers")
   (:module "generators"
    :components
    ((:file "perlin")
     (:file "simplex")
     (:file "open-simplex")
     (:file "open-simplex2-fast")
     (:file "value")
     (:file "cellular")
     (:file "misc")))
   (:file "samplers")
   (:file "noise")))
