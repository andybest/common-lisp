(asdf:defsystem #:umbra
  :description "A library of reusable GPU shader functions."
  :author ("Michael Fiano <mail@michaelfiano.com>")
  :maintainer "Michael Fiano <mail@michaelfiano.com>"
  :license "MIT"
  :homepage "https://www.michaelfiano.com/projects/umbra"
  :source-control (:git "https://github.com/mfiano/umbra.git")
  :bug-tracker "https://github.com/mfiano/umbra/issues"
  :encoding :utf-8
  :depends-on (#:alexandria
               #:shadow
               #:varjo)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "common")
   (:file "common-swizzle")
   (:file "common-vari")
   (:file "common-math")
   (:file "common-structs")
   (:file "color-grading")
   (:file "color-space")
   (:file "graphing")
   (:file "shaping-iq")
   (:file "shaping-levin")
   (:file "shaping-penner")
   (:file "shaping-misc")
   (:file "hashing-bbs")
   (:file "hashing-fast32")
   (:file "hashing-fast32-2")
   (:file "hashing-sgpp")
   (:file "noise-cellular")
   (:file "noise-hermite")
   (:file "noise-perlin")
   (:file "noise-polkadot")
   (:file "noise-simplex")
   (:file "noise-value")
   (:file "noise-misc")
   (:file "sdf-2d")
   (:file "sprite")
   (:file "effects-window-rain")))
