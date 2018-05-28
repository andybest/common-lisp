(asdf:defsystem #:umbra
  :description "A collection of general purpose reusable GPU shader functions."
  :author ("Michael Fiano <mail@michaelfiano.com>")
  :maintainer "Michael Fiano <mail@michaelfiano.com>"
  :license "MIT"
  :homepage "https://www.michaelfiano.com/projects/umbra"
  :source-control (:git "https://github.com/mfiano/umbra.git")
  :bug-tracker "https://github.com/mfiano/umbra/issues"
  :version "0.1.0"
  :encoding :utf-8
  :long-description #.(uiop:read-file-string (uiop/pathname:subpathname *load-pathname* "README.md"))
  :depends-on (#:gamebox-math.vari
               #:shadow
               #:defpackage-plus)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "common")
   (:file "math")
   (:file "graph")
   (:file "color-space")
   (:file "color-grading")
   (:file "shaping-penner")
   (:file "shaping-iq")
   (:file "shaping-levin")
   (:file "shaping-misc")
   (:file "hashing-bbs")
   (:file "hashing-sgpp")
   (:file "hashing-fast32")
   (:file "hashing-fast32-2")
   (:file "noise-simplex")
   (:file "noise-value")
   (:file "noise-perlin")
   (:file "noise-value-perlin")
   (:file "noise-cubist")
   (:file "noise-cellular")
   (:file "noise-polkadot")
   (:file "noise-stars")
   (:file "noise-hermite")
   (:file "noise-value-hermite")))
