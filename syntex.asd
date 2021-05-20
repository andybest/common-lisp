(asdf:defsystem #:syntex
  :description ""
  :author ("Michael Fiano <mail@mfiano.net>")
  :license "MIT"
  :homepage "https://git.mfiano.net/mfiano/syntex"
  :encoding :utf-8
  :depends-on (#:cl-cpus
               #:damn-fast-priority-queue
               #:golden-utils
               #:lparallel
               #:pngload
               #:seedable-rng
               #:uiop
               #:zpng)
  :pathname "src"
  :serial t
  :components
  ((:file "package-common")
   (:file "package-harrison")
   (:file "package-wfc")
   (:file "package")
   (:file "conditions")
   (:file "common")
   (:file "image")
   (:module "harrison"
    :components
    ((:file "harrison")))
   (:module "wfc"
    :components
    ((:file "grid")
     (:file "kernel")
     (:file "sample")
     (:file "pattern")
     (:file "core")
     (:file "adjacency")
     (:file "tile-map")
     (:file "solver")
     (:file "wfc")))))
