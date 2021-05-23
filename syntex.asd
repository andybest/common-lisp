(asdf:defsystem #:syntex
  :description ""
  :author ("Michael Fiano <mail@mfiano.net>")
  :license "MIT"
  :homepage "https://git.mfiano.net/mfiano/syntex"
  :encoding :utf-8
  :depends-on (#:cl-cpus
               #:golden-utils
               #:lparallel
               #:pngload
               #:seedable-rng
               #:uiop
               #:trivial-garbage
               #:zpng)
  :pathname "src"
  :serial t
  :components
  ((:module "common"
    :components
    ((:file "package")
     (:file "conditions")
     (:file "priority-queue")
     (:file "common")
     (:file "image")))
   (:module "harrison"
    :components
    ((:file "package")
     (:file "harrison")))
   (:module "wfc"
    :components
    ((:file "package")
     (:file "grid")
     (:file "kernel")
     (:file "core")
     (:file "sample")
     (:file "pattern")
     (:file "history")
     (:file "tile-map")
     (:file "solver")
     (:file "wfc")))
   (:file "package")))
