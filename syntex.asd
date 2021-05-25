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
  ((:file "package")
   (:module "common"
    :components
    ((:file "conditions")
     (:file "priority-queue")
     (:file "common")
     (:file "image")))
   (:module "harrison"
    :components
    ((:file "harrison")))
   (:module "wfc"
    :components
    ((:file "util")
     (:file "core")
     (:file "grid")
     (:file "kernel")
     (:file "sample")
     (:file "pattern")
     (:file "history")
     (:file "tile-map")
     (:file "solver")
     (:file "wfc")))))
