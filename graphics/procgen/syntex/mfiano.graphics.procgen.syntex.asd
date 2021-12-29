(asdf:defsystem #:mfiano.graphics.procgen.syntex
  :description "Various texture synthesis algorithms."
  :author "Michael Fiano <mail@mfiano.net>"
  :license "MIT"
  :homepage "https://github.com/mfiano/common-lisp/tree/master/graphics/procgen/syntex"
  :encoding :utf-8
  :depends-on (#:cl-cpus
               #:lparallel
               #:mfiano.file-formats.png
               #:mfiano.misc.rng
               #:mfiano.misc.utils
               #:trivial-garbage
               #:uiop
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
    ((:file "core")
     (:file "grid")
     (:file "kernel")
     (:file "util")
     (:file "sample")
     (:file "pattern")
     (:file "tile-map")
     (:file "backtracker")
     (:file "solver")
     (:file "wfc")))))
