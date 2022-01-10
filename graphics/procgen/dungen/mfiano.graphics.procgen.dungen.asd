(asdf:defsystem #:mfiano.graphics.procgen.dungen
  :description "A procedural dungeon generator."
  :author "Michael Fiano <mail@mfiano.net>"
  :license "MIT"
  :homepage "https://github.com/mfiano/common-lisp/tree/master/graphics/procgen/dungen"
  :encoding :utf-8
  :depends-on (#:graph
               #:mfiano.misc.rng
               #:mfiano.misc.utils)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "state")
   (:file "stage")
   (:file "cell")
   (:file "kernel")
   (:file "region")
   (:file "room")
   (:file "junction")
   (:file "corridor")
   (:file "test")))
