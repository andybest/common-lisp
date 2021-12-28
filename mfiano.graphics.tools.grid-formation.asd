(asdf:defsystem #:mfiano.graphics.tools.grid-formation
  :description "Simple cellular grid formations and algorithms."
  :author "Michael Fiano <mail@mfiano.net>"
  :license "MIT"
  :homepage "https://github.com/mfiano/common-lisp/tree/master/graphics/tools/grid-formation"
  :version "0.1.0"
  :encoding :utf-8
  :depends-on (#:mfiano.math.origin
               #:mfiano.misc.utils)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "grid")
   (:file "quad")
   (:file "quad-4-way")
   (:file "quad-8-way")
   (:file "hex")
   (:file "hex-rows")
   (:file "hex-columns")))
