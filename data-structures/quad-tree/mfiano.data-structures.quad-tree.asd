(asdf:defsystem #:mfiano.data-structures.quad-tree
  :description "An implementation of the quad tree data structure."
  :author "Michael Fiano <mail@mfiano.net>"
  :license "MIT"
  :homepage "https://github.com/mfiano/common-lisp/tree/master/data-structures/quad-tree"
  :version "0.1.0"
  :encoding :utf-8
  :depends-on (#:mfiano.math.origin
               #:mfiano.misc.utils)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "quad-tree")))
