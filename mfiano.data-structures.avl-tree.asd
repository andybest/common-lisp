(asdf:defsystem #:mfiano.data-structures.avl-tree
  :description "An implementation of the AVL tree data structure."
  :author "Michael Fiano <mail@mfiano.net>"
  :license "MIT"
  :homepage "https://github.com/mfiano/common-lisp/tree/master/data-structures/avl-tree"
  :version "0.1.0"
  :encoding :utf-8
  :depends-on (#:mfiano.misc.utils)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "avl-tree")))
