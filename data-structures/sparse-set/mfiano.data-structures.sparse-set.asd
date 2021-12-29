(asdf:defsystem #:mfiano.data-structures.sparse-set
  :description "An implementation of the sparse set data structure."
  :author "Michael Fiano <mail@mfiano.net>"
  :license "MIT"
  :homepage "https://github.com/mfiano/common-lisp/tree/master/data-structures/sparse-set"
  :version "0.1.0"
  :encoding :utf-8
  :depends-on (#:mfiano.misc.utils)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "sparse-set")))
