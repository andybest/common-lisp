(asdf:defsystem #:mfiano.data-structures.dynamic-array
  :description "An optimized 1-dimensional array of fixnums that automatically re-adjusts in size."
  :author "Michael Fiano <mail@mfiano.net>"
  :license "MIT"
  :homepage "https://github.com/mfiano/common-lisp/tree/master/data-structures/dynamic-array"
  :version "0.1.0"
  :encoding :utf-8
  :depends-on (#:mfiano.misc.utils)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "dynamic-array")))
