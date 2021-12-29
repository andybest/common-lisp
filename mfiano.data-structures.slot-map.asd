(asdf:defsystem #:mfiano.data-structures.slot-map
  :description "An implementation of the slot-map data structure."
  :author "Michael Fiano <mail@mfiano.net>"
  :license "MIT"
  :homepage "https://github.com/mfiano/common-lisp/tree/master/data-structures/slot-map"
  :version "0.1.0"
  :encoding :utf-8
  :depends-on (#:mfiano.data-structures.dynamic-array
               #:mfiano.misc.utils)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "slot-map")))
