(asdf:defsystem #:mfiano.misc.utils
  :description "A utility library."
  :author "Michael Fiano <mail@mfiano.net>"
  :license "MIT"
  :homepage "https://github.com/mfiano/common-lisp/tree/master/misc/utils"
  :version "0.1.0"
  :encoding :utf-8
  :depends-on (#:alexandria
               #:serapeum
               #:uiop)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "closure")
   (:file "type")
   (:file "symbol")
   (:file "number")
   (:file "character")
   (:file "array")
   (:file "string")
   (:file "sequence")
   (:file "list")
   (:file "list-alist")
   (:file "list-plist")
   (:file "hash-table")
   (:file "macro")
   (:file "filesystem")
   (:file "math")
   (:file "misc")))
