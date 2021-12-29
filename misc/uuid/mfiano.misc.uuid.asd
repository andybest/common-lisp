(asdf:defsystem #:mfiano.misc.uuid
  :description "Create and parse RFC-4122 UUID version 4 identifiers."
  :author "Michael Fiano <mail@mfiano.net>"
  :license "MIT"
  :homepage "https://github.com/mfiano/common-lisp/tree/master/misc/uuid"
  :encoding :utf-8
  :depends-on (#:mfiano.misc.rng
               #:mfiano.misc.utils)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "conditions")
   (:file "uuid")))
