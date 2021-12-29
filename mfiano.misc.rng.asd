(asdf:defsystem #:mfiano.misc.rng
  :description "A seedable random number generator."
  :author "Michael Fiano <mail@mfiano.net>"
  :license "MIT"
  :homepage "https://github.com/mfiano/common-lisp/tree/master/misc/rng"
  :version "0.1.0"
  :encoding :utf-8
  :depends-on (#:cl-pcg
               #:ironclad
               #:mfiano.misc.utils)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "dictionary")
   (:file "generator")
   (:file "conditions")))
