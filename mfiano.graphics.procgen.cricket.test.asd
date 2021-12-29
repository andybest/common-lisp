(asdf:defsystem #:mfiano.graphics.procgen.cricket.test
  :description "Tests for mfiano.graphics.procgen.cricket."
  :author "Michael Fiano <mail@mfiano.net>"
  :license "MIT"
  :homepage "https://github.com/mfiano/common-lisp/tree/master/graphics/procgen/cricket"
  :depends-on (#:mfiano.file-formats.png
               #:mfiano.graphics.procgen.cricket
               #:mfiano.misc.utils
               #:prove)
  :defsystem-depends-on (#:prove-asdf)
  :pathname "test"
  :serial t
  :perform (asdf:test-op (op c)
                         (uiop:symbol-call '#:mfiano.graphics.procgen.cricket.test '#:run-tests c))
  :components
  ((:file "package")
   (:file "common")
   (:test-file "test")))
