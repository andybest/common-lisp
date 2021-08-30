(asdf:defsystem #:gfxmath.test
  :description "Unit tests for gfxmath."
  :author "Michael Fiano <mail@mfiano.net>"
  :license "MIT"
  :homepage "https://github.com/mfiano/gfxmath"
  :depends-on (#:gfxmath
               #:mfiano-utils
               #:prove)
  :defsystem-depends-on (#:prove-asdf)
  :pathname "test"
  :serial t
  :perform (asdf:test-op (op c) (uiop:symbol-call '#:gfxmath.test '#:run-tests c))
  :components
  ((:file "package")
   (:file "common")
   (:test-file "vector")
   (:test-file "matrix")
   (:test-file "quaternion")))
