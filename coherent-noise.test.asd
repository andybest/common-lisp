(asdf:defsystem #:coherent-noise.test
  :description "Tests for coherent-noise"
  :author "Michael Fiano <mail@mfiano.net>"
  :license "MIT"
  :depends-on (#:coherent-noise
               #:golden-utils
               #:pngload
               #:prove)
  :defsystem-depends-on (#:prove-asdf)
  :pathname "test"
  :serial t
  :perform (asdf:test-op (op c) (uiop:symbol-call '#:coherent-noise.test '#:run-tests c))
  :components
  ((:file "package")
   (:file "common")
   (:test-file "test")))
