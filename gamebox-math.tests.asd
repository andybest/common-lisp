(asdf:defsystem gamebox-math.tests
  :description "Tests for gamebox-math."
  :author "Michael Fiano <michael.fiano@gmail.com>"
  :maintainer "Michael Fiano <michael.fiano@gmail.com>"
  :license "MIT"
  :defsystem-depends-on (:prove-asdf)
  :depends-on (#:gamebox-math
               #:prove)
  :pathname "t"
  :serial t
  :components
  ((:file "package")
   (:test-file "vec3")))
