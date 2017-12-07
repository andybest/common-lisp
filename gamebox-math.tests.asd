(in-package :cl-user)

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
   (:test-file "vec2")
   (:test-file "vec3")
   (:test-file "vec4")
   (:test-file "matrix")
   (:test-file "quaternion")
   (:test-file "dual-quaternion")))
