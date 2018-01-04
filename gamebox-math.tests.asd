(asdf:defsystem #:gamebox-math.tests
  :description "Tests for gamebox-math."
  :author "Michael Fiano <michael.fiano@gmail.com>"
  :maintainer "Michael Fiano <michael.fiano@gmail.com>"
  :license "MIT"
  :defsystem-depends-on (:prove-asdf)
  :depends-on (#:gamebox-math
               #:defpackage-plus
               #:prove)
  :pathname "t"
  :serial t
  :components
  ( (:file "package")
   (:test-file "vector2")
   (:test-file "vector3")
   (:test-file "vector4")
   (:test-file "matrix2")
   (:test-file "matrix3")
   (:test-file "matrix4")
   (:test-file "quaternion")
   (:test-file "dual-quaternion")))
