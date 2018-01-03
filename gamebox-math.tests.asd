(asdf:defsystem gamebox-math.tests
  :description "Tests for gamebox-math."
  :author "Michael Fiano <michael.fiano@gmail.com>"
  :maintainer "Michael Fiano <michael.fiano@gmail.com>"
  :license "MIT"
  :defsystem-depends-on (:prove-asdf)
  :depends-on (#:gamebox-math
               #:defpackage-plus
               #:prove)
  :serial t
  :components
  ((:file "src/package-local-nicknames-compat")
   (:file "t/package")
   (:test-file "t/vec2")
   (:test-file "t/vec3")
   (:test-file "t/vec4")
   (:test-file "t/mat2")
   (:test-file "t/mat3")
   (:test-file "t/mat4")
   (:test-file "t/quat")
   (:test-file "t/dquat")))
