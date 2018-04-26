(asdf:defsystem #:gamebox-math.tests
  :description "Tests for gamebox-math."
  :author "Michael Fiano <mail@michaelfiano.com>"
  :maintainer "Michael Fiano <mail@michaelfiano.com>"
  :license "MIT"
  :defsystem-depends-on (:prove-asdf)
  :depends-on (#:gamebox-math
               #:defpackage-plus
               #:prove)
  :pathname "tests"
  :serial t
  :components
  ( (:file "package")
    (:test-file "vec2i")
    (:test-file "vec2f")
    (:test-file "vec3i")
    (:test-file "vec3f")
    (:test-file "vec4i")
    (:test-file "vec4f")
    (:test-file "mat2")
    (:test-file "mat3")
    (:test-file "mat4")
    (:test-file "quat")
    (:test-file "dquat")))
