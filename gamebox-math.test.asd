(asdf:defsystem #:gamebox-math.test
  :description "Tests for gamebox-math."
  :author "Michael Fiano <mail@michaelfiano.com>"
  :maintainer "Michael Fiano <mail@michaelfiano.com>"
  :license "MIT"
  :defsystem-depends-on (:prove-asdf)
  :depends-on (#:gamebox-math
               #:prove)
  :pathname "test"
  :serial t
  :components
  ((:file "package")
   (:test-file "vec2")
   (:test-file "vec3")
   (:test-file "vec4")
   (:test-file "mat2")
   (:test-file "mat3")
   (:test-file "mat4")
   (:test-file "quat")))
