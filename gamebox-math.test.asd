(asdf:defsystem #:gamebox-math.test
  :description "Tests for gamebox-math."
  :author "Michael Fiano <mail@michaelfiano.com>"
  :maintainer "Michael Fiano <mail@michaelfiano.com>"
  :license "MIT"
  :depends-on (#:gamebox-math
               #:parachute)
  :pathname "test"
  :serial t
  :perform (asdf:test-op (o c) (uiop:symbol-call
                                '#:parachute '#:test '#:box.math.test))
  :components
  ((:file "package")
   (:file "vec2")
   (:file "vec3")
   (:file "vec4")
   (:file "mat2")
   (:file "mat3")
   (:file "mat4")
   (:file "quat")))
