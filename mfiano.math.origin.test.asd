(asdf:defsystem #:mfiano.math.origin.test
  :description "Tests for origin."
  :author "Michael Fiano <mail@mfiano.net>"
  :license "MIT"
  :homepage "https://github.com/mfiano/common-lisp/tree/master/math/origin"
  :depends-on (#:mfiano.math.origin
               #:parachute)
  :pathname "test"
  :serial t
  :perform (asdf:test-op (o c) (uiop:symbol-call '#:parachute '#:test '#:mfiano.math.origin.test))
  :components
  ((:file "package")
   (:file "vec2")
   (:file "vec3")
   (:file "vec4")
   (:file "mat2")
   (:file "mat3")
   (:file "mat4")
   (:file "quat")))
