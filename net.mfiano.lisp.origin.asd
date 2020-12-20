(asdf:defsystem #:net.mfiano.lisp.origin
  :description "A native Lisp graphics math library with an emphasis on performance and correctness."
  :author "Michael Fiano <mail@mfiano.net>"
  :license "MIT"
  :homepage "https://mfiano.net/projects/origin"
  :source-control (:git "https://github.com/mfiano/origin")
  :bug-tracker "https://github.com/mfiano/origin/issues"
  :encoding :utf-8
  :depends-on (#:net.mfiano.lisp.golden-utils
               #:specialization-store)
  :pathname "src"
  :serial t
  :in-order-to ((asdf:test-op (asdf:test-op #:net.mfiano.lisp.origin.test)))
  :components
  ((:file "common/package")
   (:file "common/util")
   (:file "common/constants")
   (:file "types/vec2")
   (:file "types/vec3")
   (:file "types/vec4")
   (:file "types/dvec2")
   (:file "types/dvec3")
   (:file "types/dvec4")
   (:file "types/mat2")
   (:file "types/mat3")
   (:file "types/mat4")
   (:file "types/dmat2")
   (:file "types/dmat3")
   (:file "types/dmat4")
   (:file "types/quat")
   (:file "types/dquat")
   (:file "operations/vec2")
   (:file "operations/vec3")
   (:file "operations/vec4")
   (:file "operations/dvec2")
   (:file "operations/dvec3")
   (:file "operations/dvec4")
   (:file "operations/mat2")
   (:file "operations/mat3")
   (:file "operations/mat4")
   (:file "operations/dmat2")
   (:file "operations/dmat3")
   (:file "operations/dmat4")
   (:file "operations/quat")
   (:file "operations/dquat")
   (:file "shaping/shaping")
   (:file "geometry/primitives/point2d")
   (:file "geometry/primitives/point3d")
   (:file "geometry/primitives/line2d")
   (:file "geometry/primitives/line3d")
   (:file "geometry/primitives/circle")
   (:file "geometry/primitives/box2d")
   (:file "geometry/primitives/oriented-box-2d")
   (:file "geometry/tests/2d")))
