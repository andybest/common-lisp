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
  ((:module "common"
    :components
    ((:file "package")
     (:file "util")
     (:file "constants")))
   (:module "types"
    :components
    ((:file "vec2")
     (:file "vec3")
     (:file "vec4")
     (:file "dvec2")
     (:file "dvec3")
     (:file "dvec4")
     (:file "mat2")
     (:file "mat3")
     (:file "mat4")
     (:file "dmat2")
     (:file "dmat3")
     (:file "dmat4")
     (:file "quat")
     (:file "dquat")))
   (:module "operations"
    :components
    ((:file "vec2")
     (:file "vec3")
     (:file "vec4")
     (:file "dvec2")
     (:file "dvec3")
     (:file "dvec4")
     (:file "mat2")
     (:file "mat3")
     (:file "mat4")
     (:file "dmat2")
     (:file "dmat3")
     (:file "dmat4")
     (:file "quat")
     (:file "dquat")))
   (:module "primitives"
    :components
    ((:file "point2d")
     (:file "point3d")
     (:file "line2d")
     (:file "line3d")
     (:file "circle")
     (:file "box2d")
     (:file "oriented-box-2d")))
   (:module "primitive-tests"
    :components
    ((:file "2d")))
   (:module "general"
    :components
    ((:file "package")
     (:file "shaping")))))
