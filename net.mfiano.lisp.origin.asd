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
  ((:file "package")
   (:file "internal")
   (:file "swizzle")
   (:file "constants")
   (:file "shaping")
   (:file "vec2")
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
   (:file "dquat")
   (:file "vec2-ops")
   (:file "vec3-ops")
   (:file "vec4-ops")
   (:file "dvec2-ops")
   (:file "dvec3-ops")
   (:file "dvec4-ops")
   (:file "mat2-ops")
   (:file "mat3-ops")
   (:file "mat4-ops")
   (:file "dmat2-ops")
   (:file "dmat3-ops")
   (:file "dmat4-ops")
   (:file "quat-ops")
   (:file "dquat-ops")
   (:file "general")))
