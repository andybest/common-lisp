(asdf:defsystem #:origin
  :description "A native Lisp graphics math library with an emphasis on performance and correctness."
  :author "Michael Fiano <mail@michaelfiano.com>"
  :maintainer "Michael Fiano <mail@michaelfiano.com>"
  :license "MIT"
  :homepage "https://github.com/mfiano/origin"
  :source-control (:git "git@github.com:mfiano/origin.git")
  :bug-tracker "https://github.com/mfiano/origin/issues"
  :encoding :utf-8
  :depends-on (#:golden-utils
               #:alexandria
               #:specialization-store)
  :pathname "src"
  :serial t
  :in-order-to ((asdf:test-op (asdf:test-op #:origin.test)))
  :components
  ((:file "package")
   (:file "internal")
   (:file "common")
   (:file "swizzle")
   (:file "math-constants")
   (:file "shaping")
   (:file "vec2")
   (:file "vec3")
   (:file "vec4")
   (:file "mat2")
   (:file "mat3")
   (:file "mat4")
   (:file "quat")
   (:file "vec2-ops")
   (:file "vec3-ops")
   (:file "vec4-ops")
   (:file "mat2-ops")
   (:file "mat3-ops")
   (:file "mat4-ops")
   (:file "quat-ops")
   (:file "general")
   (:file "physics")))
