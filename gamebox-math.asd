(asdf:defsystem #:gamebox-math
  :description "A high performance math library useful for making games."
  :author "Michael Fiano <mail@michaelfiano.com>"
  :maintainer "Michael Fiano <mail@michaelfiano.com>"
  :license "MIT"
  :homepage "https://github.com/mfiano/gamebox-math"
  :source-control (:git "git@github.com:mfiano/gamebox-math.git")
  :bug-tracker "https://github.com/mfiano/gamebox-math/issues"
  :version "7.0.0"
  :encoding :utf-8
  :depends-on (#:golden-utils)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "common")
   (:file "vec2")
   (:file "vec3")
   (:file "vec4")
   (:file "mat2")
   (:file "mat3")
   (:file "mat4")
   (:file "quat")))
