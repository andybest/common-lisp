(asdf:defsystem #:gamebox-math
  :description "A high performance math library useful for making games."
  :author "Michael Fiano <mail@michaelfiano.com>"
  :maintainer "Michael Fiano <mail@michaelfiano.com>"
  :license "MIT"
  :homepage "https://github.com/mfiano/gamebox-math"
  :source-control (:git "git@github.com:mfiano/gamebox-math.git")
  :bug-tracker "https://github.com/mfiano/gamebox-math/issues"
  :version "6.2.0"
  :encoding :utf-8
  :long-description #.(uiop:read-file-string (uiop/pathname:subpathname *load-pathname* "README.md"))
  :depends-on (#:golden-utils
               #:defpackage-plus)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "common")
   (:file "vec2i")
   (:file "vec2f")
   (:file "vec3i")
   (:file "vec3f")
   (:file "vec4i")
   (:file "vec4f")
   (:file "mat2")
   (:file "mat3")
   (:file "mat4")
   (:file "quat")
   (:file "dquat")))
