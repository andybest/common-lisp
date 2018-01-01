(in-package :cl-user)

(asdf:defsystem #:gamebox-math
  :description "A high performance math library useful for making games."
  :author "Michael Fiano <michael.fiano@gmail.com>"
  :maintainer "Michael Fiano <michael.fiano@gmail.com>"
  :license "MIT"
  :homepage "https://github.com/mfiano/gamebox-math"
  :bug-tracker "https://github.com/mfiano/gamebox-math/issues"
  :source-control (:git "git@github.com:mfiano/gamebox-math.git")
  :version "4.1.0"
  :encoding :utf-8
  :long-description #.(uiop:read-file-string
                       (uiop/pathname:subpathname *load-pathname* "README.md"))
  :depends-on (#:alexandria
               #:defstar)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "common")
   (:file "swizzle")
   (:file "vec2")
   (:file "vec3")
   (:file "vec4")
   (:file "mat4")
   (:file "quat")
   (:file "dquat")))
