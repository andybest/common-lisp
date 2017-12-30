(in-package :cl-user)

(asdf:defsystem #:gamebox-math
  :description "A high performance math library useful for making games."
  :author "Michael Fiano <michael.fiano@gmail.com>"
  :maintainer "Michael Fiano <michael.fiano@gmail.com>"
  :license "MIT"
  :homepage "https://github.com/mfiano/gamebox-math"
  :bug-tracker "https://github.com/mfiano/gamebox-math/issues"
  :source-control (:git "git@github.com:mfiano/gamebox-math.git")
  :version "3.0.0"
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
   (:file "vec2-base")
   (:file "vec2-ops")
   (:file "vec3-base")
   (:file "vec3-ops")
   (:file "vec4-base")
   (:file "vec4-ops")
   (:file "mat4-base")
   (:file "mat4-ops")
   (:file "quat-base")
   (:file "quat-ops")
   (:file "dquat-base")
   (:file "dquat-ops")))
