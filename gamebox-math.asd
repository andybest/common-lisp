(in-package :cl-user)

(asdf:defsystem #:gamebox-math
  :description "A high performance math library useful for making games."
  :author "Michael Fiano <michael.fiano@gmail.com>"
  :maintainer "Michael Fiano <michael.fiano@gmail.com>"
  :license "MIT"
  :homepage "https://github.com/mfiano/gamebox-math"
  :bug-tracker "https://github.com/mfiano/gamebox-math/issues"
  :source-control (:git "git@github.com:mfiano/gamebox-math.git")
  :version "1.0.2"
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
   (:file "vector-base")
   (:file "vector-ops")
   (:file "matrix-base")
   (:file "matrix-ops")
   (:file "quaternion-base")
   (:file "quaternion-ops")
   (:file "dual-quaternion-base")
   (:file "dual-quaternion-ops")
   (:file "math")))
