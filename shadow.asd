(asdf:defsystem #:shadow
  :description "A simple wrapper around varjo for writing OpenGL shader programs."
  :author "Michael Fiano <michael.fiano@gmail.com>"
  :maintainer "Michael Fiano <michael.fiano@gmail.com>"
  :license "MIT"
  :homepage "https://github.com/mfiano/shadow"
  :bug-tracker "https://github.com/mfiano/shadow/issues"
  :source-control (:git "git@github.com:mfiano/shadow.git")
  :version "1.0.0"
  :encoding :utf-8
  :long-description #.(uiop:read-file-string (uiop/pathname:subpathname *load-pathname* "README.md"))
  :depends-on (#:alexandria
               #:defpackage-plus
               #:varjo
               #:cl-opengl)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "shadow")))
