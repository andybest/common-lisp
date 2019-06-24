(asdf:defsystem #:shadow
  :description "A lightweight system to help with defining and managing OpenGL shader programs."
  :author "Michael Fiano <mail@michaelfiano.com>"
  :maintainer "Michael Fiano <mail@michaelfiano.com>"
  :license "MIT"
  :homepage "https://www.michaelfiano.com/projects/shadow"
  :source-control (:git "git@github.com:mfiano/shadow.git")
  :bug-tracker "https://github.com/mfiano/shadow/issues"
  :encoding :utf-8
  :depends-on (#:static-vectors
               #:glsl-packing
               #:varjo
               #:cl-opengl
               #:golden-utils
               #:alexandria
               #:origin)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "shadow")
   (:file "common")
   (:file "functions")
   (:file "stages")
   (:file "program")
   (:file "packing")
   (:file "attributes")
   (:file "uniforms")
   (:file "layout")
   (:file "blocks")
   (:file "buffers")))
