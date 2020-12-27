(asdf:defsystem #:shadow
  :description "A management system for OpenGL shader programs and associated buffer objects."
  :author "Michael Fiano <mail@mfiano.net>"
  :license "MIT"
  :homepage "https://mfiano.net/projects/shadow"
  :source-control (:git "https://git.mfiano.net/mfiano/shadow")
  :bug-tracker "https://git.mfiano.net/mfiano/shadow/issues"
  :encoding :utf-8
  :depends-on (#:cl-opengl
               #:glsl-packing
               #:golden-utils
               #:origin
               #:static-vectors
               #:varjo)
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
   (:file "buffers")
   (:file "glsl")))
