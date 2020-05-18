(asdf:defsystem #:net.mfiano.lisp.shadow
  :description "A management system for OpenGL shader programs and associated buffer objects."
  :author "Michael Fiano <mail@mfiano.net>"
  :license "MIT"
  :homepage "https://mfiano.net/projects/shadow"
  :source-control (:git "https://github.com/mfiano/shadow")
  :bug-tracker "https://github.com/mfiano/shadow/issues"
  :encoding :utf-8
  :depends-on (#:cl-opengl
               #:glsl-packing
               #:net.mfiano.lisp.golden-utils
               #:net.mfiano.lisp.origin
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
