(asdf:defsystem #:shadow
  :description "A management system for OpenGL shader programs and associated buffer objects."
  :author "Michael Fiano <mail@mfiano.net>"
  :license "MIT"
  :homepage "https://git.mfiano.net/mfiano/shadow"
  :encoding :utf-8
  :depends-on (#:cffi
               #:cl-opengl
               #:glsl-packing
               #:golden-utils
               #:origin
               #:split-sequence
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
