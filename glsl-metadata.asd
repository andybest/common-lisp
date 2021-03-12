(asdf:defsystem #:glsl-metadata
  :description ""
  :author ("Michael Fiano <mail@mfiano.net>")
  :license "MIT"
  :homepage "https://git.mfiano.net/mfiano/glsl-metadata"
  :encoding :utf-8
  :depends-on (#:golden-utils
               #:uiop)
  :pathname "src"
  :serial t
  :components
  ((:file "package")))
