(asdf:defsystem #:glsl-metadata
  :description ""
  :author ("Michael Fiano <mail@mfiano.net>")
  :license "MIT"
  :homepage "https://mfiano.net/projects/glsl-metadata"
  :source-control (:git "https://github.com/mfiano/glsl-metadata.git")
  :bug-tracker "https://github.com/mfiano/glsl-metadata/issues"
  :encoding :utf-8
  :depends-on (#:golden-utils
               #:uiop)
  :pathname "src"
  :serial t
  :components
  ((:file "package")))
