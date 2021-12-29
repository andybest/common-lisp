(asdf:defsystem #:mfiano.file-formats.png.test
  :description "Tests for mfiano.file-formats.png."
  :author "Michael Fiano <mail@mfiano.net>"
  :license "MIT"
  :homepage "https://github.com/mfiano/common-lisp/tree/master/file-formats/png"
  :encoding :utf-8
  :depends-on (#:alexandria
               #:local-time
               #:mfiano.file-formats.png
               #:opticl
               #:png-read
               #:uiop)
  :pathname "src"
  :serial t
  :components
  ((:file "test")))
