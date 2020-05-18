(asdf:defsystem #:net.mfiano.lisp.patchwork
  :description "A spritesheet packer for games."
  :author "Michael Fiano <mail@mfiano.net>"
  :license "MIT"
  :homepage "https://mfiano.net/projects/patchwork"
  :source-control (:git "https://github.com/mfiano/patchwork")
  :bug-tracker "https://github.com/mfiano/patchwork/issues"
  :encoding :utf-8
  :depends-on (#:binpack
               #:opticl
               #:pngload
               #:uiop)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "packer")
   (:file "unpacker")))
