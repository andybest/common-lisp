(asdf:defsystem #:patchwork
  :description "A spritesheet packer for games."
  :author "Michael Fiano <mail@mfiano.net>"
  :license "MIT"
  :homepage "https://git.mfiano.net/mfiano/patchwork"
  :encoding :utf-8
  :depends-on (#:binpack
               #:golden-utils
               #:opticl
               #:pngload
               #:uiop)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "packer")
   (:file "unpacker")
   (:file "slicer")))
