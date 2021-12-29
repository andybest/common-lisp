(asdf:defsystem #:mfiano.graphics.tools.patchwork
  :description "A spritesheet packer for games."
  :author "Michael Fiano <mail@mfiano.net>"
  :license "MIT"
  :homepage "https://github.com/mfiano/common-lisp/tree/master/graphics/tools/patchwork"
  :encoding :utf-8
  :depends-on (#:binpack
               #:mfiano.file-formats.png
               #:mfiano.misc.utils
               #:opticl
               #:uiop)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "packer")
   (:file "unpacker")))
