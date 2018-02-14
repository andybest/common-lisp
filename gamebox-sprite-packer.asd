(asdf:defsystem #:gamebox-sprite-packer
  :description "A spritesheet packer for games."
  :author "Michael Fiano <mail@michaelfiano.com>"
  :maintainer "Michael Fiano <mail@michaelfiano.com>"
  :license "MIT"
  :homepage "https://www.michaelfiano.com/projects/gamebox-sprite-packer"
  :source-control (:git "https://github.com/mfiano/sprite-packer.git")
  :bug-tracker "https://github.com/mfiano/gamebox-sprite-packer/issues"
  :version "1.0.5"
  :encoding :utf-8
  :long-description #.(uiop:read-file-string (uiop/pathname:subpathname *load-pathname* "README.md"))
  :depends-on (#:pngload
               #:opticl)
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "packer")
               (:file "unpacker")))
