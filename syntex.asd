(asdf:defsystem #:syntex
  :description ""
  :author ("Michael Fiano <mail@mfiano.net>")
  :license "MIT"
  :homepage "https://mfiano.net/projects/syntex"
  :source-control (:git "https://git.mfiano.net/mfiano/syntex.git")
  :encoding :utf-8
  :depends-on (#:cl-cpus
               #:golden-utils
               #:lparallel
               #:pngload
               #:seedable-rng
               #:uiop
               #:zpng)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "conditions")
   (:file "common")
   (:file "image")
   (:module "synthesizers"
    :components
    ((:file "harrison")))))
