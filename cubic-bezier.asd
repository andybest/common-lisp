(asdf:defsystem #:cubic-bezier
  :description "A library for constructing and evaluating cubic Bézier curve paths."
  :author ("Michael Fiano <mail@mfiano.net>")
  :license "MIT"
  :homepage "https://mfiano.net/projects/cubic-bezier"
  :source-control (:git "https://github.com/mfiano/cubic-bezier.git")
  :encoding :utf-8
  :depends-on (#:golden-utils
               #:origin)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "cubic-bezier")))
