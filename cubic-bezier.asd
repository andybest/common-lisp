(asdf:defsystem #:cubic-bezier
  :description "A library for constructing and evaluating cubic Bézier curve paths."
  :author ("Michael Fiano <mail@mfiano.net>")
  :license "MIT"
  :homepage "https://git.mfiano.net/mfiano/cubic-bezier"
  :encoding :utf-8
  :depends-on (#:golden-utils
               #:origin)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "cubic-bezier")))
