(asdf:defsystem #:mfiano.math.cubic-bezier
  :description "A library for constructing and evaluating cubic Bézier curve paths."
  :author "Michael Fiano <mail@mfiano.net>"
  :license "MIT"
  :homepage "https://github.com/mfiano/common-lisp/tree/master/math/cubic-bezier"
  :version "0.1.0"
  :encoding :utf-8
  :depends-on (#:mfiano.math.origin
               #:mfiano.misc.utils)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "cubic-bezier")))
