(asdf:defsystem #:umbra
  :description "A collection of general purpose reusable GPU shader functions."
  :author ("Michael Fiano <mail@michaelfiano.com>")
  :maintainer "Michael Fiano <mail@michaelfiano.com>"
  :license "MIT"
  :homepage "https://www.michaelfiano.com/projects/umbra"
  :source-control (:git "https://github.com/mfiano/umbra.git")
  :bug-tracker "https://github.com/mfiano/umbra/issues"
  :version "0.1.0"
  :encoding :utf-8
  :long-description #.(uiop:read-file-string (uiop/pathname:subpathname *load-pathname* "README.md"))
  :depends-on (#:gamebox-math.vari
               #:shadow
               #:defpackage-plus)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "math")
   (:file "color-space")
   (:file "color-grading")
   (:file "easing")
   (:file "graph")))
