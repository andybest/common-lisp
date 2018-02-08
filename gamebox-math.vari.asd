(asdf:defsystem #:gamebox-math.vari
  :description "A compatibility wrapper to make varjo work with gamebox-math."
  :author "Michael Fiano <mail@michaelfiano.com>"
  :maintainer "Michael Fiano <mail@michaelfiano.com>"
  :license "MIT"
  :depends-on (#:gamebox-math
               #:varjo
               #:defpackage-plus)
  :pathname "contrib"
  :serial t
  :components
  ((:file "vari")))
