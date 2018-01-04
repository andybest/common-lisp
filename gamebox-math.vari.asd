(asdf:defsystem #:gamebox-math.vari
  :description "A compatibility wrapper to make varjo work with gamebox-math."
  :author "Michael Fiano <michael.fiano@gmail.com>"
  :maintainer "Michael Fiano <michael.fiano@gmail.com>"
  :license "MIT"
  :depends-on (#:gamebox-math
               #:varjo
               #:defpackage-plus)
  :pathname "contrib"
  :serial t
  :components
  ((:file "vari")))
