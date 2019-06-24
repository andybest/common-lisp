(asdf:defsystem #:dungen
  :description "A procedural game map generator."
  :author ("Michael Fiano <mail@michaelfiano.com>")
  :maintainer "Michael Fiano <mail@michaelfiano.com>"
  :license "MIT"
  :homepage "https://github.com/mfiano/dungen"
  :source-control (:git "https://github.com/mfiano/dungen.git")
  :bug-tracker "https://github.com/mfiano/dungen/issues"
  :encoding :utf-8
  :depends-on (#:golden-utils
               #:alexandria
               #:cl-pcg
               #:graph)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "random")
   (:file "state")
   (:file "stage")
   (:file "cell")
   (:file "kernel")
   (:file "region")
   (:file "room")
   (:file "junction")
   (:file "corridor")
   (:file "test")))
