(asdf:defsystem #:net.mfiano.lisp.dungen
  :description "A procedural game map generator."
  :author ("Michael Fiano <mail@mfiano.net>")
  :license "MIT"
  :homepage "https://mfiano.net/projects/dungen"
  :source-control (:git "https://github.com/mfiano/dungen")
  :bug-tracker "https://github.com/mfiano/dungen/issues"
  :encoding :utf-8
  :depends-on (#:cl-pcg
               #:graph
               #:net.mfiano.lisp.golden-utils)
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
