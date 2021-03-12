(asdf:defsystem #:dungen
  :description "A procedural game map generator."
  :author ("Michael Fiano <mail@mfiano.net>")
  :license "MIT"
  :homepage "https://git.mfiano.net/mfiano/dungen"
  :encoding :utf-8
  :depends-on (#:cl-pcg
               #:golden-utils
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
