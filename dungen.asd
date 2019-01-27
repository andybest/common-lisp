(asdf:defsystem #:dungen
  :description "A procedural game map generator."
  :author ("Michael Fiano <mail@michaelfiano.com>")
  :maintainer "Michael Fiano <mail@michaelfiano.com>"
  :license "MIT"
  :homepage "https://github.com/mfiano/dungen"
  :source-control (:git "https://github.com/mfiano/dungen.git")
  :bug-tracker "https://github.com/mfiano/dungen/issues"
  :version "0.1.0"
  :encoding :utf-8
  :long-description #.(uiop:read-file-string (uiop/pathname:subpathname *load-pathname* "README.md"))
  :depends-on (#:golden-utils
               #:cl-pcg
               #:graph)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "options")
   (:file "random")
   (:file "state")
   (:file "kernel")
   (:file "cell")
   (:file "region")
   (:file "room")
   (:file "junction")
   (:file "corridor")
   (:file "stage")))
