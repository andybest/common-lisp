(asdf:defsystem #:mfiano.scripts.cpu-usage
  :description ""
  :author ("Michael Fiano <mail@mfiano.net>")
  :license "BSD2"
  :homepage "https://github.com/mfiano/lisp-scripts"
  :version "0.1.0"
  :encoding :utf-8
  :depends-on (#:cffi
               #:mfiano.scripts.base
               #:mfiano-utils)
  :serial t
  :components
  ((:file "package")
   (:file "ui")
   (:file "sysctl")
   (:file "cpu-usage")))
