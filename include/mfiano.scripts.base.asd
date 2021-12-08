(asdf:defsystem #:mfiano.scripts.base
  :description "Helper library housing commonalities amoung all scripts."
  :author ("Michael Fiano <mail@mfiano.net>")
  :license "BSD2"
  :homepage "https://github.com/mfiano/lisp-scripts"
  :version "0.1.0"
  :encoding :utf-8
  :depends-on (#:adopt
               #:local-time
               #:mfiano-utils
               #:parse-float
               #:with-user-abort)
  :serial t
  :components
  ((:file "package")
   (:file "variables")
   (:file "utilities")
   (:file "options")
   (:file "common")))
