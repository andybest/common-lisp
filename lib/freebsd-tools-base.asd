(asdf:defsystem #:freebsd-tools-base
  :description "Helper library housing commonalities among all freebsd-tools applications."
  :author ("Michael Fiano <mail@mfiano.net>")
  :license "BSD2"
  :homepage "https://github.com/mfiano/freebsd-tools"
  :version "0.1.0"
  :encoding :utf-8
  :depends-on (#:adopt
               #:local-time
               #:mfiano-utils
               #:parse-float
               #:with-user-abort)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "variables")
   (:file "utilities")
   (:file "options")
   (:file "common")))
