(asdf:defsystem #:mfiano.cmd.freebsd.lib
  :description "A helper library housing commonalities among all FreeBSD command line applications."
  :author "Michael Fiano <mail@mfiano.net>"
  :license "BSD2"
  :homepage "https://github.com/mfiano/common-lisp/tree/master/cmd/freebsd"
  :version "0.1.0"
  :encoding :utf-8
  :depends-on (#:adopt
               #:cffi
               #:local-time
               #:mfiano.ffi.freebsd
               #:mfiano.misc.utils
               #:parse-float
               #:with-user-abort)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "variables")
   (:file "utilities")
   (:file "options")
   (:file "common")
   (:file "terminal")
   (:file "progress-bar")))
