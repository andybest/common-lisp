(asdf:defsystem #:mfiano.cmd.freebsd.clcpu
  :description "A CPU usage monitor for FreeBSD."
  :author "Michael Fiano <mail@mfiano.net>"
  :license "BSD2"
  :homepage "https://github.com/mfiano/common-lisp/tree/master/cmd/freebsd"
  :version "0.1.0"
  :encoding :utf-8
  :depends-on (#:cffi
               #:mfiano.cmd.freebsd.lib
               #:mfiano.ffi.freebsd
               #:mfiano.misc.utils)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "util")
   (:file "ui")
   (:file "report")
   (:file "main")))
