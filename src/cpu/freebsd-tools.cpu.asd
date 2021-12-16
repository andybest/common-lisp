(asdf:defsystem #:freebsd-tools.cpu
  :description "A CPU usage monitor for FreeBSD."
  :author ("Michael Fiano <mail@mfiano.net>")
  :license "BSD2"
  :homepage "https://github.com/mfiano/freebsd-tools"
  :version "0.1.0"
  :encoding :utf-8
  :depends-on (#:cffi
               #:cl-freebsd
               #:freebsd-tools-base
               #:mfiano-utils)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "ffi")
   (:file "util")
   (:file "ui")
   (:file "report")
   (:file "main")))
