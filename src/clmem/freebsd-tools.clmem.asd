(asdf:defsystem #:freebsd-tools.clmem
  :description "A memory usage monitor for FreeBSD."
  :author ("Michael Fiano <mail@mfiano.net>")
  :license "BSD2"
  :homepage "https://github.com/mfiano/freebsd-tools"
  :version "0.1.0"
  :encoding :utf-8
  :depends-on (#:cffi
               #:cl-freebsd
               #:freebsd-tools.lib
               #:mfiano-utils)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "ui")
   (:file "report")
   (:file "main")))
