(asdf:defsystem #:mfiano.ffi.freebsd
  :description "A small, but growing collection of syscall and base install library FFI wrappers for FreeBSD."
  :author "Michael Fiano <mail@mfiano.net>"
  :license "BSD2"
  :homepage "https://github.com/mfiano/common-lisp/tree/master/ffi/freebsd"
  :version "0.1.0"
  :encoding :utf-8
  :depends-on (#:cffi
               #:mfiano.misc.utils)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "ffi-libraries")
   (:file "ffi-variables")
   (:file "ffi-enums")
   (:file "ffi-structures")
   (:file "ffi-functions")
   (:file "utilities")
   (:file "conditions")
   (:file "wrappers")
   (:file "macros")))
