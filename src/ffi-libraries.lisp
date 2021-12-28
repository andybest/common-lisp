(in-package #:mfiano.ffi.freebsd)

(c:define-foreign-library libc
  (:freebsd "libc.so.7"))

(c:use-foreign-library libc)
