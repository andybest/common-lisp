(in-package #:cl-freebsd)

(c:define-foreign-library libc
  (:freebsd "/lib/libc.so.7"))

(c:use-foreign-library libc)
