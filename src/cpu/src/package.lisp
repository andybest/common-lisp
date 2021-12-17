(in-package #:cl-user)

(defpackage #:freebsd-tools.cpu
  (:local-nicknames
   (#:lib #:freebsd-tools.lib)
   (#:bsd #:cl-freebsd)
   (#:c #:cffi)
   (#:u #:mfiano-utils)
   (#:ui #:adopt))
  (:use #:cl)
  (:export #:toplevel #:*ui*))
