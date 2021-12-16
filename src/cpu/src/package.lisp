(in-package #:cl-user)

(defpackage #:freebsd-tools.cpu
  (:local-nicknames
   (#:b #:freebsd-tools-base)
   (#:bsd #:cl-freebsd)
   (#:c #:cffi)
   (#:u #:mfiano-utils)
   (#:ui #:adopt))
  (:use #:cl)
  (:export #:toplevel #:*ui*))
