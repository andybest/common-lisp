(in-package #:cl-user)

(defpackage #:freebsd-tools.clmem
  (:local-nicknames
   (#:lib #:freebsd-tools.lib)
   (#:bsd #:cl-freebsd)
   (#:c #:cffi)
   (#:u #:mfiano-utils)
   (#:ui #:adopt))
  (:use #:cl)
  (:export #:app #:*ui*))
