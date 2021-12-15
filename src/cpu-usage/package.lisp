(in-package #:cl-user)

(defpackage #:mfiano.scripts.cpu-usage
  (:local-nicknames
   (#:b #:mfiano.scripts.base)
   (#:bsd #:cl-freebsd)
   (#:c #:cffi)
   (#:u #:mfiano-utils)
   (#:ui #:adopt))
  (:use #:cl)
  (:export #:toplevel #:*ui*))
