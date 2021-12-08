(in-package #:cl-user)

(defpackage #:mfiano.scripts.cpu-usage
  (:local-nicknames
   (#:base #:mfiano.scripts.base)
   (#:u #:mfiano-utils)
   (#:ui #:adopt))
  (:use #:cl)
  (:export #:toplevel #:*ui*))
