(in-package #:cl-user)

(defpackage #:mfiano.cmd.freebsd.clmem
  (:local-nicknames
   (#:lib #:mfiano.cmd.freebsd.lib)
   (#:bsd #:mfiano.ffi.freebsd)
   (#:c #:cffi)
   (#:u #:mfiano.misc.utils)
   (#:ui #:adopt))
  (:use #:cl)
  (:export #:app #:*ui*))
