(in-package #:cl-user)

(defpackage #:net.mfiano.lisp.patchwork
  (:local-nicknames
   (#:bin #:binpack))
  (:use #:cl)
  (:export
   #:make-atlas
   #:make-atlas-from-directory
   #:unpack-atlas))
